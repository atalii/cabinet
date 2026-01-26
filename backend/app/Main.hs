{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Cabinet as C
import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status
import Network.Wai.Parse
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

data UploadResult = UploadOk | UploadEmpty | NoFiles
  deriving (Read, Show)

instance Parsable UploadResult where
  parseParam = readEither

newtype UUID = UUID UUID.UUID

unwrapUUID :: UUID -> UUID.UUID
unwrapUUID (UUID uuid) = uuid

instance Parsable UUID where
  parseParam x = UUID <$> readEither x

main :: IO ()
main = C.newPool >>= serve

getPort :: IO Int
getPort = fmap (fromMaybe 3000 . (>>= readMaybe)) (lookupEnv "CABINET_BACKEND_PORT")

serve :: C.FilePool -> IO ()
serve pool =
  getPort >>= \port -> scotty port $ do
    --
    -- These first routes provide a response interpreted by the frontend,
    -- e.g., during SSR.
    --

    get "/index" $ idx >>= json . buildIndex
    get "/metadata" $ poolMetadata >>= json . scrapeMetadata

    --
    -- These routes' responses are meant to be consumed by the browser.
    --

    get "/files/by-uuid/public/:uuid/:fname?" $ getByUUID True
    get "/files/by-uuid/:uuid/:fname?" $ getByUUID False

    --
    -- The remaining routes are used for their side effects. At the end of
    -- execution, they redirect to '/', which, in a deployment, will be routed
    -- to the frontend's index via a reverse proxy.
    --

    post "/gc/trigger" $ do
      liftIO $ C.runGc pool
      redirect "/"

    post "/set-attrs/by-uuid/:uuid" $ do
      public <- formCheckBoxValue "public"
      sticky <- formCheckBoxValue "sticky"
      uuid <- captureParam "uuid"
      liftIO $ C.setPublic pool (unwrapUUID uuid) public
      liftIO $ C.setSticky pool (unwrapUUID uuid) sticky
      redirect "/"

    post "/set-mime-type/by-uuid/:uuid" $ do
      mimeType <- formParam "type"
      uuid <- captureParam "uuid"
      liftIO $ C.setMimeType pool (unwrapUUID uuid) mimeType
      redirect "/"

    post "/files/upload" $
      files
        >>= upload
        >>= redirect . L.append "/?status=" . L.pack . show
  where
    formCheckBoxValue :: L.Text -> ActionM Bool
    formCheckBoxValue name = formParseCheckBox <$> formParamMaybe name

    formParseCheckBox :: Maybe L.Text -> Bool
    formParseCheckBox (Just "on") = True
    formParseCheckBox _ = False

    getByUUID :: Bool -> ActionM ()
    getByUUID mustBePublic =
      captureParam "uuid"
        >>= liftIO . C.poolLookup pool . unwrapUUID
        >>= \case
          Just (C.IndexEntry _ content_type _ _ public _, content) ->
            if mustBePublic && not public
              then status forbidden403
              else
                setHeader "Content-Type" (L.fromStrict $ E.decodeUtf8 content_type)
                  >> raw (BL.fromStrict content)
          Nothing -> status notFound404

    upload [] = return NoFiles
    upload fs = mapM_ uploadSingle fs >> return UploadOk

    idx = liftIO $ C.poolIndex pool
    poolMetadata = liftIO $ C.poolMetadata pool

    uploadSingle (_, f) | BL.null (fileContent f) = redirect $ L.append "/?status=" $ L.pack $ show UploadEmpty
    uploadSingle (_, f) = liftIO $ void $ scottyFileToCabinetFile f >>= C.addToPool pool

    scottyFileToCabinetFile :: FileInfo BL.ByteString -> IO C.FileBuf
    scottyFileToCabinetFile f =
      C.buildFile
        (E.decodeUtf8 $ fileName f)
        (processMime $ fileContentType f)
        False
        (B.toStrict $ fileContent f)

    -- We want to be able to specify some default parameters for insufficient
    -- Content-Types. E.g., a text files should always have an encoding specified.
    -- If this step ever becomes an annoyance, consider having it be configurable
    -- and correctly parse MIME types, rather than matching on strings.
    processMime :: B.ByteString -> B.ByteString
    processMime "text/plain" = "text/plain;charset=UTF-8"
    processMime a = a

-- Get a JSON document with an index of available files in the cabinet.
buildIndex :: [C.IndexEntry] -> A.Value
buildIndex idx = A.toJSONList $ map entryView sortedIdx
  where
    sortedIdx = sortOn (Down . C.i_creation) idx

    entryView :: C.IndexEntry -> A.Value
    entryView ie =
      A.object
        [ "name" .= C.i_name ie,
          "id" .= C.i_id ie,
          "creation_date" .= C.i_creation ie,
          "is_sticky" .= C.i_sticky ie,
          "is_public" .= C.i_public ie,
          "mime_type" .= decodeUtf8Lenient (C.i_mime_type ie)
        ]

-- Get a JSON document with an index of available files in the cabinet.
scrapeMetadata :: C.Metadata -> A.Value
scrapeMetadata md =
  A.object
    [ "in_use" .= C._m_in_use md,
      "in_use_at_last_gc" .= C._m_in_use_at_last_gc md,
      "gc_interval" .= C._m_gc_interval md,
      "gc_prop" .= C._m_gc_prop md
    ]
