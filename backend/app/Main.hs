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
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status
import Network.Wai.Parse
import Resources (loadResource)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

styleSheet :: IO B.ByteString
styleSheet = loadResource "styles.css"

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
getPort = fmap (fromMaybe 3000 . (>>= readMaybe)) (lookupEnv "CABINET_PORT")

serve :: C.FilePool -> IO ()
serve pool =
  getPort >>= \port -> scotty port $ do
    get "/index" $ idx >>= json . buildIndex
    get "/metadata" $ poolMetadata >>= json . scrapeMetadata

    get "/files/by-uuid/:uuid" getByUUID
    get "/files/by-uuid/:uuid/:fname" getByUUID

    post "/set-attrs/by-uuid/:uuid" $ do
      public <- formCheckBoxValue "public"
      uuid <- captureParam "uuid"
      liftIO $ C.setPublic pool (unwrapUUID uuid) public
      redirect "/"

    post "/files/upload" $
      files
        >>= upload
        -- This is moderately evil, but we never expect /?status=whatever to be
        -- handled by this server; instead we're redirecting to the frontend.
        >>= redirect . L.append "/?status=" . L.pack . show
  where
    formCheckBoxValue :: L.Text -> ActionM Bool
    formCheckBoxValue name = formParseCheckBox <$> formParamMaybe name

    formParseCheckBox :: Maybe L.Text -> Bool
    formParseCheckBox (Just "on") = True
    formParseCheckBox _ = False

    getByUUID :: ActionM ()
    getByUUID =
      captureParam "uuid"
        >>= liftIO . C.poolLookup pool . unwrapUUID
        >>= \case
          Just (C.IndexEntry _ content_type _ _, content) ->
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
        (fileContentType f)
        False
        (B.toStrict $ fileContent f)

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
          "creation_date" .= C.i_creation ie
        ]

-- Get a JSON document with an index of available files in the cabinet.
scrapeMetadata :: C.Metadata -> A.Value
scrapeMetadata md =
  A.object
    [ "in_use" .= C.m_in_use md,
      "in_use_at_last_gc" .= C.m_in_use_at_last_gc md,
      "gc_interval" .= C.m_gc_interval md,
      "gc_prop" .= C.m_gc_prop md
    ]
