{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Status
import Network.Wai.Parse
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Read (readMaybe)
import Web.Scotty
import Data.List
import Data.Ord
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Cabinet as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.UUID as UUID
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)
import Resources (loadResource)

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
serve pool = getPort >>= \port -> scotty port $ do
  get "/static/styles.css" $ do
    styles <- liftAndCatchIO styleSheet
    setHeader "Content-Type" "text/css"
      >> raw (BL.fromStrict styles)

  get "/" $ idx >>= renderPage . buildIndex Nothing

  get "/uploaded/status/:status" $ do
    i <- idx
    captureParam "status" >>= \case
      UploadOk -> renderPage $ buildIndex (Just "Upload completed successfully.") i
      NoFiles -> renderPage $ buildIndex (Just "No files were uploaded.") i
      UploadEmpty -> renderPage $ buildIndex (Just "Can't upload an empty file.") i

  get "/files/by-uuid/:uuid" getByUUID
  get "/files/by-uuid/:uuid/:fname" getByUUID

  post "/files/upload" $ files >>= upload >>= redirect . L.append "/uploaded/status/" . L.pack . show
  where
    getByUUID :: ActionM ()
    getByUUID = captureParam "uuid"
      >>= liftAndCatchIO . C.poolLookup pool . unwrapUUID
      >>= \case
        Just (C.IndexEntry _ content_type _ _, content) ->
          setHeader "Content-Type" (L.fromStrict $ E.decodeUtf8 content_type)
            >> raw (BL.fromStrict content)
        Nothing -> status notFound404

    upload [] = return NoFiles
    upload fs = mapM_ uploadSingle fs >> return UploadOk

    idx = liftAndCatchIO $ C.poolIndex pool

    uploadSingle (_, f) | BL.null (fileContent f) = redirect $ L.append "/uploaded/status/" $ L.pack $ show UploadEmpty
    uploadSingle (_, f) = liftAndCatchIO $ void $ scottyFileToCabinetFile f >>= C.addToPool pool

    scottyFileToCabinetFile :: FileInfo BL.ByteString -> IO C.FileBuf
    scottyFileToCabinetFile f =
      C.buildFile
        (E.decodeUtf8 $ fileName f)
        (fileContentType f)
        False
        (B.toStrict $ fileContent f)

renderPage :: H.Html -> ActionM ()
renderPage = html . renderHtml

layout :: T.Text -> H.Html -> H.Html
layout title inner = H.docTypeHtml $ do
  H.head $ do
    H.link H.! A.href "/static/styles.css" H.! A.rel "stylesheet"
    -- <meta name="viewport" content="width=device-width, initial-scale=1" />
    H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
    H.title $ H.toHtml title

  H.body $ H.main inner

-- Build the markup for the index page. This contains a form to upload files and a view of available
-- files to download.
--
-- TODO: This should also display instance stats about store and GC.
buildIndex :: Maybe T.Text -> [C.IndexEntry] -> H.Html
buildIndex uploadStatus idx = layout "Cabinet" $ statusView uploadStatus >> uploadFormView >> indexView
  where
    statusView Nothing = mempty
    statusView (Just msg) = H.div H.! A.class_ "status" $ H.toHtml msg

    uploadFormView =
      H.form H.! A.action "/files/upload" H.! A.method "post" H.! A.enctype "multipart/form-data" $
        fileUpload >> submit

    indexView = H.div $ mapM_ entryView sortedIdx

    sortedIdx = sortOn (Down . C.idxTime) idx

    fileUpload = H.input H.! A.type_ "file" H.! A.multiple mempty H.! A.name "files" H.! A.required ""
    submit = H.input H.! A.type_ "submit" H.! A.value "Upload documents."

    entryView :: C.IndexEntry -> H.Html
    entryView (C.IndexEntry title _ uuid time) =
        H.a H.! A.href (Blaze.stringValue $ "/files/by-uuid/" ++ show uuid ++ "/" ++ (urlEncode . T.unpack) title) $ do
            H.div H.! A.class_ "left" $ H.toHtml title
            H.div H.! A.class_ "right" $ H.toHtml $ show time
