{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- TODO:
-- + basic stylesheet via static server middleware (tomorrow)
-- + refactoring, general cleaning of web stuff (tomorrow)
-- + deploy (wednesday)

import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Cabinet as C
import Data.FileEmbed (embedFileRelative)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.UUID as UUID
import Network.HTTP.Types.Status
import Network.Wai.Parse
import qualified Text.Blaze as Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

styleSheet :: B.ByteString
styleSheet = $(embedFileRelative "static/styles.css")

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

serve :: C.FilePool -> IO ()
serve pool = scotty 3000 $ do
  get "/static/styles.css" $
    setHeader "Content-Type" "text/css"
      >> raw (BL.fromStrict styleSheet)

  get "/" $ idx >>= renderPage . buildIndex Nothing

  get "/uploaded/status/:status" $ do
    i <- idx
    pathParam "status" >>= \case
      UploadOk -> renderPage $ buildIndex (Just "Upload completed successfully.") i
      NoFiles -> renderPage $ buildIndex (Just "No files were uploaded.") i
      UploadEmpty -> renderPage $ buildIndex (Just "Can't upload an empty file.") i

  get "/files/by-uuid/:uuid" $
    pathParam "uuid"
      >>= liftIO . C.poolLookup pool . unwrapUUID
      >>= \case
        Just (C.IndexEntry _ content_type _ _, content) ->
          setHeader "Content-Type" (L.fromStrict $ E.decodeUtf8 content_type)
            >> raw (BL.fromStrict content)
        Nothing -> status notFound404

  post "/files/upload" $ files >>= upload >>= redirect . L.append "/uploaded/status/" . L.show
  where
    upload [] = return NoFiles
    upload fs = mapM_ uploadSingle fs >> return UploadOk

    idx = liftIO $ C.poolIndex pool

    uploadSingle (_, f) | BL.null (fileContent f) = redirect $ L.append "/uploaded/status/" $ L.show UploadEmpty
    uploadSingle (_, f) = liftIO $ void $ scottyFileToCabinetFile f >>= C.addToPool pool

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
    statusView (Just msg) = H.div $ H.toHtml msg

    uploadFormView =
      H.form H.! A.action "/files/upload" H.! A.method "post" H.! A.enctype "multipart/form-data" $
        fileUpload >> submit

    indexView = H.ul $ mapM_ entryView idx

    fileUpload = H.input H.! A.type_ "file" H.! A.multiple mempty H.! A.name "files" H.! A.required ""
    submit = H.input H.! A.type_ "submit" H.! A.value "Upload documents."

    entryView :: C.IndexEntry -> H.Html
    entryView (C.IndexEntry title _ uuid _) =
      H.li $
        H.a H.! A.href (Blaze.stringValue $ "/files/by-uuid/" ++ show uuid) $
          H.toHtml title
