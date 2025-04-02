module Resources (loadResource) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Paths_cabinet

loadResource :: String -> IO B.ByteString
loadResource path = fmap BU.fromString d
  where
    d = getDataFileName path' >>= readFile
    path' = "static/" ++ path
