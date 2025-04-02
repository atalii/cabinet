module Resources (loadResource) where

import qualified Data.ByteString as B
import System.IO (withFile, IOMode (..))
import Paths_cabinet

loadResource :: String -> IO B.ByteString
loadResource path = path' >>= slurp
  where
    path' = getDataFileName $ "static/" ++ path
    slurp p = withFile p ReadMode B.hGetContents
