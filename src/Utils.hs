module Utils (urlToFilename) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.MD5 (md5)
import System.FilePath (addExtension, (</>))
import Text.HTML.Scalpel (URL)

urlToFilename :: URL -> FilePath -> String
urlToFilename url dir = dir </> addExtension md5hash "html"
  where
    md5hash :: String
    md5hash = show $ md5 $ fromString url
