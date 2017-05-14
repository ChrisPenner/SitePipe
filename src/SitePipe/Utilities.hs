module SitePipe.Utilities
  ( module X
  , simpleURL
  , addPrefix
  , setExt
  ) where

import System.FilePath.Posix as X
import Data.Aeson.Types
import SitePipe.Types
import Data.Maybe

simpleURL :: (ToJSON a) => a -> String
simpleURL = fromMaybe "" . fmap takeBaseName . getValue "filepath"

setExt :: String -> String -> String
setExt = flip replaceExtension

addPrefix :: String -> String -> String
addPrefix = (++)
