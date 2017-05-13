module SitePipe.Utilities
  ( module X
  , simpleURL
  , addPrefix
  , setExt
  ) where

import System.FilePath.Posix as X
import Data.Aeson.Types
import SitePipe.Types

simpleURL :: (ToJSON a) => a -> String
simpleURL = takeBaseName . getFilepath . toJSON

setExt :: String -> String -> String
setExt = flip replaceExtension

addPrefix :: String -> String -> String
addPrefix = (++)
