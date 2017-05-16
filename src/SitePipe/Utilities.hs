module SitePipe.Utilities
  ( module X
  , simpleURL
  , addPrefix
  , setExt
  ) where

import System.FilePath.Posix as X
import Data.Aeson.Types
import Data.Aeson.Lens
import Control.Lens
import Data.Text.Lens

simpleURL :: (ToJSON a) => a -> String
simpleURL = takeBaseName . view (key "filepath" . _String . unpacked) . toJSON

setExt :: String -> String -> String
setExt = flip replaceExtension

addPrefix :: String -> String -> String
addPrefix = (++)
