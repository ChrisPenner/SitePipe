{-# language OverloadedStrings #-}
module SitePipe.Utilities
  ( simpleURL
  , addPrefix
  , setExt
  ) where

import System.FilePath.Posix
import Data.Aeson.Types
import Data.Aeson.Lens
import Control.Lens
import Data.Text.Lens

simpleURL :: Value -> String
simpleURL = view (key "url" . _String . unpacked)

setExt :: String -> String -> String
setExt = flip replaceExtension

addPrefix :: String -> String -> String
addPrefix = (++)
