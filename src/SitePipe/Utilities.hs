{-# language OverloadedStrings #-}
module SitePipe.Utilities
  ( addPrefix
  , setExt
  ) where

import System.FilePath.Posix

-- | Set the extension of a filepath or url to the given extension.
-- Use @setExt ""@ to remove any extension.
setExt :: String -> FilePath -> FilePath
setExt = flip replaceExtension

-- | Add a prefix to a filepath or url
addPrefix :: String -> FilePath -> FilePath
addPrefix = (++)
