{-# language OverloadedStrings #-}
module SitePipe.Utilities
  ( addPrefix
  , setExt
  , getTags
  ) where

import System.FilePath.Posix
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Lens hiding ((.=))

-- | Set the extension of a filepath or url to the given extension.
-- Use @setExt ""@ to remove any extension.
setExt :: String -> FilePath -> FilePath
setExt = flip replaceExtension

-- | Add a prefix to a filepath or url
addPrefix :: String -> FilePath -> FilePath
addPrefix = (++)

-- | Given a function which creates a url from a tag name and a list of posts
-- (which have a tags property which is a list of strings)
-- this returns a list of tags which contain:
--
-- * name: The tag name
-- * url: The tag's url
-- * posts: The list of posts matching that tag
getTags :: (String -> String) -- ^ Accept a tagname and create a url
           -> [Value] -- ^ List of posts
           -> [Value]
getTags makeUrl postList = uncurry (makeTag makeUrl) <$> M.toList tagMap
  where
    tagMap = M.unionsWith mappend (toMap <$> postList)
    toMap post = M.fromList (zip (post ^.. key "tags" . values . _String . to T.unpack) $ repeat [post])

-- | Makes a single tag
makeTag :: (String -> String) -> String -> [Value] -> Value
makeTag makeUrl tagname posts = object
  [ "tag" .= tagname
  , "url" .= makeUrl tagname
  , "posts" .= posts
  ]
