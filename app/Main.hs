{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import qualified Data.Map as M
import Data.Text.Lens
import qualified Data.Text as T

indexURL :: String
indexURL = "/index.html"

main :: IO ()
main = site $ do
  posts <- fmap processPostTags <$> resourceLoader markdownReader mkPostUrl "posts/*.md"
  let allTags = byTags posts

  templateWriter "templates/index.html" [object ["posts" .= posts, ("tags" .= allTags), ("url" .= indexURL)]]
  templateWriter "templates/base.html" posts
  templateWriter "templates/tag.html" allTags
  staticAssets
    where
      mkPostUrl = setExt "html" . addPrefix "posts/" . simpleURL

staticAssets :: SiteM ()
staticAssets = do
  copyFiles id "css/*.css"
  copyFiles id "images/*"

processPostTags :: Value -> Value
processPostTags post = post & key "tags" . _Array . traverse %~ mkSimpleTag
  where
    mkSimpleTag (String t) = makeTag (T.unpack t, [])
    mkSimpleTag x = x

byTags :: [Value] -> [Value]
byTags postList = makeTag <$> M.toList tagMap
  where
    tagMap = M.unionsWith mappend (fmap toMap postList)
    toMap post = M.fromList (zip (post ^.. key "tags" . values . key "tag" . _String . unpacked) $ repeat [post])

makeTag :: (String, [Value]) -> Value
makeTag (tagname, posts) = object
  [ "tag" .= tagname
  , "url" .= ("/tag/" ++ tagname ++ ".html")
  , "posts" .= posts
  ]
