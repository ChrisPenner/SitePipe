{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = site $ do
  -- Load all the posts from site/posts/
  posts' <- resourceLoader markdownReader ["posts/*.md"]
  let tags = getTags makeTagUrl posts'
      posts = addTagDataToPosts tags posts'
  -- Create an object with the context for the table of contents
      indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "tags" .= tags
                            , "url" .= ("/index.html" :: String)
                            ]
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/base.html" posts
  writeTemplate "templates/tag.html" tags
  staticAssets

addTagDataToPosts :: [Value] -> [Value] -> [Value]
addTagDataToPosts tags posts = addTagData <$> posts
  where
    tagsByName = M.fromList (toKeyVal <$> tags)
    toKeyVal tag = (tag ^. key "tag" . _String, tag)
    lookupTag t = fromMaybe Null (tagsByName ^. at (t ^. _String))
    addTagData post = over (key "tags" . values) lookupTag post


makeTagUrl :: String -> String
makeTagUrl tagName = "/tags/" ++ tagName ++ ".html"

-- | All the static assets can just be copied over
staticAssets :: SiteM ()
staticAssets = copyFiles
    -- We can copy a glob
    [ "css/*.css"
    -- Or just copy the whole folder!
    , "js/"
    , "images/"
    ]
