{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import qualified Text.Mustache as MT
import qualified Text.Mustache.Types as MT
import qualified Data.Text as T

main :: IO ()
main = siteWithGlobals templateFuncs $ do
  -- Load all the posts from site/posts/
  posts <- resourceLoader markdownReader ["posts/*.md"]
  -- getTags will return a list of all tags from the posts,
  -- each tag has a 'tag' and a 'posts' property
  let tags = getTags makeTagUrl posts
      -- Create an object with the needed context for a table of contents
      indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "tags" .= tags
                            , "url" .= ("/index.html" :: String)
                            ]
      rssContext :: Value
      rssContext = object [ "posts" .= posts
                          , "domain" .= ("http://chrispenner.ca" :: String)
                          , "url" .= ("/rss.xml" :: String)
                          ]

  -- Render index page, posts and tags respectively
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
  writeTemplate "templates/tag.html" tags
  writeTemplate "templates/rss.xml" [rssContext]
  staticAssets

-- We can provide a list of functions to be availabe in our mustache templates
templateFuncs :: MT.Value
templateFuncs = MT.object
  [ "tagUrl" MT.~> MT.overText (T.pack . makeTagUrl . T.unpack)
  ]

makeTagUrl :: String -> String
makeTagUrl tagName = "/tags/" ++ tagName ++ ".html"

-- | All the static assets can just be copied over from our site's source
staticAssets :: SiteM ()
staticAssets = copyFiles
    -- We can copy a glob
    [ "css/*.css"
    -- Or just copy the whole folder!
    , "js/"
    , "images/"
    ]
