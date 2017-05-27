{-# language OverloadedStrings #-}
module Main where

import SitePipe

main :: IO ()
main = site $ do
  -- Load all the posts from site/posts/
  posts <- resourceLoader markdownReader ["posts/*.md"]

  let indexContext :: Value
      indexContext = object [ "posts" .= posts
                            , "url" .= ("/index.html" :: String)
                            ]

  -- Render index page and posts
  writeTemplate "templates/index.html" [indexContext]
  writeTemplate "templates/post.html" posts
