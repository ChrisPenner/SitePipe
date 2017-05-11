module Main where

import SitePipe

main :: IO ()
main = site $ do
  results <- simpleResource "temp/posts/**/*.md" "temp/templates/post.html" "dist"
  print results
