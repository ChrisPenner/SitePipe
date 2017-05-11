module Main where

import SitePipe

main :: IO ()
main = site $ do
  template <- loadTemplate "temp/templates/post.html"
  results <- simpleResource (markdownPipe template) "temp/posts/*.md"
  print results
