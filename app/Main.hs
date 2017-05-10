module Main where

import SitePipe

main :: IO ()
main = site $ do
  results <- simpleResource "*.md" "example.html"
  print results
