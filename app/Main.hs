{-# language DeriveGeneric #-}
module Main where

import Data.Aeson
import GHC.Generics

import SitePipe

data Post = Post
  { filepath :: String
  , content :: String
  , tags :: Maybe [String]
  } deriving (Show, Generic)

instance FromJSON Post
instance ToJSON Post

main :: IO ()
main = site $ do
  results <- simpleResource "*.md" "example.html"
  print results
