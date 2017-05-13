{-# language DeriveGeneric #-}
module Main where

import SitePipe
import GHC.Generics

data Post = Post
  { tags :: [String]
  , author :: String
  , date :: String
  , title :: String
  , content :: String
  , filepath :: String
  , prevPost :: Maybe String
  , nextPost :: Maybe String
  , image :: Maybe String
  } deriving (Show, Generic)

instance ToJSON Post
instance FromJSON Post

main :: IO ()
main = site basicSettings $ do
  posts <- resourceLoader markdownReader "temp/posts/*.md"
  templateWriter "temp/templates/post.html" postUrls (posts :: [Post])
    where
      postUrls = fmap ("posts/" ++) <$> simpleURL
