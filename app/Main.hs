{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
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
  posts <- resourceLoader markdownReader "posts/*.md"
  liftIO . print $ toJSON (head posts :: Post)
  templateWriter "templates/base.html" postUrls posts
  copyFiles id "css/*.css"
  css <- resourceLoader textReader "css/*.css"
  textWriter cssUrls (css :: [Value])
    where
      postUrls = setExt "html" . addPrefix "posts/" . simpleURL
      cssUrls = setExt "css" . addPrefix "css/" . simpleURL
