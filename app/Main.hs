{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import GHC.Generics

data Post = Post
  { url :: String
  , tags :: Maybe [String]
  , categories :: Maybe [String]
  , author :: String
  , date :: String
  , title :: String
  , content :: String
  , filepath :: String
  , prevPost :: Maybe String
  , nextPost :: Maybe String
  , image :: Maybe String
  } deriving (Show, Generic)

data Index = Index
  { posts :: [Post]
  , url :: String
  } deriving (Show, Generic)

instance ToJSON Post
instance FromJSON Post

instance ToJSON Index
instance FromJSON Index

main :: IO ()
main = site basicSettings $ do
  posts' <- resourceLoader' markdownReader mkPostUrl "posts/*.md"
  templateWriter "templates/index.html" [Index posts' "/index.html"]
  templateWriter "templates/base.html" posts'
  staticAssets
    where
      mkPostUrl = setExt "html" . addPrefix "posts/" . simpleURL

staticAssets :: SiteM ()
staticAssets = do
  copyFiles id "css/*.css"
  copyFiles id "images/*"
