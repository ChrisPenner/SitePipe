{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import Data.List
import qualified Data.Map as M
import Data.Maybe
import GHC.Generics

data Post = Post
  { url :: String
  , tags :: Maybe [String]
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
  , tags :: [Tag]
  , url :: String
  } deriving (Show, Generic)

data Tag = Tag
  { tag :: String
  , url :: String
  , posts :: [Post]
  } deriving (Show, Generic)

instance ToJSON Post
instance FromJSON Post

instance ToJSON Index
instance FromJSON Index

instance ToJSON Tag
instance FromJSON Tag

main :: IO ()
main = site basicSettings $ do
  posts' <- resourceLoader' markdownReader mkPostUrl "posts/*.md"
  let allTags = byTags posts'
  templateWriter "templates/index.html" [Index posts' allTags "/index.html"]
  templateWriter "templates/base.html" posts'
  templateWriter "templates/tag.html" allTags
  staticAssets
    where
      mkPostUrl = setExt "html" . addPrefix "posts/" . simpleURL

staticAssets :: SiteM ()
staticAssets = do
  copyFiles id "css/*.css"
  copyFiles id "images/*"

getTags :: [Post] -> [String]
getTags = nub . concat . mapMaybe (tags :: Post -> Maybe [String])

byTags :: [Post] -> [Tag]
byTags postList = makeTag <$> M.toList tagMap
  where
    tagMap = M.unionsWith mappend (fmap toMap postList)
    toMap post@Post{tags=Just ts} = M.fromList (zip ts $ repeat [post])
    toMap _ = M.empty
    makeTag (tagname, psts) = 
      Tag { tag=tagname
          , url="/tag/" ++ tagname ++ ".html"
          , posts=psts
          }
