{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import qualified Data.Map as M
import Data.Text.Lens
import qualified Data.Text as T
import qualified Text.Mustache as MT
import qualified Text.Mustache.Types as MT

main :: IO ()
main = siteWithGlobals funcs $ do
  posts <- fmap processPostTags <$> resourceLoader markdownReader ["**/*.md"]
  let tags = byTags posts
  writeTemplate "templates/index.html" [mkIndexEnv posts tags]
  writeTemplate "templates/base.html" (over (key "tags" . _Array . traverse) stripHTMLSuffix <$> posts)
  writeTemplate "templates/tag.html" (stripPostsHTMLSuffix <$> tags)
  staticAssets

funcs :: MT.Value
funcs = MT.object
  ["truncate" MT.~> MT.overText (T.take 30)
  ]

stripHTMLSuffix :: Value -> Value
stripHTMLSuffix obj = obj
  & key "url" . _String . unpacked %~ setExt ""

stripPostsHTMLSuffix :: Value -> Value
stripPostsHTMLSuffix tag = tag
  & key "posts" . _Array . traversed . key "url" . _String . unpacked %~ setExt ""

mkIndexEnv :: [Value] -> [Value] -> Value
mkIndexEnv posts tags =
  object [ "posts" .= (stripHTMLSuffix <$> posts)
         , "tags" .= (stripHTMLSuffix <$> tags)
         , "url" .= ("/index.html" :: String)
         ]

staticAssets :: SiteM ()
staticAssets = copyFiles
    [ "css/*.css"
    , "js/"
    , "images/"
    ]

processPostTags :: Value -> Value
processPostTags post = post & key "tags" . _Array . traverse %~ mkSimpleTag
  where
    mkSimpleTag (String t) = makeTag (T.unpack t, [])
    mkSimpleTag x = x

byTags :: [Value] -> [Value]
byTags postList = makeTag <$> M.toList tagMap
  where
    tagMap = M.unionsWith mappend (fmap toMap postList)
    toMap post = M.fromList (zip (post ^.. key "tags" . values . key "tag" . _String . unpacked) $ repeat [post])

makeTag :: (String, [Value]) -> Value
makeTag (tagname, posts) = object
  [ "tag" .= tagname
  , "url" .= ("/tag/" ++ tagname ++ ".html")
  , "posts" .= posts
  ]
