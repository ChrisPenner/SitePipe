{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
module Main where

import SitePipe
import Data.Text.Lens
import qualified Data.Text as T
import qualified Text.Mustache as MT
import qualified Text.Mustache.Types as MT

main :: IO ()
main = siteWithGlobals funcs $ do
  posts <- resourceLoader markdownReader ["posts/*.md"]
  let tags = getTags id posts
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
