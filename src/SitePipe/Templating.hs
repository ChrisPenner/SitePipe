{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
module SitePipe.Templating
  ( renderTemplate
  ) where

import qualified Text.Mustache as M
import qualified Data.Text as T
import Data.Aeson.Types
import SitePipe.Types
import Control.Monad.Writer

renderTemplate :: (ToJSON env) => M.Template -> env -> SiteM String
renderTemplate template (toJSON -> env) =
  case M.checkedSubstitute template env of
    ([], result) -> return (T.unpack result)
    (errs, r) -> do
      tell $ ["*** Warnings rendering " ++ path ++ "***"] ++ (fmap show errs) ++ ["------"]
      return (T.unpack r)
  where
    path = getFilepath env
