{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language ViewPatterns #-}
module SitePipe.Templating
  ( renderTemplate
  ) where

import qualified Text.Mustache as M
import qualified Data.Text as T
import Data.Text.Lens
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.Types
import SitePipe.Types
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Text.Mustache.Types as MT

-- | Given a template, produces a function compatible with 'SitePipe.Files.writeWith'
-- which writes resources using the template.
renderTemplate :: (ToJSON env) => M.Template -> env -> SiteM String
renderTemplate template (toJSON -> env) = do
  gContext <- asks globalContext
  let fullContext = addContext gContext (MT.toMustache env)
  case M.checkedSubstitute template fullContext of
    ([], result) -> return (T.unpack result)
    (errs, r) -> do
      tell $ ["*** Warnings rendering " ++ path ++ "***"] ++ (fmap show errs) ++ ["------"]
      return (T.unpack r)
  where
    path = env ^. key "filepath" . _String . unpacked
    addContext (MT.Object context) (MT.Object e) = MT.Object (context <> e)
    addContext _ e = e
