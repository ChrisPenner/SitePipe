{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module SitePipe.Templating
  ( renderTemplate
  ) where

import qualified Text.Mustache as M
import qualified Data.Text as T
import Control.Monad.Catch
import Data.Aeson.Types
import SitePipe.Types

renderTemplate :: (ToJSON env, MonadThrow m) => M.Template -> env -> m String
renderTemplate template (toJSON -> env) =
  case M.checkedSubstitute template env of
    ([], result) -> return (T.unpack result)
    (errs, _) -> throwM $ TemplateInterpolateErr path errs
  where
    path = getFilepath env
