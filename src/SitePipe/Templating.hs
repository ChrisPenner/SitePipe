{-# language OverloadedStrings #-}
module SitePipe.Templating
  (renderTemplate
  ) where

import Text.Mustache
import qualified Data.Text as T
import Control.Monad.Catch
import Data.Aeson.Types

renderTemplate :: (ToJSON env, MonadThrow m) => Template -> env -> m String
renderTemplate template =
  return . T.unpack . substitute template . toJSON
