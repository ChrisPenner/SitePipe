module SitePipe.Templating
  -- ()
  where

import Text.Mustache (substitute, compileTemplate)
import Data.Yaml (Object)
import qualified Data.Text as T
import Control.Monad.Catch

import SitePipe.Error

type Template = String

render :: MonadThrow m => String -> Template -> Object -> m String
render name templateStr obj = do
  template <- compiled
  return . T.unpack $ substitute template obj
  where
    compiled =
      case compileTemplate name (T.pack templateStr) of
        Left err -> throwM (PParseErr err)
        Right template -> return template
