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
import Control.Monad.IO.Class
import System.IO

renderTemplate :: (ToJSON env, MonadThrow m, MonadIO m) => M.Template -> env -> m String
renderTemplate template (toJSON -> env) =
  -- return . T.unpack $ M.substitute template env
  case M.checkedSubstitute template env of
    ([], result) -> return (T.unpack result)
    (errs, r) -> liftIO $ do
      hPutStrLn stderr $ "*** Warnings rendering " ++ path ++ "***"
      traverse (hPrint stderr) errs
      hPutStrLn stderr "------"
      return (T.unpack r)
  where
    path = getFilepath env
