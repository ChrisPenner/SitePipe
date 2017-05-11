{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
module SitePipe.Files
  ( resourceGlob
  , loadTemplate
  , simpleResource
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import SitePipe.Pipes
import SitePipe.Types
import SitePipe.Templating
import qualified System.FilePath.Glob as G
import Data.Aeson
import Text.Mustache
import System.Directory

resourceGlob :: (FromJSON resource, MonadIO m, MonadThrow m) => Pipe m resource -> String -> m [resource]
resourceGlob pipe pattern = do
  filenames <- liftIO $ G.glob pattern
  traverse (loadResource pipe) filenames

loadTemplate :: (MonadIO m, MonadThrow m) => String -> m Template
loadTemplate filePath = do
  mTemplate <- liftIO $ localAutomaticCompile filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

simpleResource :: (MonadIO m, MonadThrow m) => Pattern -> TemplatePath -> String -> m [Value]
simpleResource pattern templatePath outputDir = do
  template <- loadTemplate templatePath
  resources <- resourceGlob (markdownPipe template) pattern
  renderedContent <- traverse (renderTemplate template) resources
  let relPaths = getRelativeFilepath <$> resources

  liftIO . withCurrentDirectory outputDir $
    traverse (uncurry writeFile) (zip relPaths renderedContent)
  return resources
