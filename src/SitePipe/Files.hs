{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
module SitePipe.Files
  ( fromFile
  , glob
  , resourceGlob
  , loadTemplate
  , simpleResource
  ) where

import Text.Pandoc (readMarkdown)
import Control.Monad.Catch
import Control.Monad.IO.Class
import SitePipe.Pipes
import SitePipe.Error
import SitePipe.Templating
import qualified System.FilePath.Glob as G
import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import Data.Aeson
import Text.Mustache
import qualified Data.Text as T

type TemplatePath = String
type Pattern = String

fromFile :: (MonadIO m, MonadThrow m) => PandocReader -> String -> m Value
fromFile reader path = do
  source <- liftIO $ readFile path
  (_Object . at "filepath" ?~ String (T.pack path)) <$> parseResource reader path source

glob :: (MonadIO m, MonadThrow m) => PandocReader -> Pattern -> m [Value]
glob reader pat = liftIO (G.glob pat) >>= traverse (fromFile reader)

resourceGlob :: (FromJSON resource, MonadIO m, MonadThrow m) => PandocReader -> String -> m [resource]
resourceGlob reader = glob reader >=> toResources

loadTemplate :: (MonadIO m, MonadThrow m) => String -> m Template
loadTemplate filePath = do
  mTemplate <- liftIO $ localAutomaticCompile filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

simpleResource :: (MonadIO m, MonadThrow m) => Pattern -> TemplatePath -> m [String]
simpleResource pattern templatePath = do
  template <- loadTemplate templatePath
  resources <- resourceGlob readMarkdown pattern
  traverse (renderTemplate template) (resources :: [Value])
