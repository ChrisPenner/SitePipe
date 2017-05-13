{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
module SitePipe.Files
  ( loadTemplate
  , resourceLoader
  , resourceWriter
  , templateWriter
  ) where

import Control.Monad.Catch
import Data.Foldable
import SitePipe.Pipes
import SitePipe.Templating
import SitePipe.Types
import qualified System.FilePath.Glob as G
import Data.Aeson
import Text.Mustache
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader

resourceLoader :: (FromJSON resource) => (String -> IO String) -> String -> SiteM [resource]
resourceLoader rReader pattern = do
  filenames <- liftIO $ G.glob pattern
  traverse (loadResource rReader) filenames

loadTemplate :: String -> SiteM Template
loadTemplate filePath = do
  mTemplate <- liftIO $ localAutomaticCompile filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

resourceWriter :: (ToJSON a) => (a -> IO String) -> (a -> IO String) -> [a] -> SiteM ()
resourceWriter resourceRenderer makeFilepath resources = do
  outD <- asks outputDir
  liftIO $ do
    exists <- doesDirectoryExist outD
    when exists (removeDirectoryRecursive outD)
    createDirectoryIfMissing False outD
    cwd <- getCurrentDirectory
    setCurrentDirectory outD
    traverse_ (writeResource resourceRenderer makeFilepath) resources
    setCurrentDirectory cwd

writeResource :: (a -> IO String) -> (a -> IO String) -> a -> IO ()
writeResource renderer makeFilepath obj = do
  renderedContent <- renderer obj
  url <- makeFilepath obj
  createDirectoryIfMissing True $ takeDirectory url
  putStrLn $ "Writing " ++ url
  writeFile url renderedContent

templateWriter :: (ToJSON a) => FilePath -> (a -> IO String) -> [a] -> SiteM ()
templateWriter templatePath makeFilepath resources = do
  template <- loadTemplate templatePath
  resourceWriter (renderTemplate template) makeFilepath resources
