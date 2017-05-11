{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
module SitePipe.Files
  ( resourceGlob
  , loadTemplate
  , simpleResource
  ) where

import Control.Monad.Catch
import Data.Foldable
import SitePipe.Pipes
import SitePipe.Types
import qualified System.FilePath.Glob as G
import Data.Aeson
import Text.Mustache
import System.Directory
import System.FilePath.Posix

resourceGlob :: (FromJSON resource) => Pipe resource -> String -> IO [resource]
resourceGlob pipe pattern = do
  filenames <- G.glob pattern
  traverse (loadResource pipe) filenames

loadTemplate :: String -> IO Template
loadTemplate filePath = do
  mTemplate <- localAutomaticCompile filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

simpleResource :: Pipe Value -> Pattern -> IO ()
simpleResource pipe@(Pipe{..}) pattern = do
  resources <- resourceGlob pipe pattern
  createDirectoryIfMissing False "./dist"
  withCurrentDirectory "./dist"
    $ traverse_ (writeResource pipe) resources

writeResource :: Pipe a -> a -> IO ()
writeResource (Pipe{..}) obj = do
  renderedContent <- resourceWriter obj
  url <- computeURL obj
  createDirectoryIfMissing True $ takeDirectory url
  writeFile url renderedContent
