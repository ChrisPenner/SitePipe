{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
module SitePipe.Files
  ( loadTemplate
  , resourceLoader
  , resourceLoader'
  , resourceWriter
  , templateWriter
  , textWriter
  , copyFiles
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

srcGlob :: String -> SiteM [String]
srcGlob pattern@('/':_) = throwM $ SitePipeError ("glob pattern " ++ pattern ++ " must be a relative path")
srcGlob pattern = do
  srcD <- asks srcDir
  liftIO $ G.glob (srcD </> pattern)

resourceLoader' :: (FromJSON resource) => (String -> IO String) -> (Value -> String) -> String -> SiteM [resource]
resourceLoader' rReader makeUrl pattern = do
  filenames <- srcGlob pattern
  traverse (loadResource rReader makeUrl) filenames

resourceLoader :: (String -> IO String) -> (Value -> String) -> String -> SiteM [Value]
resourceLoader = resourceLoader'

loadTemplate :: String -> SiteM Template
loadTemplate filePath = do
  srcD <- asks srcDir
  mTemplate <- liftIO $ automaticCompile [srcD] filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

resourceWriter :: (ToJSON a) => (a -> IO String) -> [a] -> SiteM ()
resourceWriter resourceRenderer resources =
  traverse_ (writeResource resourceRenderer) resources

writeResource :: (ToJSON a) => (a -> IO String) -> a -> SiteM ()
writeResource renderer obj = do
  outD <- asks outputDir
  renderedContent <- liftIO $ renderer obj
  let outFile = outD </> getURL (toJSON obj)
  liftIO . createDirectoryIfMissing True $ takeDirectory outFile
  liftIO . putStrLn $ "Writing " ++ outFile
  liftIO $ writeFile outFile renderedContent

templateWriter :: (ToJSON a) => FilePath -> [a] -> SiteM ()
templateWriter templatePath resources = do
  template <- loadTemplate templatePath
  resourceWriter (renderTemplate template) resources

textWriter :: (ToJSON a) => [a] -> SiteM ()
textWriter resources = do
  resourceWriter (return . getContent . toJSON) resources

copyFiles :: (String -> String) -> String -> SiteM ()
copyFiles transformPath pattern = do
  Settings{..} <- ask
  srcFilenames <- srcGlob pattern
  let destFilenames = (outputDir </>) . transformPath . makeRelative srcDir <$> srcFilenames
  liftIO $ traverse_ (createDirectoryIfMissing True . takeDirectory) destFilenames
  liftIO $ traverse_ copy (zip srcFilenames destFilenames)
    where
      copy (src, dest) = do
        putStrLn $ "Copying " ++ src ++ " to " ++ dest
        copyFile src dest
