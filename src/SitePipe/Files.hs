{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
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
import SitePipe.Templating
import SitePipe.Types
import qualified System.FilePath.Glob as G
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens
import Text.Mustache
import System.Directory
import System.FilePath.Posix
import Control.Monad.Reader
import qualified Data.Text as T
import SitePipe.Parse

srcGlob :: String -> SiteM [String]
srcGlob pattern@('/':_) = throwM $ SitePipeError ("glob pattern " ++ pattern ++ " must be a relative path")
srcGlob pattern = do
  srcD <- asks srcDir
  liftIO $ G.glob (srcD </> pattern)

resourceLoader' :: (FromJSON resource) => (String -> IO String) -> (Value -> String) -> String -> SiteM [resource]
resourceLoader' fileReader makeUrl pattern = do
  filenames <- srcGlob pattern
  traverse (loadResource fileReader makeUrl) filenames

resourceLoader :: (String -> IO String) -> (Value -> String) -> String -> SiteM [Value]
resourceLoader = resourceLoader'

loadTemplate :: String -> SiteM Template
loadTemplate filePath = do
  srcD <- asks srcDir
  mTemplate <- liftIO $ automaticCompile [srcD] filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

resourceWriter :: (ToJSON a) => (a -> SiteM String) -> [a] -> SiteM ()
resourceWriter resourceRenderer resources =
  traverse_ (writeResource resourceRenderer) resources

writeResource :: (ToJSON a) => (a -> SiteM String) -> a -> SiteM ()
writeResource renderer obj = do
  outD <- asks outputDir
  renderedContent <- renderer obj
  let outFile = outD </> getURL (toJSON obj)
  liftIO . createDirectoryIfMissing True $ takeDirectory outFile
  liftIO . putStrLn $ "Writing " ++ outFile
  liftIO $ writeFile outFile renderedContent

writeResources :: (ToJSON a) => (a -> SiteM String) -> [a] -> SiteM ()
writeResources = traverse_ . writeResource

templateWriter :: (ToJSON a) => FilePath -> [a] -> SiteM ()
templateWriter templatePath resources = do
  template <- loadTemplate templatePath
  writeResources (renderTemplate template) resources

textWriter :: (ToJSON a) => [a] -> SiteM ()
textWriter resources =
  writeResources (return . getContent . toJSON) resources

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

loadResource :: (FromJSON a) => (String -> IO String) -> (Value -> String) -> String -> SiteM a
loadResource rReader makeUrl filepath = do
  cwd <- liftIO getCurrentDirectory
  let relPath = makeRelative cwd filepath
  file <- liftIO $ readFile filepath
  (meta, source) <- processSource filepath file
  content <- liftIO $ rReader source
  valueToResource (addMeta relPath content meta)
    where
      addMeta relPath content meta =
        meta
        & _Object . at "filepath" ?~ String (T.pack filepath)
        & _Object . at "relativePath" ?~ String (T.pack relPath)
        & _Object . at "content" ?~ String (T.pack content)
        & (\obj -> obj & _Object . at "url" ?~ String (T.pack . ("/" </>) . makeUrl $ obj))

valueToResource :: (MonadThrow m, FromJSON a) => Value -> m a
valueToResource obj =
  case parseEither parseJSON obj of
    Left err -> throwM (JSONErr name err)
    Right result -> return result
  where
    name = getFilepath obj
