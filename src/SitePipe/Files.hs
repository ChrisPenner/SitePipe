{-# language RankNTypes #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
module SitePipe.Files
  ( loadTemplate
  , resourceLoader
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
srcGlob pattern = do
  srcD <- asks srcDir
  liftIO $ G.glob (srcD </> pattern)

resourceLoader :: (FromJSON resource) => (String -> IO String) -> String -> SiteM [resource]
resourceLoader rReader pattern = do
  filenames <- srcGlob pattern
  traverse (loadResource rReader) filenames

loadTemplate :: String -> SiteM Template
loadTemplate filePath = do
  mTemplate <- liftIO $ localAutomaticCompile filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

resourceWriter :: (ToJSON a) => (a -> IO String) -> (a -> String) -> [a] -> SiteM ()
resourceWriter resourceRenderer makeFilepath resources = do
  outD <- asks outputDir
  liftIO $ do
    cwd <- getCurrentDirectory
    setCurrentDirectory outD
    traverse_ (writeResource resourceRenderer makeFilepath) resources
    setCurrentDirectory cwd

writeResource :: (a -> IO String) -> (a -> String) -> a -> IO ()
writeResource renderer makeFilepath obj = do
  renderedContent <- renderer obj
  let url = makeFilepath obj
  createDirectoryIfMissing True $ takeDirectory url
  putStrLn $ "Writing " ++ url
  writeFile url renderedContent

templateWriter :: (ToJSON a) => FilePath -> (a -> String) -> [a] -> SiteM ()
templateWriter templatePath makeFilepath resources = do
  template <- loadTemplate templatePath
  resourceWriter (renderTemplate template) makeFilepath resources

textWriter :: (ToJSON a) => (a -> String) -> [a] -> SiteM ()
textWriter makeFilepath resources = do
  resourceWriter (return . getContent . toJSON) makeFilepath resources

copyFiles :: (String -> String) -> String -> SiteM ()
copyFiles _ pat@('/':_) = throwM $ SitePipeError ("glob pattern " ++ pat ++ " must be a relative path")
copyFiles transformPath pattern = do
  Settings{..} <- ask
  srcFilenames <- srcGlob (srcDir </> pattern)
  let destFilenames = (outputDir </>) . transformPath . makeRelative srcDir <$> srcFilenames
  liftIO $ traverse_ (uncurry copyFile) (zip srcFilenames destFilenames)
