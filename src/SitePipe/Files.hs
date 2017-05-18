{-# language ViewPatterns #-}
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
  , copyFilesWith
  , copyDirs
  , copyDirsWith
  ) where

import Data.String
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
import Data.Text.Lens
import SitePipe.Parse
import SitePipe.Utilities
import Shelly hiding ((</>), FilePath, relPath)
import Data.String.Utils
import Data.Bool

srcGlob :: String -> SiteM [String]
srcGlob pattern@('/':_) = throwM $ SitePipeError ("glob pattern " ++ pattern ++ " must be a relative path")
srcGlob pattern = do
  srcD <- asks srcDir
  liftIO $ G.glob (srcD </> pattern)

resourceLoader' :: (FromJSON resource) => (String -> IO String) -> [String] -> SiteM [resource]
resourceLoader' fileReader patterns = do
  filenames <- concat <$> traverse srcGlob patterns
  traverse (loadResource fileReader) filenames

resourceLoader :: (String -> IO String) -> [String] -> SiteM [Value]
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

templateWriter :: (ToJSON a) => FilePath -> [a] -> SiteM ()
templateWriter templatePath resources = do
  template <- loadTemplate templatePath
  writeResources (renderTemplate template) resources

writeResources :: (ToJSON a) => (a -> SiteM String) -> [a] -> SiteM ()
writeResources = traverse_ . writeResource

writeResource :: (ToJSON a) => (a -> SiteM String) -> a -> SiteM ()
writeResource renderer obj = do
  outD <- asks outputDir
  renderedContent <- renderer obj
  let outFile = outD </> (toJSON obj ^. key "url" . _String . unpacked . to (dropWhile (== '/')))
  liftIO . createDirectoryIfMissing True $ takeDirectory outFile
  liftIO . putStrLn $ "Writing " ++ outFile
  liftIO $ writeFile outFile renderedContent


textWriter :: (ToJSON a) => [a] -> SiteM ()
textWriter resources =
  writeResources (return . view (key "content" . _String . unpacked) . toJSON) resources

copyFilesWith :: (String -> String) -> [String] -> SiteM ()
copyFilesWith transformPath patterns = do
  Settings{..} <- ask
  srcFilenames <- concat <$> traverse srcGlob patterns
  let destFilenames = (outputDir </>) . transformPath . makeRelative srcDir <$> srcFilenames
  shelly $ do
    let getDir pth = bool (takeDirectory) (takeDirectory . takeDirectory) (endswith "/" pth) $ pth
    traverse_ (mkdir_p . fromString . getDir) destFilenames
    traverse_ copy (zip srcFilenames destFilenames)
    where
      copy (src, dest) = do
        echo $ T.concat ["Copying ",  T.pack src, " to ", T.pack dest]
        cp_r (fromString src) (fromString dest)

copyFiles :: [String] -> SiteM ()
copyFiles = copyFilesWith id

copyDirsWith :: (String -> String) -> [String] -> SiteM ()
copyDirsWith = copyFilesWith

copyDirs :: [String] -> SiteM ()
copyDirs = copyDirsWith id

loadResource :: (FromJSON a) => (String -> IO String) -> String -> SiteM a
loadResource fileReader filepath = do
  Settings{srcDir} <- ask
  let relPath = makeRelative srcDir filepath
  file <- liftIO $ readFile filepath
  (meta, source) <- processSource filepath file
  content <- liftIO $ fileReader source
  valueToResource (addMeta relPath content meta)
    where
      addMeta relPath content meta =
        meta
        & _Object . at "filepath" ?~ String (T.pack relPath)
        & _Object . at "content" ?~ String (T.pack content)
        & _Object . at "url" ?~ (String . T.pack . setExt "html" $ ("/" </> relPath))

valueToResource :: (MonadThrow m, FromJSON a) => Value -> m a
valueToResource obj =
  case parseEither parseJSON obj of
    Left err -> throwM (JSONErr name err)
    Right result -> return result
  where
    name = obj ^. key "filepath" . _String . unpacked
