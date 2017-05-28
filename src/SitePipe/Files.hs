{-# language ViewPatterns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
module SitePipe.Files
  (
  -- * Loaders
  resourceLoader

  -- * Writers
  , writeWith
  , writeTemplate
  , textWriter

  -- * Loader/Writers
  , copyFiles
  , copyFilesWith
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

-- | Given a filepath globbing pattern relative to your sources root
-- this returns a list of absolute filepaths of matching files.
-- Standard globbing rules apply.
--
-- * "posts/*.md": matches any markdown files in the posts directory of your source folder.
-- * "**/*.txt": matches all text files recursively in your source folder.
srcGlob :: GlobPattern -> SiteM [FilePath]
srcGlob pattern@('/':_) = throwM $ SitePipeError ("glob pattern " ++ pattern ++ " must be a relative path")
srcGlob pattern = do
  srcD <- asks srcDir
  liftIO $ G.glob (srcD </> pattern)

-- | Loads a Mustache template given a relative filepath.
loadTemplate :: FilePath -> SiteM Template
loadTemplate filePath = do
  srcD <- asks srcDir
  mTemplate <- liftIO $ automaticCompile [srcD] filePath
  case mTemplate of
    Left err -> throwM $ TemplateParseErr err
    Right template -> return template

-- | Given a path to a mustache template file (relative to your source directory);
-- this writes a list of resources to the output directory by applying each one to the template.
writeTemplate :: (ToJSON a)
                 => FilePath -- ^ Path to template (relative to site dir)
                 -> [a]  -- ^ List of resources to write
                 -> SiteM ()
writeTemplate templatePath resources = do
  template <- loadTemplate templatePath
  writeWith (renderTemplate template) resources

-- | Write a list of resources using the given processing function from a resource
-- to a string.
writeWith :: (ToJSON a)
          => (a -> SiteM String) -- ^ A function which renders a resource to a string.
          -> [a] -- ^ List of resources to write
          -> SiteM ()
writeWith resourceRenderer resources =
  traverse_ (writeOneWith resourceRenderer) resources

-- | Write a single resource to file using the given processing function.
writeOneWith :: (ToJSON a) => (a -> SiteM String) -> a -> SiteM ()
writeOneWith renderer obj = do
  outD <- asks outputDir
  renderedContent <- renderer obj
  let outFile = outD </> (toJSON obj ^. key "url" . _String . unpacked . to (dropWhile (== '/')))
  liftIO . createDirectoryIfMissing True $ takeDirectory outFile
  liftIO . putStrLn $ "Writing " ++ outFile
  liftIO $ writeFile outFile renderedContent

-- | Writes the content of the given resources without using a template.
textWriter :: (ToJSON a)
           => [a] -- ^ List of resources to write
           -> SiteM ()
textWriter resources =
  writeWith (return . view (key "content" . _String . unpacked) . toJSON) resources

-- | Given a list of file or directory globs (see 'srcGlob')
-- we copy matching files and directories as-is from the source directory
-- to the output directory maintaining their relative filepath.
copyFiles :: [GlobPattern] -> SiteM ()
copyFiles = copyFilesWith id

-- | Runs 'copyFiles' but using a filepath transforming function to determine
-- the output filepath. The filepath transformation accepts and should return
-- a relative path.
copyFilesWith :: (FilePath -> FilePath) -> [GlobPattern] -> SiteM ()
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

-- | Given a resource reader (see "SitePipe.Readers")
-- this function finds all files matching any of the provided list
-- of fileglobs (according to 'srcGlob') and returns a list of loaded resources
-- as Aeson 'Value's.
resourceLoader :: (String -> IO String) -- ^ A reader which processes file contents
               -> [GlobPattern] -- ^ File glob; relative to the @site@ directory
               -> SiteM [Value] -- ^ Returns a list of Aeson objects
resourceLoader = resourceLoaderGen

-- | A more generic version of 'resourceLoader' which returns any type with a
-- 'FromJSON' instance. It also handles and displays any conversion errors.
resourceLoaderGen :: (FromJSON a) => (String -> IO String) -> [GlobPattern] -> SiteM [a]
resourceLoaderGen fileReader patterns = do
  filenames <- concat <$> traverse srcGlob patterns
  traverse (loadWith fileReader) filenames

-- | loads a file from filepath and applies a given filreader.
loadWith :: (FromJSON a) => (String -> IO String) -> FilePath -> SiteM a
loadWith fileReader filepath = do
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

-- | Converts a 'Value' to a generic resource implementing 'FromJSON', handling any errors.
valueToResource :: (MonadThrow m, FromJSON a) => Value -> m a
valueToResource obj =
  case parseEither parseJSON obj of
    Left err -> throwM (JSONErr name err)
    Right result -> return result
  where
    name = obj ^. key "filepath" . _String . unpacked
