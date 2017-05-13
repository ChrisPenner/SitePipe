{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module SitePipe.Pipes
  ( loadResource
  , site
  , Pipe(..)
  , getContent
  , getFilepath
  )
    where


import Control.Monad.Catch as Catch
import Data.Aeson.Lens
import Data.Aeson.Types hiding (Parser, parse)
import Control.Lens
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import Control.Monad.Reader

import SitePipe.Parse
import SitePipe.Types

-- markdownPipe :: (FromJSON a, ToJSON a) => M.Template -> Pipe a
-- markdownPipe template = Pipe
--   { resourceReader=pandocReader readMarkdown
--   , resourceRenderer=renderTemplate template
--   , transformResource=id
--   , computeURL=simpleURL
--   }

site :: Settings -> SiteM () -> IO ()
site settings' spec = do
  settings <- adjSettings settings'
  clean settings
  result <- runReaderT (Catch.try spec) settings :: IO (Either SitePipeError ())
  case result of
    Left err -> print err
    Right _ -> return ()

adjSettings :: Settings -> IO Settings
adjSettings Settings{..} = do
  outD <- makeAbsolute outputDir
  srcD <- makeAbsolute srcDir
  return Settings{outputDir=outD, srcDir=srcD, ..}

clean :: Settings -> IO ()
clean (outputDir -> outD) = do
  exists <- doesDirectoryExist outD
  when exists (removeDirectoryRecursive outD)
  createDirectoryIfMissing False outD

valueToResource :: (MonadThrow m, FromJSON a) => Value -> m a
valueToResource obj =
  case parseEither parseJSON obj of
    Left err -> throwM (JSONErr name err)
    Right result -> return result
  where 
    name = getFilepath obj

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
