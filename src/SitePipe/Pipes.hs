{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module SitePipe.Pipes
  ( loadResource
  , site
  , markdownPipe
  , Pipe(..)
  , getContent
  , getFilepath
  , simpleURL
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
import Text.Pandoc (readMarkdown)

import SitePipe.Pandoc
import SitePipe.Parse
import SitePipe.Types
import SitePipe.Templating
import qualified Text.Mustache as M

markdownPipe :: (FromJSON a, ToJSON a) => M.Template -> Pipe a
markdownPipe template = Pipe
  { resourceReader=pandocReader readMarkdown
  , resourceRenderer=renderTemplate template
  , transformResource=id
  , computeURL=simpleURL
  }

simpleURL :: (ToJSON a, Monad m) => a -> m String
simpleURL (toJSON -> getFilepath -> takeBaseName -> name) = return $ name ++ ".html"

site :: Settings -> SiteM () -> IO ()
site settings spec = do
  result <- (runReaderT (Catch.try spec) settings :: IO (Either SitePipeError ()))
  case result of
    Left err -> print err
    Right _ -> return ()

valueToResource :: (MonadThrow m, FromJSON a) => Value -> m a
valueToResource obj =
  case parseEither parseJSON obj of
    Left err -> throwM (JSONErr name err)
    Right result -> return result
  where 
    name = getFilepath obj

loadResource :: (FromJSON a) => (String -> IO String) -> String -> SiteM a
loadResource rReader filepath = do
  cwd <- liftIO $ getCurrentDirectory
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
