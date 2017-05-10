{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module SitePipe.Pipes
  ( loadResource
  , site
  , markdownPipe
  , Pipe(..)
  , getContent
  , getFilepath
  )
    where

import Control.Monad.Catch as Catch
import Data.Aeson.Lens
import Data.Aeson.Types hiding (Parser, parse)
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T

import Text.Pandoc hiding (Null, renderTemplate)

import SitePipe.Parse
import SitePipe.Types
import SitePipe.Templating
import qualified Text.Mustache as M

markdownPipe :: (MonadThrow m, MonadIO m, FromJSON a, ToJSON a) => M.Template -> Pipe m a
markdownPipe template = Pipe
  { pandocReader=readMarkdown def
  , transformResource=id
  , transformContent=id
  , pandocWriter=return . writeHtmlString def
  , resourceWriter=renderTemplate template
  }

site :: IO () -> IO ()
site spec = do
  result <- Catch.try spec :: IO (Either SitePipeError ())
  case result of
    Left err -> print err
    Right _ -> return ()

valueToResource :: (MonadThrow m, FromJSON a) => Value -> m a
valueToResource obj =
  case parseEither parseJSON obj of
    Left err -> throwM (JSONErr err)
    Right result -> return result

runReader :: (MonadThrow m) => (String -> Either PandocError Pandoc) -> String -> m Pandoc
runReader reader source = case reader source of
                             Left err -> throwM err
                             Right pandoc -> return pandoc

loadResource :: (FromJSON a, MonadThrow m, MonadIO m) => Pipe m a -> String -> m a
loadResource Pipe{..} filepath = do
  file <- liftIO $ readFile filepath
  (meta, source) <- processSource filepath file
  pandoc <- transformContent <$> runReader pandocReader source
  content <- pandocWriter pandoc
  transformResource <$> valueToResource (addMeta content meta)
    where
      addMeta content meta =
        meta
        & _Object . at "filepath" ?~ String (T.pack filepath)
        & _Object . at "content" ?~ String (T.pack content)
