{-# language OverloadedStrings #-}
module SitePipe.Pipes
  ( parseResource
  , toResources
  , PandocReader
  , site
  )
    where

import Text.Megaparsec as MP
import Text.Megaparsec.String
import Control.Monad.Catch as Catch
import Control.Arrow
import Data.ByteString.Char8 (pack)
import Data.Maybe
import Data.Aeson.Lens
import Data.Aeson.Types hiding (Parser, parse)
import Data.Either
import Control.Lens
import Control.Monad.Except
import qualified Data.Text as T

import Text.Pandoc hiding (Null)

import Data.Yaml (decodeEither, Value(..))
import qualified Data.HashMap.Lazy as HM

import SitePipe.Error

site :: IO () -> IO ()
site spec = do
  result <- Catch.try spec :: IO (Either SitePipeError ())
  case result of
    Left err -> print "ERR!" >> print err
    Right _ -> return ()

toResource :: Value -> Pandoc -> Value
toResource obj pandoc = obj & (_Object . at "content") ?~ String (T.pack $ writeResult pandoc)

toResources :: (MonadThrow m, FromJSON a) => [Value] -> m [a]
toResources objs =
  let (errs, results) = (lefts &&& rights) . fmap (parseEither parseJSON) $ objs
   in case listToMaybe errs of
        Just err -> throwM (JSONErr err)
        Nothing -> return results

parseResource :: MonadThrow m => PandocReader -> String -> String -> m Value
parseResource reader ident source = do
  (metaBlock, contents) <- splitMeta ident source
  metaObj <- parseMeta metaBlock
  pandoc <- runReader reader contents
  return $ toResource metaObj pandoc

resourceP :: Parser (String, String)
resourceP = do
  yaml <- fromMaybe "" <$> optional yamlParser
  space
  rest <- manyTill anyChar eof
  return (yaml, rest)

writeResult :: Pandoc -> String
writeResult = writeHtmlString def

splitMeta :: MonadThrow m => String -> String -> m (String, String)
splitMeta ident str =
  case parse resourceP ident str of
    Left err -> throwM (MParseErr err)
    Right res -> return res

yamlParser :: Parser String
yamlParser = do
  _ <- yamlSep
  manyTill anyChar (MP.try (eol >> yamlSep))
    where
      yamlSep = string "---" >> eol

parseMeta :: MonadThrow m => String -> m Value
parseMeta metaBlock =
  case decodeEither (pack metaBlock) of
    Left err -> throwM (YamlErr err)
    Right (Object metaObj) -> return (Object metaObj)
    Right Null -> return (Object HM.empty)
    Right _ -> throwM (YamlErr "Top level yaml must be key-value pairs")

type PandocReader = ReaderOptions -> String -> Either PandocError Pandoc
runReader :: MonadThrow m => PandocReader -> String -> m Pandoc
runReader reader source = case reader def source of
                  Left pandocError -> throwM (PandocErr pandocError)
                  Right res -> return res
