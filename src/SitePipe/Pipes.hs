{-# language OverloadedStrings #-}
module SitePipe.Pipes
  -- ( splitMeta
  -- , parseMeta
  -- , parseMarkdown
  -- )
    where

import Text.Megaparsec
import Text.Megaparsec.String
import Control.Monad.Catch hiding (try)
import Data.ByteString.Char8 (pack)

import Text.Pandoc
import qualified Data.Text as T

import Data.Yaml (Object, decodeEither, Value(..))
import qualified Data.HashMap.Strict as HM

import SitePipe.Error

type Resource = Object
type Env = Object

toResource :: Object -> Pandoc -> Resource
toResource obj pandoc = HM.insert "content" (String . T.pack $ writeResult pandoc) obj

parseMarkdownResource :: MonadThrow m => String -> String -> m (Object, Pandoc)
parseMarkdownResource ident source = do
  (metaBlock, contents) <- splitMeta ident source
  metaObj <- parseMeta metaBlock
  pandoc <- parseMarkdown contents
  return (metaObj, pandoc)

writeResult :: Pandoc -> String
writeResult = writeHtmlString def

splitMeta :: MonadThrow m => String -> String -> m (String, String)
splitMeta ident str =
  case parse yamlParser ident str of
    Left err -> throwM (MParseErr err)
    Right res -> return res

yamlParser :: Parser (String, String)
yamlParser = do
  _ <- yamlSep
  yamlMap <- manyTill anyChar (try (eol >> yamlSep))
  space
  rest <- manyTill anyChar eof
  return (yamlMap, rest)
    where
      yamlSep = string "---" >> eol

parseMeta :: MonadThrow m => String -> m Object
parseMeta metaBlock =
  case decodeEither (pack metaBlock) of
    Left err -> throwM (YamlErr err)
    Right metaObj -> return metaObj

parseMarkdown :: MonadThrow m => String -> m Pandoc
parseMarkdown mkdn = case readMarkdown def mkdn of
                  Left pandocError -> throwM (PandocErr pandocError)
                  Right res -> return res
