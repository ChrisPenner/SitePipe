module SitePipe.Parse
  ( processSource
  ) where

import Control.Monad.Catch hiding (try)
import Text.Megaparsec
import Text.Megaparsec.String
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Data.Yaml hiding (Parser)
import SitePipe.Types
import Data.ByteString.Char8 (pack)
import Data.Maybe

splitMeta :: MonadThrow m => String -> String -> m (String, String)
splitMeta ident str =
  case parse resourceP ident str of
    Left err -> throwM (MParseErr err)
    Right res -> return res

yamlParser :: Parser String
yamlParser = do
  _ <- yamlSep
  manyTill anyChar (try (eol >> yamlSep))
    where
      yamlSep = string "---" >> eol

parseMeta :: MonadThrow m => String -> String -> m Value
parseMeta ident metaBlock =
  case decodeEither (pack metaBlock) of
    Left err -> throwM (YamlErr ident err)
    Right (Object metaObj) -> return (Object metaObj)
    Right Null -> return (Object HM.empty)
    Right _ -> throwM (YamlErr ident "Top level yaml must be key-value pairs")

processSource :: MonadThrow m => String -> String -> m (Value, String)
processSource ident source = do
  (metaBlock, contents) <- splitMeta ident source
  metaObj <- parseMeta ident metaBlock
  return (metaObj, contents)

resourceP :: Parser (String, String)
resourceP = do
  yaml <- fromMaybe "" <$> optional yamlParser
  space
  rest <- manyTill anyChar eof
  return (yaml, rest)

