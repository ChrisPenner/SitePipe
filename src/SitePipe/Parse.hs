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

-- | Parses yaml block from the file if it exists, returning the inner yaml block and the remaining file contents
resourceP :: Parser (String, String)
resourceP = do
  yaml <- fromMaybe "" <$> optional yamlParser
  space
  rest <- manyTill anyChar eof
  return (yaml, rest)

-- | Given an identifier and file contents runs the yaml parser and returns
-- the contents of the yaml block and the remaining file contents; handling
-- any errors.
splitMeta :: MonadThrow m => String -> String -> m (String, String)
splitMeta ident str =
  case parse resourceP ident str of
    Left err -> throwM (MParseErr err)
    Right res -> return res

-- | Parses a yaml metadata block, returning the string which contains the yaml.
yamlParser :: Parser String
yamlParser = do
  _ <- yamlSep
  manyTill anyChar (try (eol >> yamlSep))
    where
      yamlSep = string "---" >> eol

-- | Decodes a yaml metadata block into an Aeson object containing the data in the yaml.
decodeMeta :: MonadThrow m => String -> String -> m Value
decodeMeta ident metaBlock =
  case decodeEither (pack metaBlock) of
    Left err -> throwM (YamlErr ident err)
    Right (Object metaObj) -> return (Object metaObj)
    Right Null -> return (Object HM.empty)
    Right _ -> throwM (YamlErr ident "Top level yaml must be key-value pairs")

-- | Given a resource identifier and the file contents; parses and returns
-- a 'Value' representing any metadata and the file contents.
processSource :: MonadThrow m => String -> String -> m (Value, String)
processSource ident source = do
  (metaBlock, contents) <- splitMeta ident source
  metaObj <- decodeMeta ident metaBlock
  return (metaObj, contents)

