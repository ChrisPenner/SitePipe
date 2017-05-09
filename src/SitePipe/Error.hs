module SitePipe.Error
  ( SitePipeError(..)
  ) where

import Control.Monad.Catch
import Text.Pandoc.Error
import qualified Text.Megaparsec as MP
import qualified Text.Parsec as P

data SitePipeError =
  YamlErr String
    | PParseErr P.ParseError
    | MParseErr (MP.ParseError (MP.Token String) MP.Dec)
    | PandocErr PandocError
    | JSONErr String
    | TemplateParseErr P.ParseError

instance Show SitePipeError where
  show (YamlErr err) = "YAML Error: " ++ show err
  show (PandocErr err) = "Pandoc Error: " ++ show err
  show (PParseErr err) = "Template Error: " ++ show err
  show (MParseErr err) = "Meta-data Error: " ++ MP.parseErrorPretty err
  show (JSONErr err) = "JSON Parse Error: " ++ err
  show (TemplateParseErr err) = "Template Parse Error: " ++ show err


instance Exception SitePipeError
