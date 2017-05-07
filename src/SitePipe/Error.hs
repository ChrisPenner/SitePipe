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

instance Show SitePipeError where
  show (YamlErr err) = show err
  show (PandocErr err) = show err
  show (PParseErr err) = show err
  show (MParseErr err) = MP.parseErrorPretty err


instance Exception SitePipeError
