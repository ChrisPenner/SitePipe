{-# language OverloadedStrings #-}
{-# language CPP #-}
module SitePipe.Types
  ( TemplatePath
  , GlobPattern
  , Settings(..)
  , SiteM
  , SitePipeError(..)
  ) where

import Control.Monad.Catch
import Text.Pandoc
import qualified Text.Megaparsec as MP
import qualified Text.Parsec as P
import Text.Mustache.Render (SubstitutionError)
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Text.Mustache.Types as MT

#if MIN_VERSION_megaparsec(6,0,0)
import Data.Void (Void)
type MPErr = Void
#else
type MPErr = Dec
#endif

-- | String alias; Path to a template
type TemplatePath = String

-- | String alias; Valid globbing pattern. Follows shell globbing, allows recursive @/**/*@ globs.
type GlobPattern = String

-- | A monad collecting site instructions. Use liftIO to perform arbitrary IO.
type SiteM a = ReaderT Settings (WriterT [String] IO) a

-- | Global Settings
data Settings = Settings
  { srcDir :: FilePath
  , outputDir :: FilePath
  , globalContext :: MT.Value
  } deriving Show

-- | Collection of possible errors.
data SitePipeError =
  YamlErr String String
    | PParseErr P.ParseError
    | MParseErr (MP.ParseError (MP.Token String) MPErr)
    | PandocErr PandocError
    | JSONErr String String
    | TemplateParseErr P.ParseError
    | TemplateInterpolateErr String [SubstitutionError]
    | SitePipeError String

instance Show SitePipeError where
  show (YamlErr path err) = "YAML Parse Error in " ++ path ++ ":\n" ++ err
  show (PandocErr err) = "Pandoc Error: " ++ show err
  show (PParseErr err) = "Template Error: " ++ show err
  show (MParseErr err) = "Meta-data Error: " ++ MP.parseErrorPretty err
  show (JSONErr path err) = "JSON Parse Error in " ++ path ++ ":\n" ++ err
  show (TemplateParseErr err) = "Template Parse Error: " ++ show err
  show (TemplateInterpolateErr path errs) =
    "Template Interpolation Errors in " ++ path ++  ":\n" ++ show errs
  show (SitePipeError err) = err

instance Exception SitePipeError
