{-# language OverloadedStrings #-}
module SitePipe.Types
  ( SitePipeError(..)
  , Pipe(..)
  , TemplatePath
  , Pattern
  , Settings(..)
  , SiteM
  , getFilepath
  , getRelativeFilepath
  , getContent
  , basicSettings
  ) where

import Control.Monad.Catch
import Text.Pandoc
import qualified Text.Megaparsec as MP
import qualified Text.Parsec as P
import Text.Mustache.Render (SubstitutionError)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Monad.Reader

type TemplatePath = String
type Pattern = String

type SiteM a = ReaderT Settings IO a

data Settings = Settings
  { outputDir :: FilePath
  , srcDir :: FilePath
  } deriving Show

basicSettings :: Settings
basicSettings = Settings
  { outputDir="./dist"
  , srcDir="./site"
  }

data Pipe a = Pipe
  { resourceReader :: String -> IO String
  , transformResource :: a -> a
  , resourceRenderer :: a -> IO String
  , computeURL :: a -> String
  }

getFilepath :: Value -> String
getFilepath val = T.unpack $ val ^. key "filepath" . _String

getRelativeFilepath :: Value -> String
getRelativeFilepath val = T.unpack $ val ^. key "relativePath" . _String

getContent :: Value -> String
getContent val = T.unpack $ val ^. key "content" . _String

data SitePipeError =
  YamlErr String String
    | PParseErr P.ParseError
    | MParseErr (MP.ParseError (MP.Token String) MP.Dec)
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
