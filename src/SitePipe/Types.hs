{-# language OverloadedStrings #-}
module SitePipe.Types
  ( SitePipeError(..)
  , Pipe(..)
  , TemplatePath
  , Pattern
  , getFilepath
  , getContent
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

type TemplatePath = String
type Pattern = String

data Pipe m a = Pipe
  { pandocReader :: String -> Either PandocError Pandoc
  , transformResource :: a -> a
  , transformContent :: Pandoc -> Pandoc
  , pandocWriter :: Pandoc -> m String
  , resourceWriter :: a -> m String
  }

getFilepath :: Value -> String
getFilepath val = T.unpack $ val ^. key "filepath" . _String

getContent :: Value -> String
getContent val = T.unpack $ val ^. key "content" . _String

data SitePipeError =
  YamlErr String String
    | PParseErr P.ParseError
    | MParseErr (MP.ParseError (MP.Token String) MP.Dec)
    | PandocErr PandocError
    | JSONErr String
    | TemplateParseErr P.ParseError
    | TemplateInterpolateErr String [SubstitutionError]

instance Show SitePipeError where
  show (YamlErr path err) = "YAML Parse Error in " ++ path ++ ":\n" ++ err
  show (PandocErr err) = "Pandoc Error: " ++ show err
  show (PParseErr err) = "Template Error: " ++ show err
  show (MParseErr err) = "Meta-data Error: " ++ MP.parseErrorPretty err
  show (JSONErr err) = "JSON Parse Error: " ++ err
  show (TemplateParseErr err) = "Template Parse Error: " ++ show err
  show (TemplateInterpolateErr path errs) = 
    "Template Interpolation Errors in " ++ path ++  ":\n" ++ show errs

instance Exception SitePipeError
