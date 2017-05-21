{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module SitePipe.Pipes
  ( site
  , siteWithGlobals
  ) where


import Control.Monad.Catch as Catch
import System.Directory
import Control.Monad.Reader
import Data.Foldable
import Control.Monad.Writer
import Options.Applicative
import qualified Text.Mustache.Types as MT
import qualified Data.HashMap.Strict as HM

import SitePipe.Types

site :: SiteM () -> IO ()
site spec = do
  settings <- execParser settingsInfo >>= adjSettings
  clean settings
  (result, warnings) <- runWriterT (runReaderT (Catch.try spec) settings)
  case result of
    Left err -> print (err :: SitePipeError)
    Right _ -> unless (null warnings) (traverse_ putStrLn warnings)

siteWithGlobals :: MT.Value -> SiteM () -> IO ()
siteWithGlobals globals spec = do
  settings <- execParser settingsInfo >>= adjSettings
  clean settings
  (result, warnings) <- runWriterT (runReaderT (Catch.try spec) settings{globalContext=globals})
  case result of
    Left err -> print (err :: SitePipeError)
    Right _ -> unless (null warnings) (traverse_ putStrLn warnings)

settingsInfo :: ParserInfo Settings
settingsInfo = info (settingsP <**> helper)
            ( fullDesc <>
              progDesc "Static site generator" <>
              header "SitePipe - simple static site generator")

settingsP :: Parser Settings
settingsP = Settings <$> strOption srcD <*> strOption outputD <*> pure (MT.Object HM.empty)
  where
    srcD = mconcat [ help "The directory where site source is stored"
                   , metavar "SOURCE_DIR"
                   , short 's'
                   , value "./site"
                   , showDefault
                   ]

    outputD = mconcat [ help "Directory where site will be rendered"
                      , metavar "OUTPUT_DIR"
                      , short 'o'
                      , value "./dist"
                      , showDefault
                      ]

adjSettings :: Settings -> IO Settings
adjSettings Settings{..} = do
  outD <- makeAbsolute outputDir
  srcD <- makeAbsolute srcDir
  return Settings{outputDir=outD, srcDir=srcD, ..}

clean :: Settings -> IO ()
clean (outputDir -> outD) = do
  putStrLn $ "Purging " ++ outD
  exists <- doesDirectoryExist outD
  when exists (removeDirectoryRecursive outD)
  createDirectoryIfMissing False outD
