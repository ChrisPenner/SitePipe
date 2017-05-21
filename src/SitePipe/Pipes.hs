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

-- | Build a site generator from a set of rules embedded in a 'SiteM'.
-- Use this in your @main@ function.
--
-- > main :: IO ()
-- > main = site $ do
-- >   posts <- resourceLoader markdownReader ["posts/*.md"]
-- >   writeTemplate "templates/post.html" posts
site :: SiteM () -> IO ()
site = siteWithGlobals (MT.Object mempty)

-- | Like 'site', but allows you to pass an 'MT.Value' Object which consists
-- of an environment which is available inside your templates.
--
-- This is useful for globally providing utility functions for use in your templates.
--
-- > import qualified Text.Mustache as MT
-- > import qualified Text.Mustache.Types as MT
-- > utilityFuncs :: MT.Value
-- > utilityFuncs = MT.object
-- >   ["truncate" MT.~> MT.overText (T.take 30)
-- >   ]
-- >
-- > main :: IO ()
-- > main = siteWithGlobals utilityFuncs $ do
-- >  -- your site ...
--
-- > <!-- in your template -->
-- > {{#truncate}}
-- >   Anything inside this block will be truncated to 30 chars.
-- >   {{vars}} are interpolated before applying the function.
-- > {{/truncate}}
siteWithGlobals :: MT.Value -> SiteM () -> IO ()
siteWithGlobals globals spec = do
  settings <- execParser settingsInfo >>= adjSettings
  clean (outputDir settings)
  (result, warnings) <- runWriterT (runReaderT (Catch.try spec) settings{globalContext=globals})
  case result of
    Left err -> print (err :: SitePipeError)
    Right _ -> unless (null warnings) (traverse_ putStrLn warnings)

-- | Argument info for option parsing.
settingsInfo :: ParserInfo Settings
settingsInfo = info (settingsP <**> helper)
            ( fullDesc <>
              progDesc "Static site generator" <>
              header "SitePipe - simple static site generator")

-- | Settings parser
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

-- | Make given source and output dirs relative.
adjSettings :: Settings -> IO Settings
adjSettings Settings{..} = do
  outD <- makeAbsolute outputDir
  srcD <- makeAbsolute srcDir
  return Settings{outputDir=outD, srcDir=srcD, ..}

-- | Remove output directory if it exists and set up for next write.
-- This is called by 'site' automatically.
clean :: FilePath -> IO ()
clean outD = do
  putStrLn $ "Purging " ++ outD
  exists <- doesDirectoryExist outD
  when exists (removeDirectoryRecursive outD)
  createDirectoryIfMissing False outD
