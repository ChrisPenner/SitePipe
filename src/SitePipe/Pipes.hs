{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module SitePipe.Pipes
  ( site
  )
    where


import Control.Monad.Catch as Catch
import System.Directory
import Control.Monad.Reader

import SitePipe.Types

site :: Settings -> SiteM () -> IO ()
site settings' spec = do
  settings <- adjSettings settings'
  clean settings
  result <- runReaderT (Catch.try spec) settings :: IO (Either SitePipeError ())
  case result of
    Left err -> print err
    Right _ -> return ()

adjSettings :: Settings -> IO Settings
adjSettings Settings{..} = do
  outD <- makeAbsolute outputDir
  srcD <- makeAbsolute srcDir
  return Settings{outputDir=outD, srcDir=srcD, ..}

clean :: Settings -> IO ()
clean (outputDir -> outD) = do
  exists <- doesDirectoryExist outD
  when exists (removeDirectoryRecursive outD)
  createDirectoryIfMissing False outD
