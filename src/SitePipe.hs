module SitePipe
  ( 
  -- * SitePipe

  -- | This module re-exports everything you need to build a site.
  -- In addition to exporting all of SitePipe it also exports "Data.Aeson", "Data.Aeson.Lens",
  -- "Control.Lens", "System.FilePath.Posix", and 'liftIO'

  -- * Running SitePipe
    site
  , siteWithGlobals

  -- * Loaders
  ,  resourceLoader

  -- * Writers
  , writeWith
  , writeTemplate
  , textWriter

  -- * Loader/Writers
  , copyFiles
  , copyFilesWith

  -- * Readers
  -- ** Built-in
  , markdownReader
  , textReader

  -- ** Reader Generators
  , mkPandocReader

  -- * Utilities
  , setExt
  , addPrefix

  -- * Types
  , module SitePipe.Types

  -- * Exports
  , module X
  , liftIO
  ) where

import SitePipe.Types
import SitePipe.Files
import SitePipe.Pipes
import SitePipe.Readers
import SitePipe.Utilities
import Data.Aeson.Lens as X
import Data.Aeson as X
import Control.Lens as X hiding ((.=), (<.>))
import System.FilePath.Posix as X

import Control.Monad.IO.Class (liftIO)
