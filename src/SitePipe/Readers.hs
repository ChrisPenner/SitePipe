{-# LANGUAGE FlexibleContexts #-}

module SitePipe.Readers
  (
  -- * Built-in readers
    markdownReader
  , textReader

  -- * Reader Generators
  , mkPandocReader
  , mkPandocReaderWith
  , readMarkdown

  -- * Pandoc Writers
  , pandocToHTML
  ) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Catch
import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Highlighting
import Data.Text (pack, unpack)

-- | Given any standard pandoc reader (see "Text.Pandoc"; e.g. 'readMarkdown', 'readDocX')
-- makes a resource reader compatible with 'SitePipe.Files.resourceLoader'.
--
-- > docs <- resourceLoader (mkPandocReader readDocX) ["docs/*.docx"]
mkPandocReader :: (ReaderOptions -> String -> PandocIO Pandoc) -> String -> IO String
mkPandocReader pReader = mkPandocReaderWith pReader pure pandocToHTML

-- | Like `mkPandocReader`, but allows you to provide both a @'Pandoc' -> 'Pandoc'@ transformation,
-- which is great for things like relativizing links or running transforms over specific document elements.
-- See https://hackage.haskell.org/package/pandoc-lens for some useful tranformation helpers. You also specify
-- the tranformation from @Pandoc -> String@ which allows you to pick the output format of the reader.
-- If you're unsure what to use in this slot, the pandocToHTML function is a good choice.
mkPandocReaderWith :: (ReaderOptions -> String -> PandocIO Pandoc) -> (Pandoc -> PandocIO Pandoc) -> (Pandoc -> PandocIO String) -> String -> IO String
mkPandocReaderWith pReader transformer writer content =
  runPandoc $ writer =<< transformer =<< pReader def content

-- | A simple helper which renders pandoc to HTML; good for use with 'mkPandocReaderWith'
pandocToHTML :: Pandoc -> PandocIO String
pandocToHTML p = fmap unpack $ writeHtml5String def{writerHighlightStyle = Just pygments} p

-- | Runs the Pandoc reader handling errors.
runPandoc :: PandocIO a -> IO a
runPandoc m = do
  z <- flip evalStateT def $ runExceptT $ unPandocIO m
  case z of
    Left e  -> throwM e
    Right a -> pure a

-- | Reads markdown files into html
markdownReader :: String -> IO String
markdownReader = mkPandocReader $ \ro s -> readMarkdown ro $ pack s

-- | Reads text files without processing
textReader :: String -> PandocIO String
textReader = pure
