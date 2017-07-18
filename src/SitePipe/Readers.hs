module SitePipe.Readers
  (
  -- * Built-in readers
    markdownReader
  , textReader

  -- * Reader Generators
  , mkPandocReader

  -- * Pandoc Writers
  , pandocToHTML
  ) where

import Text.Pandoc
import Control.Monad.Catch

-- | Given any standard pandoc reader (see "Text.Pandoc"; e.g. 'readMarkdown', 'readDocX')
-- makes a resource reader compatible with 'SitePipe.Files.resourceLoader'.
--
-- > docs <- resourceLoader (mkPandocReader readDocX) ["docs/*.docx"]
mkPandocReader :: (ReaderOptions -> String -> Either PandocError Pandoc) -> String -> IO String
mkPandocReader pReader = mkPandocReaderWith pReader id pandocToHTML

-- | Like `mkPandocReader`, but allows you to provide both a @'Pandoc' -> 'Pandoc'@ transformation,
-- which is great for things like relativizing links or running transforms over specific document elements. 
-- See https://hackage.haskell.org/package/pandoc-lens for some useful tranformation helpers. You also specify
-- the tranformation from @Pandoc -> String@ which allows you to pick the output format of the reader.
-- If you're unsure what to use in this slot, the pandocToHTML function is a good choice.
mkPandocReaderWith :: (ReaderOptions -> String -> Either PandocError Pandoc) -> (Pandoc -> Pandoc) -> (Pandoc -> String) -> String -> IO String
mkPandocReaderWith pReader transformer writer content = writer . transformer <$> runPandocReader (pReader def) content

-- | A simple helper which renders pandoc to HTML; good for use with 'mkPandocReaderWith'
pandocToHTML :: Pandoc -> String
pandocToHTML = writeHtmlString def{writerHighlight=True}

-- | Runs the Pandoc reader handling errors.
runPandocReader :: (MonadThrow m) => (String -> Either PandocError Pandoc) -> String -> m Pandoc
runPandocReader panReader source =
  case panReader source of
    Left err -> throwM err
    Right pandoc -> return pandoc

-- | Reads markdown files into html
markdownReader :: String -> IO String
markdownReader = mkPandocReader readMarkdown

-- | Reads text files without processing
textReader :: String -> IO String
textReader = return
