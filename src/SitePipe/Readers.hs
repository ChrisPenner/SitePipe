module SitePipe.Readers
  (
  -- * Built-in readers
    markdownReader
  , textReader

  -- * Reader Generators
  , mkPandocReader
  ) where

import Text.Pandoc
import Control.Monad.Catch

-- | Given any standard pandoc reader (see "Text.Pandoc"; e.g. 'readMarkdown', 'readDocX')
-- makes a resource reader compatible with 'SitePipe.Files.resourceLoader'.
--
-- > docs <- resourceLoader (mkPandocReader readDocX) ["docs/*.docx"]
mkPandocReader :: (ReaderOptions -> String -> (Either PandocError Pandoc)) -> String -> IO String
mkPandocReader pReader content = writeHtmlString def <$> runPandocReader (pReader def) content

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
