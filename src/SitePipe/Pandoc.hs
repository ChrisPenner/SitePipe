module SitePipe.Pandoc
  ( pandocReader
  , markdownReader
  ) where

import Text.Pandoc
import Control.Monad.Catch

pandocReader :: (ReaderOptions -> String -> (Either PandocError Pandoc)) -> String -> IO String
pandocReader pReader content = writeHtmlString def <$> runPandocReader (pReader def) content

runPandocReader :: (MonadThrow m) => (String -> Either PandocError Pandoc) -> String -> m Pandoc
runPandocReader panReader source =
  case panReader source of
    Left err -> throwM err
    Right pandoc -> return pandoc

markdownReader :: String -> IO String
markdownReader = pandocReader readMarkdown
