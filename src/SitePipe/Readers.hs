module SitePipe.Readers
  ( mkPandocReader
  , markdownReader
  , textReader
  ) where

import Text.Pandoc
import Control.Monad.Catch

mkPandocReader :: (ReaderOptions -> String -> (Either PandocError Pandoc)) -> String -> IO String
mkPandocReader pReader content = writeHtmlString def <$> runPandocReader (pReader def) content

runPandocReader :: (MonadThrow m) => (String -> Either PandocError Pandoc) -> String -> m Pandoc
runPandocReader panReader source =
  case panReader source of
    Left err -> throwM err
    Right pandoc -> return pandoc

markdownReader :: String -> IO String
markdownReader = mkPandocReader readMarkdown

textReader :: String -> IO String
textReader = return
