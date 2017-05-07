module Main where

import SitePipe.Pipes
import SitePipe.Templating
import Control.Monad
import Control.Monad.Except

main :: IO ()
main = void . runExceptT $ do
  example <- liftIO $ readFile "example.md"
  template <- liftIO $ readFile "example.html"
  (env, post) <- parseMarkdownResource "example.md" example
  result <- render "example.md" template (toResource env post)
  liftIO $ putStr result
