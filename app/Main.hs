module Main where

import           Control.Monad.State
import           Data.Attoparsec.Text
import qualified Data.Text.IO as T
import           Lib
import           System.Environment

main :: IO ()
main = do
  [filepath] <- getArgs
  contents <- T.readFile filepath
  print $ do
    program <- parseOnly parseImplicitGroup contents
    pure $ evalState (force program) macros

