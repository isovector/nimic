module Main where

import Text.PrettyPrint.HughesPJ hiding (char, empty, ptext)
import           Control.Monad.State
import           Data.Attoparsec.Text
import qualified Data.Text.IO as T
import           Lib
import           System.Environment

main :: IO ()
main = do
  [filepath] <- getArgs
  contents <- T.readFile filepath
  let res = do
        program <- parseOnly parseImplicitGroup contents
        pure $ evalState (force program) macros
  case res of
    Left err -> do
      putStrLn "nimic parse error:"
      putStrLn err
    Right z -> putStrLn $ render $ ppr z

