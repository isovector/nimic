{-# LANGUAGE OverloadedStrings #-}

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
  prelcont <- T.readFile "examples/prelude.nim"
  progcont <- T.readFile filepath
  let res = do
        prelude <- parseOnly parseImplicitGroup prelcont
        program <- parseOnly parseImplicitGroup progcont
        pure $ flip evalState macros $ do
          ran_prelude <- force prelude
          force $ Group [ ran_prelude, Sym ";",  program ]
  case res of
    Left err -> do
      putStrLn "nimic parse error:"
      putStrLn err
    Right z -> putStrLn $ render $ ppr z

