{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad
import Control.Applicative

import Data.Void
import Data.Text (Text)
import Data.Char
import Data.Maybe
import Data.Attoparsec.Text
import qualified Data.Text as T

data Term f
  = Sym Text
  | Group [Term f]
  | MatchVariable f
  deriving (Eq, Ord, Show)

data Macro
  = Macro {
      match:: Term Text,
      rewrite:: Term Text
    }

type MatchName = Text

data Binding 
  = Binding {
      name:: MatchName,
      value:: Term Void
    }
    deriving (Eq, Ord, Show)
  
parseToken :: Parser Text
parseToken = do
  c <- satisfy $ symbolChar
  cs <- parseToken'
  pure $ T.pack $ c:cs
  

parseToken' :: Parser [Char]
parseToken' = do
  isEnd <- atEnd
  if isEnd
    then pure []
    else (:) <$> satisfy symbolChar <*> parseToken'

parseSym :: Parser (Term MatchName)
parseSym = do
  token <- parseToken
  pure $ Sym token

parseGroup :: Parser (Term MatchName)
parseGroup = do
  char '('
  subTerm <- parseTerm
  char ')'
  pure $ Group [subTerm]


symbolChar :: Char -> Bool
symbolChar c = not $ or [ isSpace c, c == '#', c == '(', c == ')'] 

parseMatchVariable :: Parser (Term MatchName)
parseMatchVariable = do
  char '#'
  varName <- parseToken
  pure $ MatchVariable varName

parseTerm :: Parser (Term MatchName)
parseTerm = do
  --skipSpace
  parseSym
-- Test choice [
--         ]

attemptToBind :: Term Void -> Term Text -> Maybe [Binding]
attemptToBind (Sym s) (Sym s') | s == s' = Just []
attemptToBind (Group l) (Group l') = do
  guard $ length l == length l'
  bindings <- sequence $ zipWith attemptToBind l l'
  Just $ join bindings

attemptToBind prog (MatchVariable m) = Just [Binding m prog]
attemptToBind _ _  = Nothing



runParser :: Parser a -> Text -> Either String a 
runParser p source = runParser' $ parse p source


runParser' iRes = case iRes of
  Fail _ _ msg -> Left msg
  Partial f -> runParser' $ f ""
  Done i r -> Right r

bar = parseTest parseGroup "(a)"
  

-- $> bar
