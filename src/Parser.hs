{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall              #-}

module Parser where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T
import           Types


parseToken :: Parser Text
parseToken = takeWhile1 symbolChar

parseSym :: Parser (Term a)
parseSym = do
  token <- parseToken
  pure $ Sym token

parseGroup :: CanParseVar a => Parser (Term a)
parseGroup = do
  _ <- char '('
  subTerm <- many parseTerm
  skipSpace
  _ <- char ')'
  pure $ Group subTerm

parseImplicitGroup :: CanParseVar a => Parser (Term a)
parseImplicitGroup = do
  subTerm <- many parseTerm
  skipSpace
  endOfInput
  pure $
    case subTerm of
      [a] -> a
      _   -> Group subTerm


symbolChar :: Char -> Bool
symbolChar c = not $ or
  [ isSpace c
  , c == '('
  , c == ')'
  ]

parseMatchVariable :: CanParseVar a => Parser (Term a)
parseMatchVariable = do
  _ <- char '#'
  varName <- parseVarName
  pure $ MatchVariable varName

parseStep :: CanParseVar a => Parser (Term a)
parseStep = do
  _ <- char '!'
  v <- parseTerm
  pure $ Step $ introduce v

parseTerm :: CanParseVar a => Parser (Term a)
parseTerm = do
  skipSpace
  choice [ parseGroup
         , parseSym
         ]

class CanParseVar f where
  parseVarName :: Parser (f MatchName)
  introduce :: Term f -> f (Term f)

instance CanParseVar Identity where
  parseVarName = Identity <$> parseToken
  introduce = Identity

instance CanParseVar Void1 where
  parseVarName = empty
  introduce = error "can't introduce"

