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
parseToken = do
  c <- satisfy $ symbolChar
  cs <- parseToken'
  pure $ T.pack $ c:cs


parseToken' :: Parser [Char]
parseToken' = do
  mc <- peekChar
  case mc of
    Just c | symbolChar c -> (:) <$> char c <*> parseToken'
    _ -> pure []

parseSym :: Parser (Term a)
parseSym = do
  token <- parseToken
  pure $ Sym token

parseGroup :: CanParseVar a => Parser (Term a)
parseGroup = do
  _ <- char '('
  subTerm <- some parseTerm
  skipSpace
  _ <- char ')'
  pure $ Group subTerm

parseImplicitGroup :: CanParseVar a => Parser (Term a)
parseImplicitGroup = do
  subTerm <- some $ do
    skipSpace
    c <- peekChar
    case c of
      Just ';' -> empty
      _ -> parseTerm
  pure $ case subTerm of
    [a] -> a
    _   -> Group subTerm

parseCommaSep :: CanParseVar a => Parser (Term a)
parseCommaSep = do
  _ <- char '{'
  subTerm <- sepBy parseImplicitGroup (skipSpace >> char ';')
  skipSpace
  _ <- char '}'
  pure $ foldr1 (\a b -> Group [ a, Sym ";", b ]) subTerm


symbolChar :: Char -> Bool
symbolChar c = not $ or
  [ isSpace c
  , c == '('
  , c == ')'
  , c == '{'
  , c == '}'
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
  choice [ parseCommaSep
         , parseGroup
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

