{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

module Lib where

import Data.List (find)
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Data
import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T

data Void1 a
  deriving Data

instance Eq (Void1 a) where
  _ == _ = True

instance Ord (Void1 a) where
  compare _ _ = EQ

instance Show (Void1 a) where
  show _ = "Void1"

data Term f
  = Sym Text
  | Group [Term f]
  | MatchVariable (f MatchName)
  | Force (f (Term f))

deriving instance (forall x. Eq x => Eq (f x)) => Eq (Term f)
deriving instance (forall x. Show x => Show (f x)) => Show (Term f)
deriving instance (forall x. Data x => Data (f x), Typeable f) => Data (Term f)

data Macro = Macro
  { macroMatch   :: Term Identity
  , macroRewrite :: Term Identity
  }
  deriving Data

type MatchName = Text

data Binding = Binding
  { bindingName :: MatchName
  , bindingValue :: Term Void1
  }
  deriving (Eq, Show, Data)

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
  _ <- char ')'
  pure $ Group subTerm


symbolChar :: Char -> Bool
symbolChar c = not $ or [ isSpace c, c == '#', c == '(', c == ')']

parseMatchVariable :: CanParseVar a => Parser (Term a)
parseMatchVariable = do
  _ <- char '#'
  varName <- parseVarName
  pure $ MatchVariable varName

parseTerm :: CanParseVar a => Parser (Term a)
parseTerm = do
  skipSpace
  choice [ parseGroup
         , parseMatchVariable
         , parseSym
         ]

class CanParseVar f where
  parseVarName :: Parser (f MatchName)

instance CanParseVar Identity where
  parseVarName = Identity <$> parseToken

instance CanParseVar Void1 where
  parseVarName = empty

attemptToBind :: Term Void1 -> Term Identity -> Maybe [Binding]
attemptToBind (Sym s) (Sym s') | s == s' = Just []
attemptToBind (Group l) (Group l') = do
  guard $ length l == length l'
  bindings <- sequence $ zipWith attemptToBind l l'
  Just $ join bindings
attemptToBind prog (MatchVariable (Identity m)) = Just [Binding m prog]
attemptToBind _ _  = Nothing


coerceBack :: Term Identity -> Term Void1
coerceBack (Sym s)           = Sym s
coerceBack (Group s)         = Group $ fmap coerceBack s
coerceBack (Force s)         = error $ "unforced " ++ show s
coerceBack (MatchVariable s) = error $ "unbound " ++ show s

substBindings :: [Binding] -> Term Identity -> Term Void1
substBindings _ (Sym s) = Sym s
substBindings bs (Group s) = Group $ fmap (substBindings bs) s
substBindings bs (MatchVariable (Identity n)) =
  case find ((== n) . bindingName) bs of
    Just binding -> bindingValue binding
    Nothing      -> error "unbound error!"
substBindings _ (Force _s) = undefined


attemptMacro :: Term Void1 -> Macro -> Maybe (Term Void1)
attemptMacro prog (Macro pattern rewrite) = do
  bs <- attemptToBind prog pattern
  pure $ substBindings bs rewrite



mkMacro :: Text -> Text -> Macro
mkMacro ptext rtext =
  let Right macro = do
        pattern <- parseOnly parseTerm ptext
        rewrite <- parseOnly parseTerm rtext
        pure $ Macro pattern rewrite
   in macro

macros :: [Macro]
macros =
  [ mkMacro "(if true then #a else #b)" "#a"
  , mkMacro "(if false then #a else #b)" "#b"
  ]



bar :: Either String (Maybe (Term Void1))
bar = do
  pattern <- parseOnly parseTerm "(if true then #a else #b)"
  rewrite <- parseOnly parseTerm "#a"
  program <- parseOnly parseTerm "(if true then 5 else 6)"
  let macro = Macro pattern rewrite

  pure $ attemptMacro program macro


-- $> bar
