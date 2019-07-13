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

import Data.Monoid
import Data.List (find)
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Char
import Data.Maybe
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
  -- TODO(sandy): add type safety so this is only on the bottom later!
  | Step (f (Term f))

deriving instance (forall x. Eq x => Eq (f x)) => Eq (Term f)
deriving instance (forall x. Show x => Show (f x)) => Show (Term f)
deriving instance (forall x. Data x => Data (f x), Typeable f) => Data (Term f)

data Macro = Macro
  { macroMatch   :: Term Identity
  , macroRewrite :: Term Identity
  }
  deriving (Eq, Data, Show)

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
symbolChar c = not $ or [ isSpace c, c == '#', c == '(', c == ')', c == '!']

parseMatchVariable :: CanParseVar a => Parser (Term a)
parseMatchVariable = do
  _ <- char '#'
  varName <- parseVarName
  pure $ MatchVariable varName

parseStep :: CanParseVar a => Parser (Term a)
parseStep = do
  _ <- char '!'
  v <- parseMatchVariable
  pure $ Step $ introduce v

parseTerm :: CanParseVar a => Parser (Term a)
parseTerm = do
  skipSpace
  choice [ parseGroup
         , parseMatchVariable
         , parseStep
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

attemptToBind :: Term Void1 -> Term Identity -> Maybe [Binding]
attemptToBind (Sym s) (Sym s') | s == s' = Just []
attemptToBind (Group l) (Group l') = do
  guard $ length l == length l'
  bindings <- sequence $ zipWith attemptToBind l l'
  Just $ join bindings
attemptToBind prog (MatchVariable (Identity m)) = Just [Binding m prog]
attemptToBind _ _  = Nothing


substBindings :: [Macro] -> [Binding] -> Term Identity -> Term Void1
substBindings _ _ (Sym s) = Sym s
substBindings ms bs (Group s) = Group $ fmap (substBindings ms bs) s
substBindings _ bs (MatchVariable (Identity n)) =
  case find ((== n) . bindingName) bs of
    Just binding -> bindingValue binding
    Nothing      -> error "unbound error!"
substBindings ms bs (Step (Identity s)) =
  fromMaybe (error "couldn't step!")
    $ step ms
    $ substBindings ms bs s


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
  , mkMacro "(if #c then #a else #b)" "(if !#c then #a else #b)"
  , mkMacro "(not true)" "false"
  ]

step :: [Macro] -> Term Void1 -> Maybe (Term Void1)
step ms t = getFirst $ foldMap (First . attemptMacro ms t) ms

force :: [Macro] -> Term Void1 -> Term Void1
force ms t = fromMaybe t $ fmap (force ms) $ step ms t

attemptMacro :: [Macro] -> Term Void1 -> Macro -> Maybe (Term Void1)
attemptMacro ms prog (Macro pattern rewrite) = do
  bs <- attemptToBind prog pattern
  pure $ substBindings ms bs rewrite



bar :: Either String (Term Void1)
bar = do
  program <- parseOnly parseTerm "(if (not true) then 5 else 6)"
  pure $ force macros program


-- $> bar
