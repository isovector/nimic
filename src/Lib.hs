{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

module Lib where

import Data.Traversable
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Data
import           Data.Functor.Identity
import           Data.List (find)
import           Data.Maybe
import           Data.Monoid
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

data Macro
  = Primitive (Term Void1) ([Binding] -> State [Macro] (Term Void1))
  | Macro
    { macroMatch   :: Term Identity
    , macroRewrite :: Term Identity
    }

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
symbolChar c = not $ or [ isSpace c, c == '(', c == ')', c == '!']

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


substBindings :: [Binding] -> Term Identity -> State [Macro] (Term Void1)
substBindings _ (Sym s) = pure $ Sym s
substBindings bs (Group s) = fmap Group $ traverse (substBindings bs) s
substBindings bs (MatchVariable (Identity n)) =
  case find ((== n) . bindingName) bs of
    Just binding -> pure $ bindingValue binding
    Nothing      -> error "unbound error!"
substBindings bs (Step (Identity t)) = do
  t' <- substBindings bs t
  fmap (fromMaybe (error "couldn't step!")) $ step t'


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
  , Primitive (Group [Sym "macro", Sym "#a", Sym "#b"]) (\bs -> do
      let Just a = find ((== "#a") . bindingName) bs
          Just b = find ((== "#b") . bindingName) bs
      modify $ (++ [Macro (coerceIt $ bindingValue a) (coerceIt $ bindingValue b)])
      pure $ Sym "defined"
    )
  ]

step :: Term Void1 -> State [Macro] (Maybe (Term Void1))
step t = do
  ms <- get
  z <- for ms $ \m -> attemptMacro t m
  pure $ getFirst $ foldMap First z

  -- pure $ getFirst $ foldMap (First . attemptMacro t) ms

force :: Term Void1 -> State [Macro] (Term Void1)
force t = do
  mt <- step t
  case mt of
    Nothing -> pure t
    Just t' -> force t'

absurd :: Void1 a -> b
absurd a = case a of


coerceIt :: Term Void1 -> Term Identity
coerceIt (Sym s) | T.isPrefixOf "#" s = MatchVariable $ Identity s
                 | T.isPrefixOf "!#" s = Step $ Identity $ MatchVariable $ Identity $ T.drop 1 s
                 | otherwise = Sym s
coerceIt (Group g)         = Group $ fmap coerceIt g
coerceIt (MatchVariable a) = absurd a
coerceIt (Step a)          = absurd a


attemptMacro :: Term Void1 -> Macro -> State [Macro] (Maybe (Term Void1))
attemptMacro prog (Primitive pattern rewrite) = do
  let pattern' = coerceIt pattern
  mbs <- pure $ attemptToBind prog pattern'
  case mbs of
    Just bs -> do
      z <- rewrite bs
      pure $ Just z
    Nothing -> pure Nothing

attemptMacro prog (Macro pattern rewrite) = do
  mbs <- pure $ attemptToBind prog pattern
  case mbs of
    Just bs -> fmap Just $ substBindings bs rewrite
    Nothing -> pure Nothing



bar :: Either String (Term Void1)
bar = do
  program <- parseOnly parseTerm "(macro defined finished)"
  pure $ evalState (force program) macros


-- $> bar
