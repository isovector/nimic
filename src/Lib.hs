{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall              #-}

module Lib where

import Data.Generics hiding (empty)
import Data.Traversable
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Functor.Identity
import           Data.List (find)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.HughesPJ hiding (char, empty, ptext, (<>))
import System.Process
import System.IO.Unsafe

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

ppr :: Term Void1 -> Doc
ppr (Sym t) = text $ T.unpack t
ppr (a@(Group [_, Sym ";", _])) = braces $ vcat $ punctuate (text ";") $ fmap ppr $ unrollSemis a
ppr (Group t) = parens $ sep $ fmap ppr t
ppr (MatchVariable t) = absurd t
ppr (Step t) = absurd t

unrollSemis :: Term Void1 -> [Term Void1]
unrollSemis (Group [a, Sym ";", b]) = a : unrollSemis b
unrollSemis a = [a]

deriving instance (forall x. Eq x => Eq (f x)) => Eq (Term f)
deriving instance (forall x. Show x => Show (f x)) => Show (Term f)
deriving instance (forall x. Data x => Data (f x), Typeable f) => Data (Term f)

data Macro
  = Primitive (Term Identity) ([Binding] -> State [Macro] (Term Void1))
  | Macro
    { macroMatch   :: Term Identity
    , macroRewrite :: Term Identity
    }

instance Show Macro where
  show (Primitive _ _) = "Primitive"
  show (Macro a b) = "Macro (" ++ show a ++ ") (" ++ show b ++ ")"

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
  v <- parseMatchVariable
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

attemptToBind :: Term Void1 -> Term Identity -> Maybe [Binding]
attemptToBind (Sym s) (Sym s') | s == s' = Just []
attemptToBind (Group l) (Group l') = do
  guard $ length l == length l'
  bindings <- sequence $ zipWith attemptToBind l l'
  Just $ join bindings

attemptToBind prog (MatchVariable (Identity m)) = Just [Binding m prog]
attemptToBind _ _  = Nothing

substTerm :: Term Identity -> Term Identity -> Term Identity -> Term Identity
substTerm pattern rewrite =
  everywhere $ mkT $ \case
    a | a == pattern -> rewrite
      | otherwise -> a

substBindings :: [Binding] -> Term Identity -> State [Macro] (Term Void1)
substBindings _ (Sym s) = pure $ Sym s
substBindings bs (Group s) = fmap Group $ traverse (substBindings bs) s
substBindings bs (MatchVariable (Identity n)) =
  case find ((== n) . bindingName) bs of
    Just binding -> pure $ bindingValue binding
    Nothing      -> error "unbound error!"
substBindings bs (Step (Identity t)) = do
  t' <- substBindings bs t
  ms <- get
  fmap (fromMaybe (error $ "couldn't step!\n\n" ++ show ms ++ "\n\n\n" ++ show bs ++ "\n\n\n------------>" ++ show t')) $ step t'


mkMacro :: Text -> Text -> Macro
mkMacro ptext rtext =
  let macro = either (error . show) id $ do
        pattern <- parseOnly parseTerm ptext
        rewrite <- parseOnly parseTerm rtext
        pure $ Macro (coerceIt pattern) (coerceIt rewrite)
   in macro

doAParseJob :: Text -> Term Identity
doAParseJob
  = coerceIt
  . either (error "shitty code") id
  . parseOnly parseTerm

unsafeGetPrimitiveBinding :: [Binding] -> Text -> Term Identity
unsafeGetPrimitiveBinding bs name
  = coerceIt
  . bindingValue
  . maybe (error "unsafely") id
  $ find ((== name) . bindingName) bs

unsafeGetPrimitiveBinding' :: [Binding] -> Text -> Term Void1
unsafeGetPrimitiveBinding' bs name
  = bindingValue
  . maybe (error "unsafely2") id
  $ find ((== name) . bindingName) bs

termToShell :: Term Void1 -> Text
termToShell (Sym s) = s
termToShell (Group ts) = foldMap ((<> " ") . termToShell) ts
termToShell o = T.pack $ "echo 'what the heck are you doing" <> show o <> "'"

macros :: [Macro]
macros =
  [ Primitive (doAParseJob "((macro #a #b); #c)") $ \bs -> do
      let a = unsafeGetPrimitiveBinding bs "#a"
          b = unsafeGetPrimitiveBinding bs "#b"
          c = unsafeGetPrimitiveBinding' bs "#c"
      modify $ (Macro a b :)
      pure c
  , Primitive (doAParseJob "(replace #a #b #c)") $ \bs -> do
      let a = unsafeGetPrimitiveBinding bs "#a"
          Sym b = unsafeGetPrimitiveBinding' bs "#b"
          c = unsafeGetPrimitiveBinding' bs "#c"
      substBindings [Binding b c] $ substTerm (Sym b) (MatchVariable (Identity b)) a
  , Primitive (doAParseJob "(bash #cmd)") $ \bs -> do
      let cmdTerm = unsafeGetPrimitiveBinding' bs "#cmd"
          (shellCmd :: String) = T.unpack $ termToShell cmdTerm
          stdout = T.pack $ unsafePerformIO $ readCreateProcess (shell shellCmd) ""
          shellTerm = either (error "***Shell parse***") id . parseOnly parseImplicitGroup $ stdout
      pure shellTerm
  ]

step :: Term Void1 -> State [Macro] (Maybe (Term Void1))
step t = do
  ms <- get
  z <- for ms $ \m -> attemptMacro t m
  pure $ getFirst $ foldMap First z

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
  mbs <- pure $ attemptToBind prog pattern
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
  program <- parseOnly parseTerm "(bash ls)"
  pure $ evalState (force program) macros


-- $> ppr bar
