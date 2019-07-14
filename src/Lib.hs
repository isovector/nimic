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

import           Control.Monad
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Functor.Identity
import           Data.Generics hiding (empty)
import           Data.List (find)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Parser
import           System.IO.Unsafe
import           System.Process
import           Types


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


doAParseJob :: Text -> Term Identity
doAParseJob
  = coerceIt
  . either (error "shitty code") id
  . parseOnly parseTerm


unsafeGetPrimitiveBinding :: [Binding] -> Text -> Term Identity
unsafeGetPrimitiveBinding bs name
  = coerceIt $ unsafeGetPrimitiveBinding' bs name


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


coerceIt :: Term Void1 -> Term Identity
coerceIt (Sym s) | T.isPrefixOf "#" s = MatchVariable $ Identity s
                 | T.isPrefixOf "!#" s = Step $ Identity $ MatchVariable $ Identity $ T.drop 1 s
                 | otherwise = Sym s
coerceIt (Group g)         = Group $ foldStepParser g
coerceIt (MatchVariable a) = absurd a
coerceIt (Step a)          = absurd a


foldStepParser :: [Term Void1] -> [Term Identity]
foldStepParser [] = []
foldStepParser (Sym "!" : a : as) = Step (Identity $ coerceIt a) : foldStepParser as
foldStepParser (a : as) = coerceIt a : foldStepParser as


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

