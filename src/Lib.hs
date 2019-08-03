{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall              #-}

module Lib where

import           Control.Monad
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Generics hiding (empty)
import           Data.Generics.Product
import           Data.List (find)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Lens.Micro
import           Parser
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
  everywhere $ mkT $ \a -> if (a == pattern)
                              then rewrite
                              else a

substBindings :: [Binding] -> Term Identity -> App (Term Void1)
substBindings _ (Sym s) = pure $ Sym s
substBindings bs (Group s) = fmap Group $ traverse (substBindings bs) s
substBindings bs (MatchVariable (Identity n)) =
  case find ((== n) . bindingName) bs of
    Just binding -> pure $ bindingValue binding
    Nothing      -> pure $ Sym n
substBindings bs (Step (Identity t)) = do
  t' <- substBindings bs t
  force t'

doAParseJob :: Text -> Term Identity
doAParseJob
  = coerceIt
  . either (error "shitty code") id
  . parseOnly parseTerm


unsafeGetPrimitiveBinding :: [Binding] -> Text -> App (Term Identity)
unsafeGetPrimitiveBinding bs name = do
  let z = unsafeGetPrimitiveBinding' bs name
  reassoc <- gets $ getAssocs . ctxReassocs
  pure $ coerceIt $ reassoc z


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
  [ Primitive (doAParseJob "(macro #a #b)") $ \bs -> do
      a <- unsafeGetPrimitiveBinding bs "#a"
      b <- unsafeGetPrimitiveBinding bs "#b"
      modify $ field @"ctxDefMacros" %~ (Macro a b :)
      pure $ Sym "defined"

  , Primitive (doAParseJob "(replace #a #b #c)") $ \bs -> do
      a <-  unsafeGetPrimitiveBinding bs "#a"
      let Sym b = unsafeGetPrimitiveBinding' bs "#b"
          c = unsafeGetPrimitiveBinding' bs "#c"
      substBindings [Binding b c] $ substTerm (Sym b) (MatchVariable (Identity b)) a

  , Primitive (doAParseJob "(rassoc #prec #sym)") $ \bs -> do
      let Sym prec = unsafeGetPrimitiveBinding' bs "#prec"
          Sym sym = unsafeGetPrimitiveBinding' bs "#sym"
      mkAssoc (read $ 'A' : T.unpack prec) (rassoc sym)
      pure $ Sym "defined"

  , Primitive (doAParseJob "(bash #cmd)") $ \bs -> do
      let cmdTerm = unsafeGetPrimitiveBinding' bs "#cmd"
          (shellCmd :: String) = T.unpack $ termToShell cmdTerm

      stdout <- lift $ fmap T.pack $ readCreateProcess (shell shellCmd) ""
      pure $ either (error $ "***Shell parse*** " <> T.unpack stdout) id . parseOnly parseImplicitGroup $ stdout

  , Primitive (doAParseJob "(get user input)") $ \_ -> do
      res <- lift getLine
      pure $ either (error $ "bad input" <> res) id . parseOnly parseImplicitGroup $ T.pack res
  ]

doStep :: Term Void1 -> App (Maybe (Term Void1 -> Term Void1, (Macro, [Binding])))
doStep t = do
  reassoc <- gets $ getAssocs . ctxReassocs
  z <- step t
  pure $ fmap (first $ const reassoc ) z

step :: Term Void1 -> App (Maybe (Term Void1, (Macro, [Binding])))
step g@(Group (f:rest)) = do
  mTerm <- step f
  case mTerm of
    Just (t, z) -> pure $ Just (Group (t:rest), z)
    Nothing -> step' g
step t = step' t

step' :: Term Void1 -> App (Maybe (Term Void1, (Macro, [Binding])))
step' t = do
  reassoc <- gets $ getAssocs . ctxReassocs
  ms <- gets ctxDefMacros
  z <- for ms $ \m -> attemptMacro reassoc t m
  pure $ getFirst $ foldMap First z

force :: Term Void1 -> App (Term Void1)
force t = do
  mt <- step t
  case mt of
    Nothing -> pure t
    Just (t', _) -> force t'

coerceIt :: Term Void1 -> Term Identity
coerceIt (Sym s)
  | T.isPrefixOf "#" s  = MatchVariable $ Identity s
  | T.isPrefixOf "!#" s = Step $ Identity $ MatchVariable $ Identity $ T.drop 1 s
  | T.isPrefixOf "!" s  = Step $ Identity $ Sym $ T.drop 1 s
  | otherwise           = Sym s
coerceIt (Group g)         = Group $ foldStepParser g
coerceIt (MatchVariable a) = absurd a
coerceIt (Step a)          = absurd a

foldStepParser :: [Term Void1] -> [Term Identity]
foldStepParser [] = []
foldStepParser (Sym "!" : a : as) = Step (Identity $ coerceIt a) : foldStepParser as
foldStepParser (a : as) = coerceIt a : foldStepParser as

attemptMacro :: (Term Void1 -> Term Void1) -> Term Void1 -> Macro -> App (Maybe (Term Void1, (Macro, [Binding])))
attemptMacro reassoc prog m@(Primitive pattern rewrite) = do
  mbs <- pure $ attemptToBind (reassoc prog) pattern
  case mbs of
    Just bs -> do
      z <- rewrite bs
      pure $ Just (z, (m, bs))
    Nothing -> pure Nothing

attemptMacro reassoc prog m@(Macro pattern rewrite) = do
  mbs <- pure $ attemptToBind (reassoc prog) pattern
  case mbs of
    Just bs -> fmap (Just . (, (m, bs))) $ substBindings bs rewrite
    Nothing -> pure Nothing

mkAssoc :: AssocLevel -> (Term Void1 -> Term Void1) -> App ()
mkAssoc level f = modify $ \s ->
  s & field @"ctxReassocs" <>~ \level' ->
    Endo $
      if level == level'
         then f
         else id

rassoc :: Text -> Term Void1 -> Term Void1
rassoc _ (Group []) = Group []
rassoc sep (Group listOfTerms) =
  let (beforeSep, rest) = span (/= Sym sep) listOfTerms
      afterSep = drop 1 rest
      normedHead = groupOrSingleTerm beforeSep
      normedTail = groupOrSingleTerm afterSep
  in
    case rest of
      [] -> normedHead
      _ -> Group $ [normedHead, Sym sep, rassoc sep normedTail]
rassoc _ term = term

groupOrSingleTerm :: [Term a] -> Term a
groupOrSingleTerm [a] = a
groupOrSingleTerm a = Group a

getAssocs :: (AssocLevel -> Endo (Term Void1)) -> Term Void1 -> Term Void1
getAssocs f = appEndo $ foldMap f $ reverse [minBound .. maxBound]
