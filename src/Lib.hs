{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE BangPatterns   #-}

{-# OPTIONS_GHC -Wall              #-}

module Lib where

import           Text.PrettyPrint.HughesPJ (render)
import Debug.Trace
import Data.Generics.Product
import Lens.Micro
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


attemptToBind :: (Term Void1 -> Term Void1) -> Term Void1 -> Term Identity -> Maybe [Binding]
attemptToBind reassoc (reassoc -> Sym s) (Sym s') | s == s' = Just []
attemptToBind reassoc (reassoc -> Group l) (Group l') = do
  guard $ length l == length l'
  bindings <- sequence $ zipWith (attemptToBind reassoc) l l'
  Just $ join bindings
attemptToBind _ prog (MatchVariable (Identity m)) = Just [Binding m prog]
attemptToBind _ _ _  = Nothing


substTerm :: Term Identity -> Term Identity -> Term Identity -> Term Identity
substTerm pattern rewrite =
  everywhere $ mkT $ \case
    a | a == pattern -> rewrite
      | otherwise -> a


substBindings :: [Binding] -> Term Identity -> App (Term Void1)
substBindings _ (Sym s) = pure $ Sym s
substBindings bs (Group s) = fmap Group $ traverse (substBindings bs) s
substBindings bs (MatchVariable (Identity n)) =
  case find ((== n) . bindingName) bs of
    Just binding -> pure $ bindingValue binding
    Nothing      -> error $ "unbound error! binding not found " <> T.unpack n
substBindings bs (Step (Identity t)) = do
  t' <- substBindings bs t
  ms <- gets ctxDefMacros
  fmap (fromMaybe (error $ "couldn't step!\n\n" ++ show ms ++ "\n\n\n" ++ show bs ++ "\n\n\n------------>" ++ show t')) $ step t'


doAParseJob :: Text -> Term Identity
doAParseJob
  = coerceIt id
  . either (error "shitty code") id
  . parseOnly parseTerm


unsafeGetPrimitiveBinding :: [Binding] -> Text -> App (Term Identity)
unsafeGetPrimitiveBinding bs name = do
  let z = unsafeGetPrimitiveBinding' bs name
  reassoc <- gets $ getAssocs . ctxReassocs
  pure $ coerceIt reassoc z


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
      a <- unsafeGetPrimitiveBinding bs "#a"
      b <- unsafeGetPrimitiveBinding bs "#b"
      let c = unsafeGetPrimitiveBinding' bs "#c"
      modify $ field @"ctxDefMacros" %~ (Macro a b :)
      pure c

  , Primitive (doAParseJob "(replace #a #b #c)") $ \bs -> do
      a <-  unsafeGetPrimitiveBinding bs "#a"
      let Sym b = unsafeGetPrimitiveBinding' bs "#b"
          c = unsafeGetPrimitiveBinding' bs "#c"
      substBindings [Binding b c] $ substTerm (Sym b) (MatchVariable (Identity b)) a

  , Primitive (doAParseJob "((rassoc #prec #sym) ; #rest)") $ \bs -> do
      let Sym prec = unsafeGetPrimitiveBinding' bs "#prec"
          Sym sym = unsafeGetPrimitiveBinding' bs "#sym"
          rest = unsafeGetPrimitiveBinding' bs "#rest"
      mkAssoc (read $ 'A' : T.unpack prec) (rassoc sym)
      pure rest

  , Primitive (doAParseJob "(bash #cmd)") $ \bs -> do
      let cmdTerm = unsafeGetPrimitiveBinding' bs "#cmd"
          (shellCmd :: String) = T.unpack $ termToShell cmdTerm
          !_ = unsafePerformIO $ putStrLn shellCmd
          stdout = T.pack $ "{ bash-stdout ; " <> (unsafePerformIO $ readCreateProcess (shell shellCmd) "") <> " }"
          shellTerm = either (error $ "***Shell parse*** " <> T.unpack stdout) id . parseOnly parseImplicitGroup $ stdout
      pure shellTerm
  ]

step :: Term Void1 -> App (Maybe (Term Void1))
step t = do
  reassoc <- gets $ getAssocs . ctxReassocs
  ms <- gets ctxDefMacros
  z <- for ms $ \m -> attemptMacro reassoc t m
  pure $ getFirst $ foldMap First z

force :: Term Void1 -> App (Term Void1)
force t = do
  mt <- step t
  case mt of
    Nothing -> pure t
    Just t' -> force t'

coerceIt :: (Term Void1 -> Term Void1) -> Term Void1 -> Term Identity
coerceIt reassoc (reassoc -> Sym s) | T.isPrefixOf "#" s = MatchVariable $ Identity s
                         | T.isPrefixOf "!#" s = Step $ Identity $ MatchVariable $ Identity $ T.drop 1 s
                         | T.isPrefixOf "!" s = Step $ Identity $ Sym $ T.drop 1 s
                         | otherwise = Sym s
coerceIt reassoc (reassoc -> Group g)         = Group $ foldStepParser reassoc g
coerceIt reassoc (reassoc -> MatchVariable a) = absurd a
coerceIt reassoc (reassoc -> Step a)          = absurd a

foldStepParser :: (Term Void1 -> Term Void1) -> [Term Void1] -> [Term Identity]
foldStepParser reassoc [] = []
foldStepParser reassoc (Sym "!" : a : as) = Step (Identity $ coerceIt reassoc a) : foldStepParser reassoc as
foldStepParser reassoc (a : as) = coerceIt reassoc a : foldStepParser reassoc as


attemptMacro :: (Term Void1 -> Term Void1) -> Term Void1 -> Macro -> App (Maybe (Term Void1))
attemptMacro reassoc prog (Primitive pattern rewrite) = do
  mbs <- pure $ attemptToBind reassoc prog pattern
  case mbs of
    Just bs -> do
      z <- rewrite bs
      pure $ Just z
    Nothing -> pure Nothing

attemptMacro reassoc prog (Macro pattern rewrite) = do
  mbs <- pure $ attemptToBind reassoc prog pattern
  case mbs of
    Just bs -> fmap Just $ substBindings bs rewrite
    Nothing -> pure Nothing



mkAssoc :: AssocLevel -> (Term Void1 -> Term Void1) -> App ()
mkAssoc l f = modify $ \s ->
  s & field @"ctxReassocs" <>~ \l' ->
    Endo $
      if l == l'
         then f
         else id

withGroupAssoc
    :: ([Term Void1] -> [Term Void1])
    -> Term Void1
    -> Term Void1
withGroupAssoc f (Group g) = Group $ f g
withGroupAssoc _ a = a

rassoc :: Text -> Term Void1 -> Term Void1
rassoc t = withGroupAssoc go
  where
    go [] = []
    go g =
      let (a, b) = span (/= Sym t) g
       in case b of
            [] -> a
            _ -> [groupIfNotSingleton a, Sym t, groupIfNotSingleton (go $ drop 1 b)]


groupIfNotSingleton :: [Term a] -> Term a
groupIfNotSingleton [a] = a
groupIfNotSingleton a = Group a


getAssocs :: (AssocLevel -> Endo (Term Void1)) -> Term Void1 -> Term Void1
getAssocs f = appEndo $ foldMap f $ reverse [minBound .. maxBound]

