{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

module Types where

import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Generics hiding (empty)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Generics as GG
import           Text.PrettyPrint.HughesPJ hiding (char, empty, ptext, (<>))

ppr :: Term Void1 -> Doc
ppr (Sym t) = text $ T.unpack t
ppr (a@(Group [_, Sym ";", _])) = braces $ vcat $ punctuate (text ";") $ fmap ppr $ unrollSemis a
ppr (Group t) = parens $ sep $ fmap ppr t
ppr (MatchVariable t) = absurd t
ppr (Step t) = absurd t

unrollSemis :: Term Void1 -> [Term Void1]
unrollSemis (Group [a, Sym ";", b]) = a : unrollSemis b
unrollSemis a = [a]

pprId :: Term Identity -> Doc
pprId (Sym t) = text $ T.unpack t
pprId (Group t) = parens $ sep $ fmap pprId t
pprId (MatchVariable (Identity t)) = text $ T.unpack t
pprId (Step (Identity t)) = text "!" <> pprId t


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

type App = State NimicCtx

data Macro
  = Primitive (Term Identity) ([Binding] -> App (Term Void1))
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


data NimicCtx = NimicCtx
  { ctxDefMacros :: [Macro]
  , ctxReassocs :: AssocLevel -> Endo (Term Void1)
  } deriving (GG.Generic)


data AssocLevel
  = A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9
  deriving (Eq, Ord, Show, Bounded, Enum, Read)


absurd :: Void1 a -> b
absurd a = case a of

