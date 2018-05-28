{-# OPTIONS_GHC -Wall -fno-warn-orphans          #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE DeriveDataTypeable                  #-}
{-# LANGUAGE StandaloneDeriving                  #-}
{-# LANGUAGE FlexibleInstances                   #-}
{-# LANGUAGE DataKinds                           #-}
{-# LANGUAGE UndecidableInstances                #-}
{-# LANGUAGE PolyKinds                           #-}
{-# LANGUAGE TypeFamilies                        #-}
{-# LANGUAGE TypeOperators                       #-}
{-# LANGUAGE ConstraintKinds                     #-}
module AST
  ( Spanned(..)
  , ExpPS
  , TypPS
  , PatPS
  , DecPS
  , SrcSpan(..)
  , Exp(..)
  , Typ(..)
  , Pat(..)
  , Dec(..)
  , Nam(..)
  ) where

import U.SrcLoc
import U.FastString
import GHC.Types
import Data.Data hiding ( Fixity )
import Data.Void

-- ------------------------------------------------------------------------
-- Name
-- ------------------------------------------------------------------------

data Nam
  = Nam FastString

-- ------------------------------------------------------------------------
-- Expressions
-- ------------------------------------------------------------------------

data Exp x
  = Lit    (LitX    x) String  Integer
  | Var    (VarX    x) Nam
  | Typ    (TypX    x) (Exp x) (Typ x)
  | Abs    (AbsX    x) Nam     (Exp x)
  | App    (AppX    x) (Exp x) (Exp x)
  | Tup    (TupX    x) (Exp x) (Exp x)
  | Let    (LetX    x) (Dec x) (Exp x)
  | Par    (ParX    x) (Exp x)
  | NewExp (NewExpX x)

type family LitX    x
type family VarX    x
type family TypX    x
type family AbsX    x
type family AppX    x
type family TupX    x
type family LetX    x
type family ParX    x
type family NewExpX x

-- ------------------------------------------------------------------------
-- Types
-- ------------------------------------------------------------------------

data Typ x
  = VarTyp (VarTypX x) Nam
  | FunTyp (FunTypX x) (Typ x) (Typ x)
  | TupTyp (TupTypX x) (Typ x) (Typ x)
  | ParTyp (ParTypX x) (Typ x)
  | NewTyp (NewTypX x)

type family VarTypX x
type family FunTypX x
type family TupTypX x
type family ParTypX x
type family NewTypX x

-- ------------------------------------------------------------------------
-- Patterns
-- ------------------------------------------------------------------------

data Pat x
  = VarPat (VarPatX x) Nam
  | TupPat (TupPatX x) Nam Nam
  | NewPat (NewPatX x)

type family VarPatX x
type family TupPatX x
type family NewPatX x

-- ------------------------------------------------------------------------
-- Declarations
-- ------------------------------------------------------------------------

data Dec x
  = Dec    (DecX    x) (Pat x) (Exp x)
  | NewDec (NewDecX x)

type family DecX    x
type family NewDecX x

-- ------------------------------------------------------------------------
-- Constraint Quanitifications
-- ------------------------------------------------------------------------

type ForallXExp (p :: * -> Constraint) x
  = ( p (LitX    x)
    , p (VarX    x)
    , p (TypX    x)
    , p (AbsX    x)
    , p (AppX    x)
    , p (TupX    x)
    , p (LetX    x)
    , p (ParX    x)
    , p (NewExpX x)
    )

type ForallXTyp (p :: * -> Constraint) x
  = ( p (VarTypX x)
    , p (FunTypX x)
    , p (TupTypX x)
    , p (ParTypX x)
    , p (NewTypX x)
    )

type ForallXPat (p :: * -> Constraint) x
  = ( p (VarPatX x)
    , p (TupPatX x)
    , p (NewPatX x)
    )

type ForallXDec (p :: * -> Constraint) x
  = ( p (DecX    x)
    , p (NewDecX x)
    )

type ForallXAll (p :: * -> Constraint) x
  = ( ForallXExp p x
    , ForallXPat p x
    , ForallXTyp p x
    , ForallXDec p x
    )

-- ------------------------------------------------------------------------
-- Data Instances
-- ------------------------------------------------------------------------

deriving instance ( ForallXAll Data x , Data x ) => Data (Exp x)
deriving instance ( ForallXTyp Data x , Data x ) => Data (Typ x)
deriving instance ( ForallXPat Data x , Data x ) => Data (Pat x)
deriving instance ( ForallXAll Data x , Data x ) => Data (Dec x)
deriving instance Data Nam

-- ------------------------------------------------------------------------
-- Show Instances
-- ------------------------------------------------------------------------

deriving instance ForallXAll Show x => Show (Exp x)
deriving instance ForallXTyp Show x => Show (Typ x)
deriving instance ForallXPat Show x => Show (Pat x)
deriving instance ForallXAll Show x => Show (Dec x)
deriving instance Show Nam

-- ------------------------------------------------------------------------
-- Parsed Source Extensions
-- ------------------------------------------------------------------------

data PSX

type ExpPS
  = Exp PSX
type instance LitX    PSX
  = SrcSpan
type instance VarX    PSX
  = SrcSpan
type instance TypX    PSX
  = SrcSpan
type instance AbsX    PSX
  = SrcSpan
type instance AppX    PSX
  = SrcSpan
type instance TupX    PSX
  = SrcSpan
type instance LetX    PSX
  = SrcSpan
type instance ParX    PSX
  = SrcSpan
type instance NewExpX PSX
  = Void

type TypPS
  = Typ PSX
type instance VarTypX PSX
  = SrcSpan
type instance FunTypX PSX
  = SrcSpan
type instance TupTypX PSX
  = SrcSpan
type instance ParTypX PSX
  = SrcSpan
type instance NewTypX PSX
  = Void

type PatPS
  = Pat PSX
type instance VarPatX PSX
  = SrcSpan
type instance TupPatX PSX
  = SrcSpan
type instance NewPatX PSX
  = Void

type DecPS
  = Dec PSX
type instance DecX    PSX
  = SrcSpan
type instance NewDecX PSX
  = Void

-- ------------------------------------------------------------------------
-- Spanned Type Class
-- ------------------------------------------------------------------------

class Spanned a where
  getSpan :: a -> SrcSpan
  setSpan :: a -> SrcSpan -> a

-- ------------------------------------------------------------------------
-- Spanned Type Instances
-- ------------------------------------------------------------------------

instance (ForallXExp Spanned x) => Spanned (Exp x) where
  getSpan (Lit    ex _ _) = getSpan ex
  getSpan (Var    ex _)   = getSpan ex
  getSpan (Typ    ex _ _) = getSpan ex
  getSpan (Abs    ex _ _) = getSpan ex
  getSpan (App    ex _ _) = getSpan ex
  getSpan (Tup    ex _ _) = getSpan ex
  getSpan (Let    ex _ _) = getSpan ex
  getSpan (Par    ex _)   = getSpan ex
  getSpan (NewExp ex)     = getSpan ex

  setSpan (Lit    ex s i) sp = Lit    (setSpan ex sp) s i
  setSpan (Var    ex x)   sp = Var    (setSpan ex sp) x
  setSpan (Typ    ex m a) sp = Typ    (setSpan ex sp) m a
  setSpan (Abs    ex x n) sp = Abs    (setSpan ex sp) x n
  setSpan (App    ex l m) sp = App    (setSpan ex sp) l m
  setSpan (Tup    ex m n) sp = Tup    (setSpan ex sp) m n
  setSpan (Let    ex d n) sp = Let    (setSpan ex sp) d n
  setSpan (Par    ex m)   sp = Par    (setSpan ex sp) m
  setSpan (NewExp ex)     sp = NewExp (setSpan ex sp)

instance (ForallXTyp Spanned x) => Spanned (Typ x) where
  getSpan (VarTyp ex _)   = getSpan ex
  getSpan (FunTyp ex _ _) = getSpan ex
  getSpan (TupTyp ex _ _) = getSpan ex
  getSpan (ParTyp ex _)   = getSpan ex
  getSpan (NewTyp ex)     = getSpan ex

  setSpan (VarTyp ex x)   sp = VarTyp (setSpan ex sp) x
  setSpan (FunTyp ex a b) sp = FunTyp (setSpan ex sp) a b
  setSpan (TupTyp ex a b) sp = TupTyp (setSpan ex sp) a b
  setSpan (ParTyp ex a)   sp = ParTyp (setSpan ex sp) a
  setSpan (NewTyp ex)     sp = NewTyp (setSpan ex sp)

instance (ForallXPat Spanned x) => Spanned (Pat x) where
  getSpan (VarPat ex _)   = getSpan ex
  getSpan (TupPat ex _ _) = getSpan ex
  getSpan (NewPat ex)     = getSpan ex

  setSpan (VarPat ex x)   sp = VarPat (setSpan ex sp) x
  setSpan (TupPat ex x y) sp = TupPat (setSpan ex sp) x y
  setSpan (NewPat ex)     sp = NewPat (setSpan ex sp)

instance (ForallXDec Spanned x) => Spanned (Dec x) where
  getSpan (Dec    ex _ _) = getSpan ex
  getSpan (NewDec ex)     = getSpan ex

  setSpan (Dec    ex p m) sp = Dec    (setSpan ex sp) p m
  setSpan (NewDec ex)     sp = NewDec (setSpan ex sp)


instance Spanned (Located a) where
  getSpan (L a _) = a
  setSpan (L _ x) sp = L sp x

instance Spanned SrcSpan where
  getSpan = id
  setSpan _ = id

instance Spanned Void where
  getSpan   = error "Impossible!"
  setSpan _ = error "Impossible!"
