{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
This module provides 'Borrows', which is a heterogeneous list of 'Borrow's in the same lifetime.
'Borrows' is useful when you want to reborrow multiple borrows altogether.
-}
module Control.Monad.Borrow.Pure.Borrows (
  Borrows (..),
) where

import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Data.Coerce.Directed
import Data.Kind
import Prelude.Linear

type Borrows :: BorrowKind -> Lifetime -> [Type] -> Type
data Borrows bk α xs where
  BNil :: Borrows bk α '[]
  (:-) :: !(Borrow bk α x) %1 -> !(Borrows bk α xs) %1 -> Borrows bk α (x ': xs)

instance Affine (Borrows bk α xs) where
  aff = unsafeAff

deriving via AsAffine (Borrows bk α xs) instance Consumable (Borrows bk α xs)

instance (β <= α) => Borrows bk α xs <: Borrows bk' β xs where
  subtype = UnsafeSubtype

infixr 5 :-
