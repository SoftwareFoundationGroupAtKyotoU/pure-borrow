{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
The module provides 'Borrows', which is a heterogeneous list of 'Borrow's in the same lifetime.
-}
module Control.Monad.Borrow.Pure.Experimental.Borrows (
  Borrows (..),
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Experimental.Reborrowable
import Data.Coerce.Directed.Unsafe
import Data.Kind
import Prelude.Linear hiding (foldMap)
import Unsafe.Linear qualified as Unsafe

type Borrows :: BorrowKind -> Lifetime -> [Type] -> Type
data Borrows bk α xs where
  BNil :: Borrows bk α '[]
  (:-) :: !(Borrow bk α x) %1 -> !(Borrows bk α xs) %1 -> Borrows bk α (x ': xs)

infixr 5 :-

instance Affine (Borrows bk α xs) where
  aff = unsafeAff

deriving via AsAffine (Borrows bk α xs) instance Consumable (Borrows bk α xs)

instance (β <= α) => Borrows bk α xs <: Borrows bk' β xs where
  subtype = UnsafeSubtype

instance Reborrowable (Borrows bk) where
  locally' = Unsafe.toLinear \bors k -> Control.do
    (,bors) Control.<$> srunBO (k $ upcast bors)
  {-# INLINE locally' #-}
