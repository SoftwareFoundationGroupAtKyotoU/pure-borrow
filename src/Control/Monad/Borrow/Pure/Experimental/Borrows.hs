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
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
The module provides 'Aliases', which is a heterogeneous list of 'Alias'es in the same lifetime.
-}
module Control.Monad.Borrow.Pure.Experimental.Borrows (
  Aliases (..),
  Muts,
  Shares,
  Borrows,
  Lends,
  reborrows,
  reborrowings',
  reborrowings,
  reborrowings_,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad qualified as NonLinear
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Internal
import Control.Monad.Borrow.Pure.Experimental.Reborrowable
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce.Directed.Unsafe
import Data.Kind
import Prelude.Linear hiding (foldMap)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Linear qualified as Unsafe

type Aliases :: AliasKind -> [Type] -> Type
data Aliases k xs where
  BNil :: Aliases k '[]
  (:-) :: !(Alias k x) %1 -> !(Aliases k xs) %1 -> Aliases k (x ': xs)

type role Aliases nominal nominal

infixr 5 :-

type Lends :: Lifetime -> [Type] -> Type
type Lends α = Aliases ('Lend α)

type Borrows :: BorrowKind -> Lifetime -> [Type] -> Type
type Borrows bk α = Aliases ('Borrow bk α)

type Muts :: Lifetime -> [Type] -> Type
type Muts α = Borrows 'Mut α

type Shares :: Lifetime -> [Type] -> Type
type Shares α = Borrows 'Share α

instance Affine (Aliases α xs) where
  aff = unsafeAff
  {-# INLINE aff #-}

deriving via
  AsAffine (Aliases k xs)
  instance
    (k ~ 'Borrow bk α) =>
    Consumable (Aliases k xs)

instance (k ~ 'Borrow 'Share α) => Dupable (Aliases k xs) where
  dup2 = Unsafe.toLinear $ NonLinear.join (,)
  {-# INLINE dup2 #-}

instance (k ~ 'Borrow 'Share α) => Movable (Aliases k xs) where
  move = Unsafe.toLinear Ur
  {-# INLINE move #-}

instance (α >= β, xs <: ys, ys <: xs) => Muts α xs <: Muts β ys where
  subtype = UnsafeSubtype

instance (α >= β, xs <: ys) => Shares α xs <: Shares β ys where
  subtype = UnsafeSubtype

instance (α <= β, a <: b) => Lends α a <: Lends β b where
  subtype = UnsafeSubtype

instance Reborrowable (Muts α) where
  type LifetimeOf (Muts α) = α
  type WithLifetime (Muts α) β = Muts β
  locally' = reborrowings'
  {-# INLINE locally' #-}

-- | A plural form of 'reborrow', which reborrows multiple borrows in the given 'Muts' at once.
reborrows :: forall β α a. (α >= β) => Muts α a %1 -> (Muts β a, Lend β (Muts α a))
reborrows = Unsafe.toLinear \v -> (unsafeCoerce v, unsafeCoerce v)

-- | A plural form of 'reborrowing''.
reborrowings' ::
  Muts α a %1 ->
  (forall β. Muts (β /\ α) a %1 -> BO (β /\ α') (After β r)) %1 ->
  BO α' (r, Muts α a)
{-# INLINE reborrowings' #-}
reborrowings' v k = srunBO DataFlow.do
  (v, lend) <- reborrows v
  Control.do
    v <- k v
    Control.pure $ (,) Control.<$> v Control.<*> upcast (reclaim' lend)

reborrowings ::
  Muts α a %1 ->
  (forall β. Muts (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, Muts α a)
{-# INLINE reborrowings #-}
reborrowings mutα k = reborrowings' mutα (\mut -> Control.pure Control.<$> k mut)

reborrowings_ ::
  (Consumable r) =>
  Muts α a %1 ->
  (forall β. Muts (β /\ α) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (Muts α a)
{-# INLINE reborrowings_ #-}
reborrowings_ mutα k = reborrowings mutα (Control.fmap consume . k) Control.<&> \((), a) -> a
