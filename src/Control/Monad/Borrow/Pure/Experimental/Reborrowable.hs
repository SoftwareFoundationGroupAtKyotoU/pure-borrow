{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure.Experimental.Reborrowable (
  Reborrowable (..),
  locally,
  locally_,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Data.Kind (Constraint, Type)
import Prelude.Linear

type Reborrowable :: (k -> Type) -> Constraint
class (bor ~ WithLifetime bor (LifetimeOf bor)) => Reborrowable bor where
  type LifetimeOf bor :: Lifetime
  type WithLifetime bor (α :: Lifetime) :: k -> Type

  {- |
  Executes an operation on a borrow in sub lifetime.
  You may need @-XImpredicativeTypes@ extension to use this function.

  Generalization of 'reborrowing'' and 'sharing'' that works for both 'Mut' and 'Share' borrows.
  -}
  locally' ::
    bor a %1 ->
    (forall β. WithLifetime bor (β /\ LifetimeOf bor) a %1 -> BO (β /\ α') (After β r)) %1 ->
    BO α' (r, bor a)

instance Reborrowable (Mut α) where
  type LifetimeOf (Mut α) = α
  type WithLifetime (Mut α) β = Mut β
  {-# SPECIALIZE instance Reborrowable (Mut α) #-}
  locally' = reborrowing'
  {-# INLINE locally' #-}

instance Reborrowable (Share α) where
  type LifetimeOf (Share α) = α
  type WithLifetime (Share α) β = Share β
  {-# SPECIALIZE instance Reborrowable (Share α) #-}
  locally' shr k = Control.do
    let %1 !(Ur sh) = move shr
    (,sh) Control.<$> srunBO (k (upcast sh))
  {-# INLINE locally' #-}

locally ::
  (Reborrowable bor) =>
  bor a %1 ->
  (forall β. WithLifetime bor (β /\ LifetimeOf bor) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (r, bor a)
{-# INLINE locally #-}
locally bor k = locally' bor \mut -> Control.pure Control.<$> k mut

locally_ ::
  (Reborrowable bor, Consumable r) =>
  bor a %1 ->
  (forall β. WithLifetime bor (β /\ LifetimeOf bor) a %1 -> BO (β /\ α') r) %1 ->
  BO α' (bor a)
{-# INLINE locally_ #-}
locally_ bor k = uncurry lseq Control.<$> locally bor k
