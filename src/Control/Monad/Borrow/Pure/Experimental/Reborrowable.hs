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
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (reviveAlias)
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

  {- |
  The non-finalizing form, whose continuation returns its result directly.

  This is a method rather than a function over 'locally'' so that an instance
  can supply a delimiter that never builds an 'After' at all. The default is
  the composition it replaces, so an existing instance keeps working and keeps
  its current cost; @'Mut'@, @'Share'@ and
  @'Control.Monad.Borrow.Pure.Experimental.Borrows.Muts'@ override it.
  -}
  locally ::
    bor a %1 ->
    (forall β. WithLifetime bor (β /\ LifetimeOf bor) a %1 -> BO (β /\ α') r) %1 ->
    BO α' (r, bor a)
  locally bor k = locally' bor \bor -> Control.pure Control.<$> k bor
  {-# INLINE locally #-}

  {- |
  The result-discarding form.

  The result is consumed strictly before the restored borrow is returned, so a
  lazily consumed result cannot overlap access through it.
  -}
  locally_ ::
    (Consumable r) =>
    bor a %1 ->
    (forall β. WithLifetime bor (β /\ LifetimeOf bor) a %1 -> BO (β /\ α') r) %1 ->
    BO α' (bor a)
  locally_ bor k = uncurry lseq Control.<$> locally bor k
  {-# INLINE locally_ #-}

instance Reborrowable (Mut α) where
  type LifetimeOf (Mut α) = α
  type WithLifetime (Mut α) β = Mut β
  {-# SPECIALIZE instance Reborrowable (Mut α) #-}
  locally' = reborrowing'
  {-# INLINE locally' #-}
  locally = reborrowing
  {-# INLINE locally #-}
  locally_ = reborrowing_
  {-# INLINE locally_ #-}

instance Reborrowable (Share α) where
  type LifetimeOf (Share α) = α
  type WithLifetime (Share α) β = Share β
  {-# SPECIALIZE instance Reborrowable (Share α) #-}

  -- 'move' for a shared borrow is the identity, so @sh@ is the caller's own occurrence and returning it directly would carry the defect in Note [Restoring a borrow must break its Core identity].
  -- Hand it back through 'reviveAlias' as the scalar delimiters do.
  locally' shr k = Control.do
    let %1 !(Ur sh) = move shr
    r <- srunBO (k (upcast sh))
    (r,) Control.<$> reviveAlias sh
  {-# INLINE locally' #-}

  -- The same, through the non-finalizing 'srunBO_', so that a continuation
  -- which returns its result directly never builds an 'After' to discharge.
  locally shr k = Control.do
    let %1 !(Ur sh) = move shr
    r <- srunBO_ (k (upcast sh))
    (r,) Control.<$> reviveAlias sh
  {-# INLINE locally #-}
