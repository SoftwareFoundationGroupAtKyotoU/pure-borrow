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

{- |
Borrow-like values that can be narrowed to a sublifetime and restored afterwards.

=== The obligation every method carries

An implementation of 'locally'', 'locally' or 'locally_' must not hand the
caller back the occurrence it was given. It must return it through a barrier
the optimizer cannot see through and that consumes the 'BO' state token —
'Control.Monad.Borrow.Pure.BO.Unsafe.reviveAlias' for a scalar borrow,
'Control.Monad.Borrow.Pure.Experimental.Borrows.reviveAliases' for a bundle.

This is not a performance convention. Reads that project a mutable header do
not go through the state token, so to GHC two of them on the same borrow
/variable/ are the same expression, and common-subexpression elimination is
entitled to serve the second from the first — across every write the scope
performed. A delimiter that returns its caller's own binder makes a post-scope
read syntactically identical to a pre-scope one; the result is a stale length
and a stale buffer, and writing through them runs off the end of the
allocation. See @Note [Restoring a borrow must break its Core identity]@ in
"Control.Monad.Borrow.Pure.BO.Internal" for the full argument, and treat it as
binding on any instance you write.

Each method is separately overridable, so each one owes this independently:
supplying a fast 'locally' while leaving 'locally'' to the default does not
discharge it for 'locally'.
-}
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

  The consumption of the result sits /in/ the returned value rather than being
  sequenced at scope exit, so it runs when the caller forces the restored
  borrow. That is deliberate: sequencing it at exit would make this stricter
  than the implementation @+slow@ restores, and the two are required to stay
  observationally equivalent. Linearity gives the restored borrow exactly one
  holder, so any use of it forces the consumption exactly once and first.
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
