{-# LANGUAGE NoImplicitPrelude #-}

{- |
This module provides __unsafe__ operations of "Data.HashMap.RobinHood.Mutable.Linear.Borrow".
These are not meant to be used by end-users, so generally YOU SHOULD NOT import this module, and import "Data.HashMap.RobinHood.Mutable.Linear.Borrow" instead.
Its @alter@ and @alterF@ already insert at a missing key with a single search.

This module is meant for library authors who want to build a new API on top of Pure Borrow.
It provides an insertion that searches for its key only once, in two steps: 'lookupForInsert' looks the key up and, on a miss, returns a plan, and 'unsafeInsertPrepared' inserts the key from that plan.
Nothing checks that a plan is used with the table it was made for, or that the table has not changed since, and a plan used otherwise corrupts the table and can read and write memory outside it.
-}
module Data.HashMap.RobinHood.Mutable.Linear.Borrow.Unsafe (
  -- * Suspended insertion
  InsertPlan,
  lookupForInsert,
  unsafeInsertPrepared,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Data.HashMap.RobinHood.Mutable.Linear.Borrow.Internal
import Data.HashMap.RobinHood.Mutable.Linear.Internal qualified as Raw
import Data.Hashable (Hashable)
import Data.Ref.Linear.Borrow qualified as Ref
import Prelude.Linear
import Prelude qualified as NonLinear

{- | \(O(1)\) amortized. Look a key up: a hit returns its value in 'Left', and a miss returns in 'Right' a plan for 'unsafeInsertPrepared' to insert the key without searching again.

The plan is good for one insertion into this table, before anything else changes the table.
-}
lookupForInsert ::
  forall k v bk α.
  (Hashable k) =>
  k ->
  Borrow bk α (HashMap k v) %1 ->
  BO α (Ur (Either v (InsertPlan k)), Borrow bk α (HashMap k v))
{-# INLINE lookupForInsert #-}
lookupForInsert key = askRaw go
  where
    go :: Raw.HashMap k v %1 -> (Ur (Either v (InsertPlan k)), Raw.HashMap k v)
    go hm = case Raw.lookupForInsert key hm of
      (Ur result, hm') -> (Ur (NonLinear.fmap InsertPlan result), hm')

{- | \(O(1)\) amortized. Insert the key of a plan from 'lookupForInsert' with the given value, without searching for the key again.

The table must be the one that the plan was made for, and between that 'lookupForInsert' and this call it may only have been read.
No insertion, deletion or alteration may have changed it, including one with this plan or another one, and neither may the @take@, @take_@, @swap@ or @extend@ of "Data.HashMap.RobinHood.Mutable.Linear.Borrow".
Otherwise the insertion corrupts the table and can read and write memory outside it.
Like the @insert@ of "Data.HashMap.RobinHood.Mutable.Linear.Borrow", it raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
unsafeInsertPrepared ::
  InsertPlan k ->
  v ->
  Mut α (HashMap k v) %1 ->
  BO α (Mut α (HashMap k v))
{-# INLINE unsafeInsertPrepared #-}
unsafeInsertPrepared (InsertPlan plan) !v =
  Control.fmap recoerceBor
    . Ref.modify (Raw.unsafeInsertPrepared plan v)
    . coerceBor
