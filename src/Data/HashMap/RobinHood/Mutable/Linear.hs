{-# LANGUAGE NoImplicitPrelude #-}

{- |
A linearly owned mutable hash table, using Robin Hood hashing with
backward-shift deletion.

The table linearly owns its backing store, so it must be threaded through
@%1 ->@ and consumed exactly once; its keys and values, by contrast, are
GC-owned and bound nonlinearly. That is why 'lookup' hands back a plain
@'Ur' ('Maybe' v)@ with no 'Movable' constraint, and why duplicating a table
copies only the slot array. A table over linearly owned values would need a
different representation, and is not what this module provides.

Every operation here is an ordinary linear function rather than a 'BO' action.
To mutate a table in place through a borrow — the usual shape once a table is
a field of some larger structure — use
"Data.HashMap.RobinHood.Mutable.Linear.Borrow", which keeps this table behind
a linear 'Data.Ref.Linear.Ref'.

This module is intended to be imported qualified.
-}
module Data.HashMap.RobinHood.Mutable.Linear (
  HashMap,
  Hashable,

  -- * Construction
  new,
  fromList,

  -- * Mutation
  insert,
  insertMany,
  delete,
  alter,
  alterF,

  -- * Suspended insertion
  InsertPlan,
  lookupForInsert,
  unsafeInsertPrepared,

  -- * Query
  lookup,
  member,
  size,
  capacity,

  -- * Iteration
  foldMapWithKey,
  toList,

  -- * Combining maps
  union,
) where

import Data.HashMap.RobinHood.Mutable.Linear.Internal
import Data.Hashable (Hashable)
import Prelude ()
