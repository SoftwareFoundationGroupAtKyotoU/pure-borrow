{-# LANGUAGE NoImplicitPrelude #-}

{- |
A linearly owned hash table, using Robin Hood hashing, that is built once with 'fromList' and then only queried.

The table linearly owns its backing store, so it must be threaded through @%1 ->@ and consumed exactly once; its keys and values, by contrast, are GC-owned and bound nonlinearly.
That is why 'lookup' hands back a plain @'Ur' ('Maybe' v)@ with no 'Movable' constraint.

A table that is to change after it is built must come from "Data.HashMap.RobinHood.Mutable.Linear.Borrow", whose hash map is a different type, mutated by 'BO' actions on a borrow of it.

This module is intended to be imported qualified.

= Colliding keys #collisions#

A table holds at most 128 keys with the same hash, and fewer when other keys collide with them, as happens where many hashes are each shared by dozens of keys.
'fromList' raises an error, rather than lose an entry, when a table cannot hold the keys of the list: more than 128 keys of one hash, dozens of keys on each of many hashes, or keys chosen to collide by someone who knows their 'Hashable' instance.
Keys whose hashes mostly differ practically never raise it.
If yours do, give the key type a 'Hashable' instance under which fewer keys share a hash.
Do not build a table from keys that an adversary chooses where failing is not acceptable.
-}
module Data.HashMap.RobinHood.Mutable.Linear (
  HashMap,
  Hashable,

  -- * Construction
  fromList,

  -- * Query
  lookup,
  member,
  size,
  capacity,

  -- * Iteration
  foldMapWithKey,
  toList,
) where

import Data.HashMap.RobinHood.Mutable.Linear.Internal
import Data.Hashable (Hashable)
import Prelude ()
