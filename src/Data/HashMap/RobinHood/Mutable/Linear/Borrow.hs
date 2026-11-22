{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
A borrow-aware Robin Hood hash table, using backward-shift deletion, to be mutated as @'Mut' α ('HashMap' k v)@.

Its mutations and queries are 'BO' actions on a borrow of the table.
Keys and values are GC-owned, so a lookup returns @'Ur' ('Maybe' v)@ rather than a borrow of the stored value.

This module is intended to be imported qualified.

= Colliding keys #collisions#

A table holds at most 128 keys with the same hash, and fewer when other keys collide with them, as happens where many hashes are each shared by dozens of keys.
An insertion raises an error, rather than lose an entry, when the table cannot hold the keys it would then have: more than 128 keys of one hash, dozens of keys on each of many hashes, or keys chosen to collide by someone who knows their 'Hashable' instance.
Sizing a table generously with 'empty' does not let it hold more colliding keys.
Keys whose hashes mostly differ practically never raise the error.
If yours do, give the key type a 'Hashable' instance under which fewer keys share a hash.
Nothing can catch the error and go on using the table, so do not fill one with keys that an adversary chooses where losing the computation is not acceptable.
-}
module Data.HashMap.RobinHood.Mutable.Linear.Borrow (
  HashMap,
  Hashable,

  -- * Construction
  empty,
  fromList,

  -- * Mutation
  insert,
  delete,
  alter,
  alterF,

  -- * Query
  size,
  lookup,
  member,

  -- * Iteration
  toList,

  -- * Bulk operations
  swap,
  take,
  take_,
  union,
  extend,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Bifunctor.Linear qualified as Bi
import Data.Functor.Linear qualified as Data
import Data.HashMap.RobinHood.Mutable.Linear.Borrow.Internal
import Data.HashMap.RobinHood.Mutable.Linear.Internal qualified as Raw
import Data.Hashable (Hashable)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as Ref
import Prelude.Linear hiding (insert, lookup, take)

-- * Construction

-- | \(O(n)\). An empty table sized for at least the given number of entries.
empty :: forall k v. Int -> Linearly %1 -> HashMap k v
{-# INLINE empty #-}
empty size l =
  dup l & \(tableLinear, refLinear) ->
    HashMap (Ref.new (Raw.new size tableLinear) refLinear)

{- | \(O(n)\) amortized. Build a table from a list, later keys winning.

It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
fromList :: (Hashable k) => [(k, v)] -> Linearly %1 -> HashMap k v
{-# INLINE fromList #-}
fromList dic l =
  dup l & \(tableLinear, refLinear) ->
    HashMap (Ref.new (Raw.fromList dic tableLinear) refLinear)

-- * Mutation

{- | \(O(1)\) amortized. Insert an entry, returning the value it displaced.

It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
insert ::
  (Hashable k) =>
  k ->
  v ->
  Mut α (HashMap k v) %1 ->
  BO α (Ur (Maybe v), Mut α (HashMap k v))
{-# INLINE insert #-}
insert key !v !dic = Control.do
  (Ur mval, dic) <-
    Ref.update
      (\dic -> Control.pure $ Raw.insert key v dic)
      (coerceBor dic)
  Control.pure (Ur $ forceMay mval, recoerceBor dic)

-- | \(O(1)\) amortized. Remove a key, returning the value it held.
delete ::
  (Hashable k) =>
  k ->
  Mut α (HashMap k v) %1 ->
  BO α (Ur (Maybe v), Mut α (HashMap k v))
{-# INLINE delete #-}
delete key dic = Control.do
  (Ur mval, dic) <-
    Ref.update
      (\dic -> Control.pure $ Raw.delete key dic)
      (coerceBor dic)
  Control.pure (Ur $ forceMay mval, recoerceBor dic)

{- | \(O(1)\) amortized. Insert, update or delete the entry at a key.

It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
alter ::
  (Hashable k) =>
  (Maybe v -> Maybe v) ->
  k ->
  Mut α (HashMap k v) %1 ->
  BO α (Mut α (HashMap k v))
{-# INLINE alter #-}
alter f k =
  Control.fmap recoerceBor
    . Ref.modify (Raw.alter f k)
    . coerceBor

{- | \(O(1)\) amortized. 'alter' with the replacement produced in 'BO'.

It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
alterF ::
  (Hashable k) =>
  (Maybe v -> BO α (Ur (Maybe v))) ->
  k ->
  Mut α (HashMap k v) %1 ->
  BO α (Mut α (HashMap k v))
{-# INLINE alterF #-}
alterF f key dic = Control.do
  ((), dic) <-
    Ref.update
      ( Control.fmap ((),)
          . Raw.alterF (\ !may -> Data.fmap forceMay Control.<$> f (forceMay may)) key
      )
      (coerceBor dic)
  Control.pure $ recoerceBor dic

-- * Query

{-
Every query below consumes one occurrence of the borrow and hands the same
occurrence back, exactly as 'Data.Vector.Mutable.Linear.Borrow.size' does. A
shared caller may ignore the returned borrow; a mutable one threads it on, so
that a sequence of queries against a single @'Mut' α@ needs no reborrowing.
-}

-- | \(O(1)\). The number of live entries.
size ::
  Borrow bk α (HashMap k v) %1 ->
  BO α (Ur Int, Borrow bk α (HashMap k v))
{-# INLINE size #-}
size = askRaw Raw.size

-- | \(O(1)\) amortized. The value stored at a key, if any.
lookup ::
  (Hashable k) =>
  k ->
  Borrow bk α (HashMap k v) %1 ->
  BO α (Ur (Maybe v), Borrow bk α (HashMap k v))
{-# INLINE lookup #-}
lookup !key !dic = askRaw (Raw.lookup key) dic

-- | \(O(1)\) amortized. Whether a key is present.
member ::
  (Hashable k) =>
  k ->
  Borrow bk α (HashMap k v) %1 ->
  BO α (Ur Bool, Borrow bk α (HashMap k v))
{-# INLINE member #-}
member key = askRaw (Raw.member key)

-- * Iteration

-- | \(O(n)\). The table's entries, in unspecified order.
toList ::
  Borrow bk α (HashMap k v) %1 ->
  BO α (Ur [(k, v)], Borrow bk α (HashMap k v))
{-# INLINE toList #-}
toList = askRawUr Raw.toList

-- * Bulk operations

-- | \(O(1)\). Replace a borrowed table with another, returning the old one.
swap ::
  forall k v α.
  HashMap k v %1 ->
  Mut α (HashMap k v) %1 ->
  BO α (HashMap k v, Mut α (HashMap k v))
{-# INLINE swap #-}
swap new dic = asksLinearlyM \lin -> Control.do
  Bi.second recoerceBor
    Control.<$> Ref.update
      (\ !old -> Control.pure (HashMap $ Ref.new old lin, Ref.free $ inner new))
      (coerceBor dic)

-- | \(O(1)\). Take every entry out of a borrowed table, leaving it empty.
take :: forall k v α. Mut α (HashMap k v) %1 -> BO α (HashMap k v, Mut α (HashMap k v))
take dic = Control.do
  Bi.second recoerceBor Control.<$> Ref.update go (coerceBor dic)
  where
    go :: Raw.HashMap k v %1 -> BO α (HashMap k v, Raw.HashMap k v)
    go s = asksLinearlyM \lin ->
      dup lin & \(refLinear, tableLinear) ->
        Control.pure (HashMap $! Ref.new s refLinear, Raw.new 16 tableLinear)

-- | \(O(1)\). A borrow-discarding variant of 'take'.
take_ :: forall k v α. Mut α (HashMap k v) %1 -> BO α (HashMap k v)
{-# INLINE take_ #-}
take_ dic = Control.fmap (uncurry $ flip lseq) $ take dic

{- | \(O(n)\) amortized, for \(n\) the capacity of the table with fewer entries. Union of two tables.

A key present in both takes the second table's value, as if the second table's entries were inserted into the first.
linear-base's @Data.HashMap.Mutable.Linear.union@ has the same bias, while @Data.Map.union@ and @Data.HashMap.Strict.union@ keep the first map's value.
It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
union :: (Hashable k) => HashMap k v %1 -> HashMap k v %1 -> HashMap k v
{-# INLINE union #-}
union (HashMap ref1) (HashMap ref2) = DataFlow.do
  (l, ref1) <- withLinearly ref1
  HashMap $! Ref.new (Raw.extend (Ref.free ref1) (Ref.free ref2)) l

{- | \(O(n)\) amortized, for \(n\) the capacity of the table with fewer entries. Insert every entry of a table into a borrowed one.

A key present in both takes the inserted table's value, as with 'insert'.
It raises an error on [too many colliding keys]("Data.HashMap.RobinHood.Mutable.Linear.Borrow#collisions").
-}
extend :: (Hashable k) => HashMap k v %1 -> Mut α (HashMap k v) %1 -> BO α (Mut α (HashMap k v))
{-# INLINE extend #-}
extend donor dic = Control.do
  let %1 !donor' = Ref.free (inner donor)
  !dic <- Ref.modify (\ !s -> Raw.extend s donor') $ coerceBor dic
  Control.pure $! recoerceBor dic
