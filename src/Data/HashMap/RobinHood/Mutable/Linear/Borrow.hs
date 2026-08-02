{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
A borrow-aware Robin Hood hash table, to be mutated as @'Mut' α ('HashMap' k v)@.

This is "Data.HashMap.RobinHood.Mutable.Linear" behind a linear
'Data.Ref.Linear.Ref'. The indirection is what makes the table usable through
a borrow at all: the owned table replaces its backing array when it grows, so
a mutation produces a new table value, and through a borrow there is nowhere
to thread that value back to. Writing it into the reference instead means a
growth is visible to every enclosing borrow without the enclosing structure
being rebuilt.

Keys and values are GC-owned, as in the underlying table. Consequently a
lookup returns @'Ur' ('Maybe' v)@ rather than a borrow of the stored value,
and duplicating a table copies only its slot array.

This module is intended to be imported qualified.
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

  -- * Suspended insertion
  InsertPlan,
  lookupForInsert,
  unsafeInsertPrepared,

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
import Data.HashMap.RobinHood.Mutable.Linear (Hashable)
import Data.HashMap.RobinHood.Mutable.Linear qualified as Raw
import Data.HashMap.RobinHood.Mutable.Linear.Borrow.Internal
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as Ref
import Prelude.Linear hiding (insert, lookup, take)
import Prelude qualified as NonLinear

-- * Construction

-- | \(O(n)\). An empty table sized for at least the given number of entries.
empty :: forall k v. Int -> Linearly %1 -> HashMap k v
{-# INLINE empty #-}
empty size l =
  dup l & \(tableLinear, refLinear) ->
    HashMap (Ref.new (Raw.new size tableLinear) refLinear)

-- | \(O(n)\) amortized. Build a table from a list, later keys winning.
fromList :: (Hashable k) => [(k, v)] -> Linearly %1 -> HashMap k v
{-# INLINE fromList #-}
fromList dic l =
  dup l & \(tableLinear, refLinear) ->
    HashMap (Ref.new (Raw.fromList dic tableLinear) refLinear)

-- * Mutation

-- | \(O(1)\) amortized. Insert an entry, returning the value it displaced.
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

-- | \(O(1)\) amortized. Insert, update or delete the entry at a key.
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

-- | \(O(1)\) amortized. 'alter' with the replacement produced in 'BO'.
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

-- * Suspended insertion

{- | \(O(1)\) amortized. Look a key up, and on a miss suspend the probe.

Resume the returned plan with 'unsafeInsertPrepared' to insert without a
second traversal.
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
      (Ur result, hm) -> (Ur (NonLinear.fmap InsertPlan result), hm)

{- | \(O(1)\) amortized. Resume an unsuccessful 'lookupForInsert' as an insertion.

The table must not have been mutated since the plan was produced.
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

{- | \(O(n)\) amortized. Union of two owned tables.

The smaller table is inserted into the larger, so a key present in both takes
the value from the table that is inserted second.
-}
union :: (Hashable k) => HashMap k v %1 -> HashMap k v %1 -> HashMap k v
{-# INLINE union #-}
union (HashMap ref1) (HashMap ref2) = DataFlow.do
  (l, ref1) <- withLinearly ref1
  HashMap $! Ref.new (Raw.union (Ref.free ref1) (Ref.free ref2)) l

-- | \(O(n)\) amortized. Insert every entry of an owned table into a borrowed one.
extend :: (Hashable k) => HashMap k v %1 -> Mut α (HashMap k v) %1 -> BO α (Mut α (HashMap k v))
{-# INLINE extend #-}
extend donor dic = Control.do
  let %1 !donor' = Ref.free (inner donor)
  !dic <- Ref.modify (\ !s -> Raw.union s donor') $ coerceBor dic
  Control.pure $! recoerceBor dic
