{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Trusted representation of the borrow-aware Robin Hood hash table.
module Data.HashMap.RobinHood.Mutable.Linear.Borrow.Internal (
  module Data.HashMap.RobinHood.Mutable.Linear.Borrow.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Utils (coerceLin, unsafeLeak)
import Data.HashMap.RobinHood.Mutable.Linear qualified as Raw
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import GHC.TypeError
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

{- | A borrow-aware Robin Hood hash table.

The owned table of "Data.HashMap.RobinHood.Mutable.Linear" replaces its own
backing array when it grows, so a mutation returns a table value rather than
writing through the old one. Threading that value back is impossible through a
borrow, which is why this wrapper keeps the table behind a linear 'Ref': a
growth updates the reference in place, and a @'Mut' α@ of an enclosing
structure sees the new array without the enclosing structure being rebuilt.

Its keys and values are GC-owned, exactly as the underlying table's are.
-}
newtype HashMap k v = HashMap (Ref (Raw.HashMap k v))
  deriving newtype (LinearOnly, Consumable, Dupable, Clone)

-- | A 'Raw.InsertPlan' suspended against a borrowed table.
newtype InsertPlan k = InsertPlan (Raw.InsertPlan k)

{- | A 'HashMap' cannot be 'Copyable', because it owns a mutable reference.

Cloning one is still possible inside 'BO', through the derived 'Clone'.
-}
instance
  (Unsatisfiable (ShowType (HashMap k v) :<>: Text " cannot be copied!")) =>
  Copyable (HashMap k v)
  where
  copy = unsatisfiable

-- | Release the reference of an owned table.
inner :: HashMap k v %1 -> Ref (Raw.HashMap k v)
{-# INLINE inner #-}
inner = coerceLin

coerceBor ::
  forall k v bk α.
  Borrow bk α (HashMap k v) %1 ->
  Borrow bk α (Ref (Raw.HashMap k v))
{-# INLINE coerceBor #-}
coerceBor = coerceLin

recoerceBor ::
  forall k v bk α.
  Borrow bk α (Ref (Raw.HashMap k v)) %1 ->
  Borrow bk α (HashMap k v)
{-# INLINE recoerceBor #-}
recoerceBor = coerceLin

{-
Note [Reading through a borrow leaks an alias]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'askRaw' and 'askRawUr' hand a query the table that the 'Ref' still owns, so
what the query threads back out is an alias, not a resource this scope may
release. Both therefore drop it with 'unsafeLeak' rather than treating it as
owned, and hand the caller back the borrow they came in with.

Dropping it is a genuine no-op rather than a leak. The table's keys and values
are GC-owned (see Note [Element ownership] in
"Data.HashMap.RobinHood.Mutable.Linear.Internal"), so consuming a table
bottoms out in dropping a reference to its backing array -- which the 'Ref'
still holds, and which the garbage collector reclaims once the lender frees
the whole structure.

Returning the input borrow unchanged is sound for the same reason it is in
'Data.Vector.Mutable.Growable.Linear.Borrow.size': the occurrence handed back
is the one that was consumed, so no second live borrow of the table is
created, and a mutable caller still holds exactly one.

Neither helper may be used with a query that *replaces* the backing array:
a growth would be written into an alias and then discarded, and the 'Ref'
would keep serving the old array. Every mutating operation goes through
'Ref.update' or 'Ref.modify' instead, which write the returned table back.
-}

{- | Run a table query that threads the table through, and return the borrow.

The query must not replace the table's backing array. See
Note [Reading through a borrow leaks an alias].
-}
askRaw ::
  (Raw.HashMap k v %1 -> (a, Raw.HashMap k v)) %1 ->
  Borrow bk α (HashMap k v) %1 ->
  BO α (a, Borrow bk α (HashMap k v))
{-# INLINE askRaw #-}
askRaw = Unsafe.toLinear2 \f borrow ->
  case borrow of
    UnsafeAlias (HashMap ref) ->
      case Ref.unsafeReadRef ref of
        (!raw, _) -> case f raw of
          (!res, !raw) -> unsafeLeak raw `lseq` Control.pure (res, borrow)

{- | Run a table query that consumes the table and materializes its result.

The query must not replace the table's backing array. See
Note [Reading through a borrow leaks an alias].
-}
askRawUr ::
  (Raw.HashMap k v %1 -> Ur a) %1 ->
  Borrow bk α (HashMap k v) %1 ->
  BO α (Ur a, Borrow bk α (HashMap k v))
{-# INLINE askRawUr #-}
askRawUr = Unsafe.toLinear2 \f borrow ->
  case borrow of
    UnsafeAlias (HashMap ref) ->
      case Ref.unsafeReadRef ref of
        (!raw, _) -> case f raw of
          Ur !res -> Control.pure (Ur res, borrow)

{- | Force a queried value to WHNF.

A mutation reports the value it displaced, and that value may be a thunk
reading the table it was displaced from. Forcing it before the mutation
returns keeps the thunk from being evaluated against a later state of the
table.
-}
forceMay :: Maybe a %1 -> Maybe a
{-# INLINE forceMay #-}
forceMay Nothing = Nothing
forceMay (Just !x) = Just x
