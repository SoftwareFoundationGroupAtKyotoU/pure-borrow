{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Growable, linearly owned mutable vectors with unrestricted, GC-owned elements
and a backend selected through @vector@'s generic interface.

A 'GrowableVector' keeps its replaceable backing buffer behind a stable mutable
header. Consequently, 'reserve', 'push', and 'extend' preserve the identity
reclaimed by an existing @Lend@. The backend parameter @v@ may be any immutable
backend supported by 'Data.Vector.Generic.Vector'.

'getContents' projects a mutable or shared growable borrow to the corresponding
fixed-size unrestricted vector borrow over exactly the initialized prefix.
'withContent' provides the same projection in a rank-2 no-growth scope and
restores the growable borrow afterward. Growth remains unavailable until every
fixed content borrow has ended.

The owner and mutable backing are linear, but entries are not element-owned.
Constructing, growing, cloning, discarding, and materializing a vector therefore
do not invoke @Consumable@, @Movable@, @Copyable@, or @Dupable@ operations on
elements. 'toVector' freezes the initialized prefix in \(O(1)\).

The growable owner deliberately has no splitting operation because independent
replaceable headers over overlapping storage would invalidate borrows after
growth. Split only a fixed view inside 'withContent'.

Growth provides a normal-return ownership guarantee. Allocation and copying
complete before the logical size is published; an owner is not promised to be
recoverable after an exception escapes a @BO@ computation.

Custom backends are a trusted extensibility boundary. Their mutable operations
must obey the usual @vector@ laws, including fresh allocation, exact lengths,
non-overlapping storage, copying thaw and freeze, and alias-free ownership
transfer through unsafe freeze and thaw.
-}
module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted (
  GrowableVector,
  empty,
  constant,
  fromList,
  withCapacity,
  fromVector,
  unsafeFromMutable,
  unsafeFromVector,
  toVector,
  toList,
  copyToVector,
  size,
  capacity,
  get,
  unsafeGet,
  head,
  unsafeHead,
  last,
  unsafeLast,
  copyAt,
  unsafeCopyAt,
  copyAtMut,
  unsafeCopyAtMut,
  set,
  unsafeSet,
  write,
  unsafeWrite,
  update,
  unsafeUpdate,
  modify,
  swap,
  unsafeSwap,
  reserve,
  reserveAdditional,
  push,
  extend,
  getContents,
  withContent,
  withContent_,
) where

import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.Internal
