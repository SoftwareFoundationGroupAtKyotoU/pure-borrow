{-# LANGUAGE NoImplicitPrelude #-}

{- |
Linearly owned mutable vectors whose elements are unrestricted and GC-owned.

The public backend parameter @v@ selects any immutable backend supported by
@vector@'s generic interface. The owner and its mutable backing remain linear.

Custom backends are a trusted extensibility boundary: their mutable-vector
operations must obey the usual @vector@ laws, including fresh allocation and
cloning, exact indexing, disjoint non-overlapping splits, copying thaw, and
alias-free ownership transfer through unsafe freeze/thaw. The standard boxed,
unboxed, and primitive backends satisfy these requirements.
-}
module Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted (
  Vector,
  empty,
  constant,
  fromList,
  fromVector,
  unsafeFromVector,
  unsafeFromMutable,
  toVector,
  toList,
  copyToVector,
  size,
  get,
  unsafeGet,
  head,
  unsafeHead,
  last,
  unsafeLast,
  copyAt,
  copyAtMut,
  set,
  unsafeSet,
  write,
  unsafeWrite,
  update,
  unsafeUpdate,
  modify,
  swap,
  unsafeSwap,
  splitAt,
) where

import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.Internal
