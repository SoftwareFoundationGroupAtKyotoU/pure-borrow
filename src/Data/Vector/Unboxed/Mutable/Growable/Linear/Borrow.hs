{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
A growable unboxed variant of
"Data.Vector.Mutable.Growable.Linear.Borrow".

As there, every operation that reads the header -- 'size', 'capacity', 'getContents' and the element accessors alike -- is a @BO@ action.
-}
module Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow (
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
  size,
  capacity,
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

import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow.Internal
