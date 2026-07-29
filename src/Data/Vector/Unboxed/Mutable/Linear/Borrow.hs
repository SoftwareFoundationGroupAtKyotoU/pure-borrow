{-# LANGUAGE NoImplicitPrelude #-}

{- |
An unboxed variant of "Data.Vector.Mutable.Linear.Borrow".
-}
module Data.Vector.Unboxed.Mutable.Linear.Borrow (
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
  update,
  unsafeUpdate,
  modify,
  swap,
  unsafeSwap,
  splitAt,
) where

import Data.Vector.Unboxed.Mutable.Linear.Borrow.Internal
