{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Growable, linearly owned boxed vectors with a stable outer identity.

A 'GrowableVector' keeps its replaceable backing buffer behind a mutable
header, so 'reserve', 'push', and 'extend' preserve the identity
reclaimed by an existing @Lend@. 'getContents' opens that header once and
converts a mutable or shared growable borrow into the corresponding fixed-size
borrow of exactly the initialized logical prefix. The fixed view cannot
reserve, grow, freeze, consume the growable owner, or expose spare capacity.

'withContent' combines this constant-time projection with a short borrow scope
that preserves the input borrow kind. Its callback receives one linear
occurrence; shared content can use @move@ to recover unrestricted use. A mutable
growable borrow is unavailable while the callback runs and is recovered
afterward. Use it for repeated no-growth access; perform growth only between
mutable content scopes.

The growable owner deliberately has no splitting operation. Splitting it would
either duplicate the stable header or create independently replaceable headers
over overlapping storage: subsequent growth could invalidate one side, and
parallel mutation would no longer be race-free. To process disjoint regions,
open a fixed 'getContents' view inside 'withContent' and split that fixed view;
growth remains unavailable until all pieces have been consumed.

Growth has a normal-return ownership guarantee. Capacity arithmetic and
allocation happen before the logical length is published, and no user callback
runs during a partially completed growth operation. This module does not claim
that an owner is recoverable after a synchronous or asynchronous exception
escapes a @BO@ computation.
-}
module Data.Vector.Mutable.Growable.Linear.Borrow (
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
  unsafeCopyAt,
  copyAtMut,
  unsafeCopyAtMut,
  set,
  unsafeSet,
  update,
  unsafeUpdate,
  modify,
  swap,
  unsafeSwap,
  indicesMut,
  unsafeIndicesMut,
  reserve,
  reserveAdditional,
  push,
  extend,
  getContents,
  withContent,
  withContent_,
) where

import Data.Vector.Mutable.Growable.Linear.Borrow.Internal
