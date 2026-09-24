module Data.Ref.Linear (
  Ref,
  new,
  free,
  unsafeReadRef,
  unsafeWriteRef,
  atomicModify,
  atomicModify_,
) where

import Data.Ref.Linear.Internal
