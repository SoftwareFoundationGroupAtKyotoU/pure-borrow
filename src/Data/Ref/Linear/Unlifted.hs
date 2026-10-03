{-# LANGUAGE MagicHash #-}

module Data.Ref.Linear.Unlifted (
  Ref#,
  newRef#,
  freeRef#,
  unsafeReadRef#,
  unsafeWriteRef#,
  atomicModify_#,
  atomicModify#,
) where

import Data.Ref.Linear.Unlifted.Internal
