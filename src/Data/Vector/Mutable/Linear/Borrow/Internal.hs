{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Mutable.Linear.Borrow.Internal (
  module Data.Vector.Mutable.Linear.Borrow.Internal,
) where

import Control.Monad qualified as NonLinear
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Data.Vector.Mutable (RealWorld)
import Data.Vector.Mutable qualified as MV
import GHC.IO (unsafePerformIO)
import GHC.TypeError
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

-- | Trusted representation of the public fixed-size boxed vector.
newtype Vector a = Vector {content :: MV.MVector RealWorld a}

type role Vector nominal

-- | Construct a fixed-size view over a raw mutable-vector slice.
unsafeFromMutableSlice :: Int -> Int -> MV.MVector RealWorld a %1 -> Vector a
{-# INLINE unsafeFromMutableSlice #-}
unsafeFromMutableSlice =
  Unsafe.toLinear3 \offset length_ buffer ->
    Vector (MV.unsafeSlice offset length_ buffer)

instance LinearOnly (Vector a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  (Unsatisfiable (ShowType (Vector a) :<>: Text " cannot be copied!")) =>
  Copyable (Vector a)
  where
  copy = unsatisfiable

{- | Each element is cloned through a shared borrow of it, with its own 'Clone', into a fresh vector.

The original is only read, so any number of 'Control.Monad.Borrow.Pure.parBO' branches may clone the same vector at once, unless an element is still an unevaluated call that updates memory in place, such as linear-base's @Data.Array.Mutable.Linear.map@.
'Data.Vector.Mutable.Linear.Borrow.fromList' stores its elements unevaluated, so evaluate each such call before building the vector: see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
See Note [Cloning the contents of a shared borrow] in @Data.Ref.Linear.Internal@.
-}
instance (Clone a) => Clone (Vector a) where
  clone :: forall α. Share α (Vector a) %1 -> BO α (Vector a)
  clone = Unsafe.toLinear \(UnsafeAlias (Vector v)) -> unsafeSystemIOToBO do
    let !n = MV.length v
    !new <- MV.new n
    let go !i = NonLinear.when (i < n) do
          x <- MV.unsafeRead v i
          copied <- unsafeBOToSystemIO (clone @a @α (UnsafeAlias x))
          MV.unsafeWrite new i copied
          go (i + 1)
    go 0
    NonLinear.pure (Vector new)
  {-# INLINE clone #-}

{- | Consume every element, then drop the storage.

For elements that are not 'Movable', such as t'Data.Ref.Linear.Ref's, this is the only way to dispose of the vector: its conversions to @vector@'s types and to lists need 'Movable'.
-}
instance (Consumable a) => Consumable (Vector a) where
  consume =
    Unsafe.toLinear \(Vector vector) ->
      unsafePerformIO (consumeElements 0 (MV.length vector) vector)
  -- The traversal only reads the buffer, but it runs under
  -- 'unsafePerformIO'. Inlining would let GHC duplicate that call across use
  -- sites, or float it out of a scope, and each copy would consume the
  -- elements again; 'NOINLINE' keeps exactly one occurrence.
  {-# NOINLINE consume #-}

consumeElements ::
  (Consumable a) =>
  Int ->
  Int ->
  MV.IOVector a ->
  NonLinear.IO ()
{-# INLINE consumeElements #-}
consumeElements !index !length_ vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- MV.unsafeRead vector index
      let !() = consume value
      consumeElements (index + 1) length_ vector
