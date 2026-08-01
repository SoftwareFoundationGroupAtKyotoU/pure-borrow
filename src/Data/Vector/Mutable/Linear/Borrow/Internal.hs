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

instance (Dupable a) => Clone (Vector a) where
  clone = Unsafe.toLinear \(UnsafeAlias (Vector v)) -> unsafeSystemIOToBO do
    let !n = MV.length v
    !new <- MV.new n
    let go !i = NonLinear.when (i < n) do
          x <- MV.unsafeRead v i
          let (!_, !x') = dup x
          MV.unsafeWrite new i x'
          go (i + 1)
    go 0
    NonLinear.pure (Vector new)
  {-# INLINE clone #-}
