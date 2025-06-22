{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Vector.Mutable.Linear.Borrow (
  Vector,
  empty,
  constant,
  fromList,
  fromVector,
  unsafeFromVector,
  size,
  unsafeGet,
  get,
  unsafeSet,
  set,
  unsafeIndicesMut,
  indicesMut,
  splitAtMut,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Utils
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Function qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.IntSet qualified as IntSet
import Data.Vector qualified as V
import Data.Vector.Mutable (RealWorld)
import Data.Vector.Mutable qualified as MV
import GHC.Exts qualified as GHC
import GHC.Stack (HasCallStack)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

newtype Vector a = Vector {content :: MV.MVector RealWorld a}

empty :: Linearly %1 -> Vector a
{-# NOINLINE empty #-}
empty l =
  l `lseq` GHC.noinline do
    Vector (unsafePerformEvaluateUndupableBO (unsafeSystemIOToBO $ MV.new 0))

constant :: Int -> a -> Linearly %1 -> Vector a
{-# NOINLINE constant #-}
constant n a l =
  l `lseq` GHC.noinline do
    Vector
      $! unsafePerformEvaluateUndupableBO
      $! unsafeSystemIOToBO
      $! MV.replicate n a

fromList :: [a] -> Linearly %1 -> Vector a
{-# NOINLINE fromList #-}
fromList as l =
  l `lseq` GHC.noinline do
    Vector
      $! unsafePerformEvaluateUndupableBO
      $! unsafeSystemIOToBO
      $! V.unsafeThaw
      $! V.fromList as

-- | Convert a 'V.Vector' (from @vector@ package) to a 'Vector'
fromVector :: V.Vector a -> Linearly %1 -> Vector a
{-# NOINLINE fromVector #-}
fromVector v l =
  l `lseq` GHC.noinline do
    Vector
      $! unsafePerformEvaluateUndupableBO
      $! unsafeSystemIOToBO
      $! V.thaw v

{- | Unsafely thaws 'V.Vector' (from @vector@ package) to a 'Vector',
reusing the same memory.
This is highly unsafe
-}
unsafeFromVector :: V.Vector a %1 -> Linearly %1 -> Vector a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector = Unsafe.toLinear \v l ->
  l `lseq` GHC.noinline do
    Vector
      $! unsafePerformEvaluateUndupableBO
      $! unsafeSystemIOToBO
      $! V.unsafeThaw v

size :: (AccessibleRef ref) => ref (Vector a) %1 -> (Ur Int, ref (Vector a))
{-# INLINE size #-}
size =
  unsafeUnwrapRef >>> Unsafe.toLinear \(Vector v) ->
    (move (MV.length v), unsafeWrapRef (Vector v))

-- | Get without bounds check.
unsafeGet :: (AccessibleRef ref) => Int -> ref (Vector a) %1 -> BO α (ref a)
{-# INLINE unsafeGet #-}
unsafeGet i =
  Unsafe.toLinear \v ->
    GHC.noinline
      $ unsafeUnwrapRef v
      NonLinear.& \(Vector v) ->
        unsafeWrapRef
          Control.<$> unsafeSystemIOToBO (MV.unsafeRead v i)

get ::
  ( HasCallStack
  , AccessibleRefAt α ref
  ) =>
  Int -> ref (Vector a) %1 -> BO α (ref a)
{-# INLINE get #-}
get i v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len
        then error ("get: index " <> show i <> " out of bound: " <> show len) v
        else unsafeGet i v

unsafeSet :: Int -> a %1 -> Mut α (Vector a) %1 -> BO α (Mut α (Vector a))
{-# INLINE unsafeSet #-}
unsafeSet i = Unsafe.toLinear2 \a (UnsafeMut v) ->
  let v' = v
   in Control.do
        () <- unsafeSystemIOToBO $ Unsafe.toLinear3 MV.unsafeWrite (content v') i a
        Control.pure $ UnsafeMut v

set ::
  (HasCallStack) =>
  Int -> a %1 -> Mut α (Vector a) %1 -> BO α (Mut α (Vector a))
set i a v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len
        then error ("set: index " <> show i <> " out of bound: " <> show len) v a
        else unsafeSet i a v

{- | Get multiple elements at the given indices without bounds and duplication check.
For more safety, use 'indicesMut'.
-}
unsafeIndicesMut :: Mut α (Vector a) %1 -> [Int] %1 -> BO α [Mut α a]
unsafeIndicesMut = Unsafe.toLinear \v is ->
  Data.traverse
    (\i -> move i & \(Ur i) -> unsafeGet i v)
    is

indicesMut :: (HasCallStack) => Mut α (Vector a) %1 -> [Int] %1 -> BO α [Mut α a]
indicesMut = Unsafe.toLinear2 \v is ->
  case size v of
    (Ur len, v) ->
      if
        | any (\i -> move i & \(Ur i) -> i < 0 || i >= len) is ->
            error ("indicesMut: indices out of bound: " <> show is <> " for length " <> show len) v
        | NonLinear.length is > IntSet.size (IntSet.fromList is) ->
            error ("indicesMut: duplicate indices: " <> show is) v
        | otherwise -> unsafeIndicesMut v is

splitAtMut :: Mut α (Vector a) %1 -> Int %1 -> (Mut α (Vector a), Mut α (Vector a))
{-# INLINE splitAtMut #-}
splitAtMut = Unsafe.toLinear2 \(UnsafeMut (Vector v)) i ->
  let (v1, v2) = MV.splitAt i v
   in (UnsafeMut (Vector v1), UnsafeMut (Vector v2))
