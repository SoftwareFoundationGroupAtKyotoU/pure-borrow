{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  fromMutable,
  unsafeFromMutable,
  toVector,
  toList,
  size,
  unsafeGet,
  get,
  unsafeSet,
  set,
  unsafeUpdate,
  update,
  unsafeHead,
  head,
  unsafeLast,
  last,
  unsafeIndicesMut,
  indicesMut,
  splitAtMut,
  unsafeSwap,
  swap,

  -- * An example algorithm implementations
  qsort,

  -- ** Internal functions
  divide,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Utils
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Function qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.IntSet qualified as IntSet
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector qualified as V
import Data.Vector.Mutable (RealWorld)
import Data.Vector.Mutable qualified as MV
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Prelude.Linear hiding (head, last)
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
    Vector $!
      unsafePerformEvaluateUndupableBO $!
        unsafeSystemIOToBO $!
          MV.replicate n a

fromList :: [a] %1 -> Linearly %1 -> Vector a
{-# NOINLINE fromList #-}
fromList = Unsafe.toLinear \as l ->
  l `lseq` GHC.noinline do
    Vector $!
      unsafePerformEvaluateUndupableBO $!
        unsafeSystemIOToBO $!
          V.unsafeThaw $!
            V.fromList as

-- | Convert a 'V.Vector' (from @vector@ package) to a 'Vector'
fromVector :: V.Vector a -> Linearly %1 -> Vector a
{-# NOINLINE fromVector #-}
fromVector v l =
  l `lseq` GHC.noinline do
    Vector $!
      unsafePerformEvaluateUndupableBO $!
        unsafeSystemIOToBO $!
          V.thaw v

fromMutable :: MV.MVector s a %1 -> Linearly %1 -> Vector a
{-# NOINLINE fromMutable #-}
fromMutable = Unsafe.toLinear \v l ->
  l `lseq` GHC.noinline do
    Vector (unsafePerformIO (MV.clone (Unsafe.coerce v)))

unsafeFromMutable :: MV.MVector s a %1 -> Linearly %1 -> Vector a
unsafeFromMutable v lin =
  lin `lseq` Vector (Unsafe.coerce v)

toVector :: Vector a %1 -> Ur (V.Vector a)
{-# NOINLINE toVector #-}
toVector = GHC.noinline $
  Unsafe.toLinear \(Vector v) -> Ur (unsafePerformIO $ V.unsafeFreeze v)

toList :: Vector a %1 -> Ur [a]
{-# INLINE toList #-}
toList = Ur.lift V.toList . toVector

{- | Unsafely thaws 'V.Vector' (from @vector@ package) to a 'Vector',
reusing the same memory.
This is highly unsafe
-}
unsafeFromVector :: V.Vector a %1 -> Linearly %1 -> Vector a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector = Unsafe.toLinear \v l ->
  l `lseq` GHC.noinline do
    Vector $!
      unsafePerformEvaluateUndupableBO $!
        unsafeSystemIOToBO $!
          V.unsafeThaw v

size :: (Borrow bor) => bor (Vector a) %1 -> (Ur Int, bor (Vector a))
{-# INLINE size #-}
size =
  unsafeUnwrapView >>> Unsafe.toLinear \(Vector v) ->
    (move (MV.length v), unsafeWrapView (Vector v))

-- | Get without bounds check.
unsafeGet :: (BorrowAt α bor) => Int -> bor (Vector a) %1 -> BO α (bor a)
{-# INLINE unsafeGet #-}
unsafeGet i =
  Unsafe.toLinear \v ->
    GHC.noinline $
      unsafeUnwrapView v
        NonLinear.& \(Vector v) ->
          unsafeWrapView
            Control.<$> unsafeSystemIOToBO (MV.unsafeRead v i)

head :: (HasCallStack, BorrowAt α bor) => bor (Vector a) %1 -> BO α (bor a)
{-# INLINE head #-}
head = get 0

unsafeHead :: (BorrowAt α bor) => bor (Vector a) %1 -> BO α (bor a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

unsafeLast :: (BorrowAt α bor) => bor (Vector a) %1 -> BO α (bor a)
{-# INLINE unsafeLast #-}
unsafeLast v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len -> unsafeGet (len - 1) v

last :: (HasCallStack, BorrowAt α bor) => bor (Vector a) %1 -> BO α (bor a)
{-# INLINE last #-}
last v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len
      | len > 0 -> unsafeGet (len - 1) v
      | otherwise -> error ("last: empty vector") v

get ::
  ( HasCallStack
  , BorrowAt α bor
  ) =>
  Int -> bor (Vector a) %1 -> BO α (bor a)
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

unsafeUpdate :: (β <= α) => Int -> (a %1 -> BO β (a, b)) %1 -> Mut α (Vector a) %1 -> BO β (Mut α (Vector a), b)
{-# INLINE unsafeUpdate #-}
unsafeUpdate i = Unsafe.toLinear2 \k (UnsafeMut v) -> Control.do
  a <- unsafeSystemIOToBO $ MV.unsafeRead (content v) i
  (a', b) <- k a
  () <- unsafeSystemIOToBO $ Unsafe.toLinear3 MV.unsafeWrite (content v) i a'
  Control.pure $ (UnsafeMut v, b)

update :: (β <= α) => Int -> (a %1 -> BO β (a, b)) %1 -> Mut α (Vector a) %1 -> BO β (Mut α (Vector a), b)
update i k v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len
        then error ("set: index " <> show i <> " out of bound: " <> show len) v k
        else unsafeUpdate i k v

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

splitAtMut :: Int %1 -> Mut α (Vector a) %1 -> (Mut α (Vector a), Mut α (Vector a))
{-# INLINE splitAtMut #-}
splitAtMut = Unsafe.toLinear2 \i (UnsafeMut (Vector v)) ->
  let (v1, v2) = MV.splitAt i v
   in (UnsafeMut (Vector v1), UnsafeMut (Vector v2))

instance LinearOnly (Vector a) where
  unsafeWithLinear = unsafeLinearOnly
  {-# INLINE unsafeWithLinear #-}

unsafeSwap :: Mut α (Vector a) %1 -> Int -> Int -> BO α (Mut α (Vector a))
unsafeSwap = Unsafe.toLinear3 \(UnsafeMut v) i j -> Control.do
  () <- unsafeSystemIOToBO $ MV.unsafeSwap v.content i j
  Control.pure $ UnsafeMut v

swap :: (HasCallStack) => Mut α (Vector a) %1 -> Int -> Int -> BO α (Mut α (Vector a))
swap v i j = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len || j < 0 || j >= len
        then error ("swap: index out of bound: " <> show (i, j) <> " for length " <> show len) v
        else unsafeSwap v i j

{- | A simple parallel implementation of quicksort.
It uses a sequential divide-and-conquer when size <8,
and parallel divide-and-conquer with 'parBO' otherwise.

This is meant to be a demonstrative implementation and
not practical - you need a genuine parallel scheduler
to scale this up.
-}
qsort ::
  (Ord a, Deborrowable a, Movable a) =>
  {- | Cost for using parallelism. Halved after each recursive call,
  and stops parallelizing when it reaches 1.
  -}
  Word ->
  Mut α (Vector a) %1 ->
  BO α ()
qsort = go
  where
    go budget v = case size v of
      (Ur 0, v) -> Control.pure $ consume v
      (Ur 1, v) -> Control.pure $ consume v
      (Ur n, v) ->
        let i = n `quot` 2
         in Control.do
              (pivot, v) <- sharing_ v \v ->
                move . deborrow Control.<$> unsafeGet i v
              pivot & \(Ur pivot) -> Control.do
                (lo, hi) <- divide pivot v 0 n
                let b' = budget `quot` 2
                Control.void $ parIf (b' NonLinear.> 0) (go b' lo) (go b' hi)

parIf :: Bool %1 -> BO α a %1 -> BO α b %1 -> BO α (a, b)
{-# INLINE parIf #-}
parIf p =
  if p
    then parBO
    else \l r -> Control.do
      !l <- l
      !r <- r
      Control.pure (l, r)

divide ::
  (Ord a, Deborrowable a) =>
  a ->
  Mut α (Vector a) %1 ->
  Int ->
  Int ->
  BO α (Mut α (Vector a), Mut α (Vector a))
divide pivot = partUp
  where
    partUp v l u
      | l < u = Control.do
          (e, v) <- sharing_ v $ Control.fmap deborrow . unsafeGet l
          if e < pivot
            then partUp v (l + 1) u
            else partDown v l (u - 1)
      | otherwise = Control.pure $ splitAtMut l v
    partDown v l u
      | l < u = Control.do
          (e, v) <- sharing_ v $ Control.fmap deborrow . unsafeGet u
          if pivot < e
            then partDown v l (u - 1)
            else Control.do
              v <- unsafeSwap v l u
              partUp v (l + 1) u
      | otherwise = Control.pure $ splitAtMut l v
