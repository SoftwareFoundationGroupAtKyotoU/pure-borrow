{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
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
  get,
  unsafeGet,
  set,
  unsafeSet,
  update,
  unsafeUpdate,
  modify,
  head,
  unsafeHead,
  last,
  unsafeLast,
  indicesMut,
  unsafeIndicesMut,
  splitAt,
  swap,
  unsafeSwap,
  copyAt,
  copyAtMut,
  inplace,

  -- * An example algorithm implementations
  qsort,

  -- ** Internal functions
  divide,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad qualified as NonLinear
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Control.Monad.Borrow.Pure.Unsafe
import Control.Monad.Borrow.Pure.Utils
import Control.Monad.ST.Strict (ST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce.Directed (upcast)
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
import GHC.TypeError
import Prelude.Linear hiding (head, last, splitAt)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

{- |
Linearly owned mutable vector.
Contrary to those in @linear-base@, our 'Vector' owns every element @linearly@.
This is because Pure Borrow can now treat nested mutability safely, so we must allow mutable values to be stored inside 'Vector'.
This manifests in the type of 'set' - it returns the old value, which MUST NOT drop in favour of the new value.
-}
newtype Vector a = Vector {content :: MV.MVector RealWorld a}

empty :: Linearly %1 -> Vector a
{-# NOINLINE empty #-}
empty =
  GHC.noinline \l ->
    l `lseq` do
      Vector (unsafePerformIO $ MV.new 0)

constant :: Int -> a -> Linearly %1 -> Vector a
{-# NOINLINE constant #-}
constant = GHC.noinline \n a l ->
  l `lseq` do
    Vector $!
      unsafePerformIO $!
        MV.replicate n a

fromList :: [a] %1 -> Linearly %1 -> Vector a
{-# NOINLINE fromList #-}
fromList = GHC.noinline $ Unsafe.toLinear \as l ->
  l `lseq` do
    Vector $!
      unsafePerformIO $!
        Unsafe.toLinear V.unsafeThaw $!
          Unsafe.toLinear V.fromList as

-- | Convert a 'V.Vector' (from @vector@ package) to a 'Vector'.
fromVector :: V.Vector a -> Linearly %1 -> Vector a
{-# NOINLINE fromVector #-}
fromVector = GHC.noinline $ Unsafe.toLinear \v l ->
  l `lseq` do
    Vector $!
      unsafePerformIO $!
        Unsafe.toLinear V.thaw v

-- | _O(n)_. Clone a 'V.MVector' from @vector@ package to a 'Vector'.
fromMutable :: MV.MVector s a %1 -> Linearly %1 -> Vector a
{-# NOINLINE fromMutable #-}
fromMutable = GHC.noinline $ Unsafe.toLinear \v l ->
  l `lseq` do
    Vector $!
      unsafePerformIO $!
        Unsafe.toLinear MV.clone (Unsafe.coerce v)

unsafeFromMutable :: MV.MVector s a %1 -> Linearly %1 -> Vector a
unsafeFromMutable v lin =
  lin `lseq` Vector (Unsafe.coerce v)

{- 
NOTE [Unrestricted Materialization of Vector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We impose 'Copyable' on 'toVector' and 'toList' to ensure elements doesn't bare any essentially linear contents inside, but we don't make use of the constraint internally.
Is it a cheating? Maybe. Think hard about it.
-}

-- | _O(1)_. Freezes @'Vector' a@ to @'V.Vector' a@ from @vector@ package, _without_ copying.
toVector :: 
  -- See Note [Unrestricted Materialization of Vector].
  Copyable a => 
  Vector a %1 -> Ur (V.Vector a)
{-# NOINLINE toVector #-}
toVector = GHC.noinline $
  Unsafe.toLinear \(Vector v) ->Ur $ unsafePerformIO $ V.unsafeFreeze v

-- Same applies to 'Copyable' here, as in 'toVector'.
toList :: 
  -- See Note [Unrestricted Materialization of Vector].
  Copyable a => 
  Vector a %1 -> Ur [a]
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
      unsafePerformIO $!
        V.unsafeThaw v

size :: Borrow bk α (Vector a) %1 -> (Ur Int, Borrow bk α (Vector a))
{-# INLINE size #-}
size =
  unsafeUnalias >>> Unsafe.toLinear \(Vector v) ->
    (move (MV.length v), UnsafeAlias (Vector v))

{- |
@'set' i a v@ sets the @i@-th element of @v@ to @a@, and returns the old value alongside.
Note that @a@ is bound linearly.
-}
set :: (HasCallStack, α >= β) => Int -> a %1 -> Mut α (Vector a) %1 -> BO β (a, Mut α (Vector a))
{-# INLINE set #-}
set i a v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len
        then error ("get: index " <> show i <> " out of bound: " <> show len) v a
        else unsafeSet i a v

-- | 'set' without bound check.
unsafeSet :: (α >= β) => Int -> a %1 -> Mut α (Vector a) %1 -> BO β (a, Mut α (Vector a))
unsafeSet = Unsafe.toLinear3 \i a mut@(UnsafeAlias (Vector v)) -> unsafeSystemIOToBO do
  old <- MV.unsafeRead v i
  MV.unsafeWrite v i a
  NonLinear.pure (old, mut)

-- | 'get' without bounds check.
unsafeGet :: (α >= β) => Int -> Borrow bk α (Vector a) %1 -> BO β (Borrow bk α a)
{-# INLINE unsafeGet #-}
unsafeGet i =
  Unsafe.toLinear \v ->
    unsafeUnalias v
      NonLinear.& \(Vector v) ->
        UnsafeAlias
          Control.<$> unsafeSystemIOToBO (MV.unsafeRead v i)

head :: (HasCallStack, α >= β) => Borrow bk α (Vector a) %1 -> BO β (Borrow bk α a)
{-# INLINE head #-}
head = get 0

unsafeHead :: (α >= β) => Borrow bk α (Vector a) %1 -> BO β (Borrow bk α a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

unsafeLast :: (α >= β) => Borrow bk α (Vector a) %1 -> BO β (Borrow bk α a)
{-# INLINE unsafeLast #-}
unsafeLast v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len -> unsafeGet (len - 1) v

last :: (HasCallStack, α >= β) => Borrow bk α (Vector a) %1 -> BO β (Borrow bk α a)
{-# INLINE last #-}
last v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len
      | len > 0 -> unsafeGet (len - 1) v
      | otherwise -> error ("last: empty vector") v

get ::
  (HasCallStack, α >= β) =>
  Int -> Borrow bk α (Vector a) %1 -> BO β (Borrow bk α a)
{-# INLINE get #-}
get i v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len
        then error ("get: index " <> show i <> " out of bound: " <> show len) v
        else unsafeGet i v

unsafeUpdate :: (α >= β) => Int -> (a %1 -> BO β (b, a)) %1 -> Mut α (Vector a) %1 -> BO β (b, Mut α (Vector a))
unsafeUpdate i = Unsafe.toLinear2 \k (UnsafeAlias v) -> Control.do
  a <- unsafeSystemIOToBO $ MV.unsafeRead (content v) i
  (b, a') <- k a
  () <- unsafeSystemIOToBO $ Unsafe.toLinear3 MV.unsafeWrite (content v) i a'
  Control.pure $ (b, UnsafeAlias v)

update :: (α >= β) => Int -> (a %1 -> BO β (b, a)) %1 -> Mut α (Vector a) %1 -> BO β (b, Mut α (Vector a))
update i k v = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len
        then error ("set: index " <> show i <> " out of bound: " <> show len) v k
        else unsafeUpdate i k v

modify :: (α >= β) => Int -> (a %1 -> a) %1 -> Mut α (Vector a) %1 -> BO β (Mut α (Vector a))
modify i f v = Control.do
  ((), ma) <- update i (Control.pure . ((),) . f) v
  Control.pure ma

{- | Get multiple elements at the given indices without bounds and duplication check.
For more safety, use 'indicesMut'.
-}
unsafeIndicesMut :: (α >= β) => Mut α (Vector a) %1 -> [Int] %1 -> BO β [Mut α a]
unsafeIndicesMut = Unsafe.toLinear \v is ->
  Data.traverse
    (\i -> move i & \(Ur i) -> unsafeGet i v)
    is

indicesMut :: (HasCallStack, α >= β) => Mut α (Vector a) %1 -> [Int] %1 -> BO β [Mut α a]
indicesMut = Unsafe.toLinear2 \v is ->
  case size v of
    (Ur len, v) ->
      if
        | any (\i -> move i & \(Ur i) -> i < 0 || i >= len) is ->
            error ("indicesMut: indices out of bound: " <> show is <> " for length " <> show len) v
        | NonLinear.length is > IntSet.size (IntSet.fromList is) ->
            error ("indicesMut: duplicate indices: " <> show is) v
        | otherwise -> unsafeIndicesMut v is

splitAt :: Int %1 -> Borrow bk α (Vector a) %1 -> (Borrow bk α (Vector a), Borrow bk α (Vector a))
{-# INLINE splitAt #-}
splitAt = Unsafe.toLinear2 \i (UnsafeAlias (Vector v)) ->
  let (v1, v2) = MV.splitAt i v
   in (UnsafeAlias (Vector v1), UnsafeAlias (Vector v2))

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

unsafeSwap :: (α >= β) => Mut α (Vector a) %1 -> Int -> Int -> BO β (Mut α (Vector a))
unsafeSwap = Unsafe.toLinear3 \(UnsafeAlias v) i j -> Control.do
  () <- unsafeSystemIOToBO $ MV.unsafeSwap v.content i j
  Control.pure $ UnsafeAlias v

swap :: (HasCallStack, α >= β) => Mut α (Vector a) %1 -> Int -> Int -> BO β (Mut α (Vector a))
swap v i j = DataFlow.do
  (len, v) <- size v
  case len of
    Ur len ->
      if i < 0 || i >= len || j < 0 || j >= len
        then error ("swap: index out of bound: " <> show (i, j) <> " for length " <> show len) v
        else unsafeSwap v i j

copyAt :: (Copyable a, α >= β) => Int -> Share α (Vector a) -> BO β (Ur a)
copyAt i v = Control.do Ur s <- move Control.<$> get i v; Control.pure $ Ur $ copy s

copyAtMut :: forall a α β. (Copyable a, α >= β) => Int -> Mut α (Vector a) %1 -> BO β (Ur a, Mut α (Vector a))
copyAtMut i v = upcast $ sharing @_ @α v $ copyAt i

-- | Applies an in-place mutation on 'V.MVector' from @vector@ package.
inplace ::
  (α >= β) =>
  (forall s. V.MVector s a -> ST s ()) %1 ->
  Mut α (Vector a) %1 ->
  BO β (Mut α (Vector a))
{-# INLINE inplace #-}
inplace = Unsafe.toLinear2 \f (UnsafeAlias v) -> Control.do
  !() <- unsafeSTToBO $ f $ content $ coerceLin v
  Control.pure (UnsafeAlias v)

{- | A simple parallel implementation of quicksort.
It uses a sequential divide-and-conquer when size <8,
and parallel divide-and-conquer with 'parBO' otherwise.

This is meant to be a demonstrative implementation and
not practical - you need a genuine parallel scheduler
to scale this up.
-}
qsort ::
  forall a α β.
  (Ord a, Copyable a, α >= β) =>
  {- | Cost for using parallelism. Halved after each recursive call,
  and stops parallelizing when it reaches 1.
  -}
  Word ->
  Mut α (Vector a) %1 ->
  BO β ()
qsort = go
  where
    go :: Word -> Mut α (Vector a) %1 -> BO β ()
    go budget v = case size v of
      (Ur 0, v) -> Control.pure $ consume v
      (Ur 1, v) -> Control.pure $ consume v
      (Ur n, v) -> Control.do
        let i = n `quot` 2
        (Ur pivot, v) <- copyAtMut i v
        (lo, hi) <- divide pivot v 0 n
        let b' = budget `quot` 2
        Control.void $ parIf (b' NonLinear.> 0) (go b' lo) (go b' hi)

parIf :: Bool %1 -> BO α a %1 -> BO α b %1 -> BO α (a, b)
{-# INLINE parIf #-}
parIf p = if p then parBO else Control.liftA2 (,)

divide ::
  (Ord a, Copyable a, α >= β) =>
  a ->
  Mut α (Vector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (Vector a), Mut α (Vector a))
divide pivot = partUp
  where
    partUp v l u
      | l < u = Control.do
          (Ur e, v) <- copyAtMut l v
          if e < pivot
            then partUp v (l + 1) u
            else partDown v l (u - 1)
      | otherwise = Control.pure $ splitAt l v
    partDown v l u
      | l < u = Control.do
          (Ur e, v) <- copyAtMut u v
          if pivot < e
            then partDown v l (u - 1)
            else Control.do
              v <- unsafeSwap v l u
              partUp v (l + 1) u
      | otherwise = Control.pure $ splitAt l v
