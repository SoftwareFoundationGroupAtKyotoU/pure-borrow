{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
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
  unsafeInplace,
  unsafeModifyBoxedMVector,
  modifyBoxedVector,

  -- * An example algorithm implementations
  qsort,

  -- ** Internal functions
  divide,

  -- * Removed
  modifyBoxedMVector,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Utils
import Control.Monad.ST.Strict (ST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Function qualified as NonLinear
import Data.Functor.Linear qualified as Data
import Data.IntSet qualified as IntSet
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector qualified as V
import Data.Vector.Generic qualified as G
import Data.Vector.Mutable (RealWorld)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Linear.Borrow.Internal (Vector (..))
import GHC.Exts qualified as GHC
import GHC.IO (evaluate, unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError (ErrorMessage (..), Unsatisfiable, unsatisfiable)
import Prelude.Linear hiding (head, last, splitAt)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

{- |
Linearly owned mutable vector.
Contrary to those in @linear-base@, our 'Vector' owns every element @linearly@.
This is because Pure Borrow can now treat nested mutability safely, so we must allow mutable values to be stored inside 'Vector'.
This manifests in the type of 'set' - it returns the old value, which MUST NOT drop in favour of the new value.
-}
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

{- | Build a vector that owns the elements of a list.

The list and each element are evaluated to weak head normal form, in order, as the vector itself is evaluated, which borrowing it does.
A placeholder such as @undefined@ therefore raises then, and expensive elements are computed one after another by the thread that evaluates the vector, rather than by the branches that read them.
To keep an expensive GC-owned element lazy, store it in a lazy box, such as t'Prelude.Linear.Ur'.
A lazy field inside an element, such as the component of a pair, stays unevaluated; see [Contents that are not evaluated yet]("Control.Monad.Borrow.Pure.Clone#lazy").
-}
fromList :: [a] %1 -> Linearly %1 -> Vector a
{-# NOINLINE fromList #-}
fromList = GHC.noinline $ Unsafe.toLinear \as l ->
  l `lseq` (Vector $! unsafePerformIO (thawEvaluated as))

{- | Store the elements of a list in a new mutable vector, each evaluated to weak head normal form.

Run inside 'unsafePerformIO', after its 'GHC.noDuplicate#', both the list and its elements are evaluated once: see Note [Stored contents are evaluated after noDuplicate#] in "Data.Ref.Linear.Unlifted.Internal".
'evaluate' keeps the demand on the list from GHC, and the list is taken as it is produced, one cell at a time, as 'V.fromList' takes it.
-}
thawEvaluated :: [a] -> NonLinear.IO (MV.IOVector a)
thawEvaluated as = evaluate (G.unstream (evaluatingBundle as)) NonLinear.>>= V.unsafeThaw

-- | Convert a 'V.Vector' (from @vector@ package) to a 'Vector'.
fromVector :: V.Vector a -> Linearly %1 -> Vector a
{-# NOINLINE fromVector #-}
fromVector = GHC.noinline $ Unsafe.toLinear \v l ->
  l `lseq` do
    Vector $!
      unsafePerformIO $!
        Unsafe.toLinear V.thaw v

-- | /O(n)/. Clone a 'V.MVector' from @vector@ package to a 'Vector'.
fromMutable :: MV.MVector s a %1 -> Linearly %1 -> Vector a
{-# NOINLINE fromMutable #-}
fromMutable = GHC.noinline $ Unsafe.toLinear \v l ->
  l `lseq` do
    Vector $!
      unsafePerformIO $!
        Unsafe.toLinear MV.clone (Unsafe.coerce v)

{- | Take ownership of a boxed mutable vector from @vector@ without copying.

Every element must be initialised, since consuming the vector reads each one, and the caller must keep no alias through which the storage or its elements could still be reached.
-}
unsafeFromMutable :: MV.MVector s a %1 -> Linearly %1 -> Vector a
unsafeFromMutable v lin =
  lin `lseq` Vector (Unsafe.coerce v)

{-
Note [Unrestricted Materialization of Vector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consuming 'toVector' and 'toList' transfer their elements from a linear owner
to an unrestricted, GC-owned result. 'Movable' is exactly the evidence for
that transfer. Each element is passed through 'move', which performs any deep
copy required by its 'Movable' instance. 'Copyable' is neither sufficient nor
required.
-}

-- | /O(n)/. Move every element into GC ownership, then freeze the storage.
toVector ::
  -- See Note [Unrestricted Materialization of Vector].
  (Movable a) =>
  Vector a %1 -> Ur (V.Vector a)
{-# NOINLINE toVector #-}
toVector = GHC.noinline $
  Unsafe.toLinear \(Vector v) ->
    let !frozen =
          unsafePerformIO do
            moveElements 0 (MV.length v) v
            V.unsafeFreeze v
     in Ur frozen

-- Same applies to 'Movable' here, as in 'toVector'.
toList ::
  -- See Note [Unrestricted Materialization of Vector].
  (Movable a) =>
  Vector a %1 -> Ur [a]
{-# INLINE toList #-}
toList = Ur.lift V.toList . toVector

moveElements ::
  (Movable a) =>
  Int ->
  Int ->
  MV.IOVector a ->
  NonLinear.IO ()
{-# INLINE moveElements #-}
moveElements !index !length_ vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- MV.unsafeRead vector index
      case move value of
        Ur !moved -> MV.unsafeWrite vector index moved
      moveElements (index + 1) length_ vector

{- | Unsafely thaws 'V.Vector' (from @vector@ package) to a 'Vector',
reusing the same memory.

This is highly unsafe: the immutable vector must never be read again, because the new owner mutates its storage and treats its elements as linearly owned.
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
  -- WHNF forcing stays inside the guarded run; see Note [Demand stays inside a BO run] in "Control.Monad.Borrow.Pure.BO.Internal".
  stored <- evaluateStored a
  !old <- MV.unsafeRead v i
  MV.unsafeWrite v i stored
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
  -- WHNF forcing stays inside the guarded run; see Note [Demand stays inside a BO run] in "Control.Monad.Borrow.Pure.BO.Internal".
  b <- unsafeSystemIOToBO (Unsafe.toLinear evaluateStored b)
  () <- unsafeSystemIOToBO $ Unsafe.toLinear (\x -> evaluateStored x NonLinear.>>= MV.unsafeWrite (content v) i) a'
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
copyAt i v = Control.do Ur !s <- move Control.<$> get i v; Control.pure $! Ur $! copy s

copyAtMut :: forall a α β. (Copyable a, α >= β) => Int -> Mut α (Vector a) %1 -> BO β (Ur a, Mut α (Vector a))
{-# INLINE copyAtMut #-}
#ifdef PURE_BORROW_SLOW_SCOPES
copyAtMut i v = upcast $ sharing @_ @α v $ copyAt i
#else
copyAtMut = Unsafe.toLinear2 \i mut@(UnsafeAlias (Vector v)) ->
  let !len = MV.length v
   in if i < 0 || i >= len
        then error ("get: index " <> show i <> " out of bound: " <> show len) mut
        else unsafeSystemIOToBO do
          !a <- MV.unsafeRead v i
          -- The raw read temporarily aliases the element retained by the
          -- vector. 'copy' consumes that alias and returns only an authorized
          -- unrestricted copy; the mutable vector borrow stays exclusive.
          let !copied = copy (UnsafeAlias a)
          NonLinear.pure (Ur copied, mut)
#endif

{- | Applies an in-place mutation on 'V.MVector' from @vector@ package.

The vector owns its elements linearly, and the callback bypasses that: it must only rearrange the elements, never duplicate, drop or replace one.
A duplicated element would later be consumed twice, and a dropped one never.
-}
unsafeInplace ::
  (α >= β) =>
  (forall s. V.MVector s a -> ST s ()) %1 ->
  Mut α (Vector a) %1 ->
  BO β (Mut α (Vector a))
{-# INLINE unsafeInplace #-}
unsafeInplace = Unsafe.toLinear2 \f (UnsafeAlias v) -> Control.do
  !() <- unsafeSTToBO $ f $ content $ coerceLin v
  Control.pure (UnsafeAlias v)

{- | \(O(n)\), plus the callback. Run a borrowing computation over a boxed mutable vector from @vector@, in place.

The callback sees the storage as an element-owning 'Vector', whose operations accept and hand out linearly owned elements.
So that nothing linearly owned is left behind in storage the caller keeps as GC-owned, every element passes through 'move' on the way out; that is one extra pass over the vector.

This is unsafe because the caller keeps the storage.
If the computation throws, parallel computations it started with 'parBO' may still be writing to the vector for a while afterwards, and the elements have not been moved; after catching an exception from it, do not read or reuse the vector.
'modifyBoxedVector' works on a private copy instead and has no such obligation.
-}
unsafeModifyBoxedMVector ::
  (Movable a) =>
  (forall α. Mut α (Vector a) %1 -> BO α ()) %1 ->
  V.MVector s a %1 ->
  ST s ()
{-# INLINE unsafeModifyBoxedMVector #-}
unsafeModifyBoxedMVector f =
  Unsafe.toLinear \v -> unsafeBOToST Control.do
    let storage = unsafeCoerceVector v
    () <- f (UnsafeAlias (Vector storage))
    unsafeSystemIOToBO (moveElements 0 (MV.length storage) storage)

unsafeCoerceVector :: MV.MVector s a %1 -> MV.MVector RealWorld a
unsafeCoerceVector = Unsafe.coerce

{- | \(O(n)\), plus the callback. Run a borrowing computation over a copy of a boxed vector from @vector@, and return the result.

As with 'unsafeModifyBoxedMVector', every element passes through 'move' on the way out, since the callback sees an element-owning 'Vector'.
The copy is private to this call, so it is safe even when the computation throws.
-}
modifyBoxedVector ::
  (Movable a) =>
  (forall α. Mut α (Vector a) %1 -> BO α ()) ->
  V.Vector a ->
  V.Vector a
{-# INLINE modifyBoxedVector #-}
modifyBoxedVector f = V.modify (\x -> unsafeModifyBoxedMVector f x)

{- | Renamed to 'unsafeModifyBoxedMVector' in 0.2.0.0; any use is a compile error that names the replacement and its obligation.

The name without @unsafe@ is what hid that obligation, so it is not kept even as a deprecated alias.
-}
modifyBoxedMVector ::
  ( Unsatisfiable
      ( 'Text "modifyBoxedMVector was renamed to unsafeModifyBoxedMVector in pure-borrow 0.2.0.0, and now requires Movable a."
          ':$$: 'Text "The caller keeps the storage: if the callback throws, do not read or reuse the MVector afterwards."
          ':$$: 'Text "To modify a GC-owned vector without that obligation, use modifyBoxedVector."
      )
  ) =>
  a
modifyBoxedMVector = unsatisfiable

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
