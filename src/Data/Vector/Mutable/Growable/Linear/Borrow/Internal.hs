{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Mutable.Growable.Linear.Borrow.Internal (
  module Data.Vector.Mutable.Growable.Linear.Borrow.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (aff, pop)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Internal (unsafeSrunBO_)
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Control.Syntax.DataFlow qualified as DataFlow
import Data.IntSet qualified as IntSet
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Linear.Borrow qualified as Fixed
import Data.Vector.Mutable.Linear.Borrow.Internal qualified as Fixed.Internal
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError
import Prelude.Linear hiding (getContents, head, last)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data Header a where
  Header ::
    {-# UNPACK #-} !Int ->
    !(MV.IOVector a) %1 ->
    Header a

-- | A linearly owned boxed vector with a stable header and replaceable backing allocation.
data GrowableVector a where
  GrowableVector :: !(Ref.Ref (Header a)) %1 -> GrowableVector a

type role Header nominal

type role GrowableVector nominal

instance LinearOnly (GrowableVector a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  (Unsatisfiable (ShowType (GrowableVector a) :<>: Text " cannot be copied!")) =>
  Copyable (GrowableVector a)
  where
  copy = unsatisfiable

instance (Consumable a) => Consumable (GrowableVector a) where
  consume =
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer -> consumeInitialized logicalSize buffer
  {-# INLINE consume #-}

allocateBuffer :: Int -> Linearly %1 -> MV.IOVector a
{-# NOINLINE allocateBuffer #-}
allocateBuffer =
  GHC.noinline \count linear ->
    linear `lseq` unsafePerformIO (MV.unsafeNew count)

cloneBuffer :: V.Vector a -> Linearly %1 -> MV.IOVector a
{-# NOINLINE cloneBuffer #-}
cloneBuffer =
  GHC.noinline \source linear ->
    linear `lseq` unsafePerformIO (V.thaw source)

-- | \(O(1)\). Construct an empty vector with zero capacity.
empty :: Linearly %1 -> GrowableVector a
{-# NOINLINE empty #-}
empty = withCapacity 0

-- | \(O(n)\). Construct @n@ initialized elements. The count must be non-negative.
constant ::
  Int ->
  a ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE constant #-}
constant = GHC.noinline \count value linear ->
  fromVector (V.replicate count value) linear

-- | \(O(n)\). Construct a vector from a list.
fromList ::
  [a] ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE fromList #-}
fromList = GHC.noinline \values linear ->
  fromVector (V.fromList values) linear

{- | \(O(1)\). Construct an empty vector with the requested capacity.

The capacity must be non-negative. No element in the spare allocation is
considered initialized.
-}
withCapacity :: (HasCallStack) => Int -> Linearly %1 -> GrowableVector a
{-# NOINLINE withCapacity #-}
withCapacity = GHC.noinline \requested linear ->
  if requested < 0
    then error ("withCapacity: negative capacity " <> show requested) linear
    else
      dup linear & \(bufferLinear, refLinear) ->
        GrowableVector
          (Ref.new (Header 0 (allocateBuffer requested bufferLinear)) refLinear)

-- | \(O(n)\). Copy all elements of an immutable boxed vector.
fromVector ::
  V.Vector a ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE fromVector #-}
fromVector = GHC.noinline \source linear ->
  dup linear & \(bufferLinear, refLinear) ->
    GrowableVector
      ( Ref.new
          (Header (V.length source) (cloneBuffer source bufferLinear))
          refLinear
      )

{- | \(O(1)\). Take ownership of a boxed mutable vector without copying.

The complete source is treated as initialized. The caller must not retain any
alias that can access the source allocation.
-}
unsafeFromMutable ::
  MV.MVector state a %1 ->
  Linearly %1 ->
  GrowableVector a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear \source linear ->
    GrowableVector
      ( Ref.new
          (Header (MV.length source) (Unsafe.coerce source))
          linear
      )

{- | \(O(1)\). Unsafely take ownership of an immutable boxed vector's storage.

The complete source is treated as initialized. No immutable alias may be read
after this operation, because subsequent growable mutation reuses its storage.
-}
unsafeFromVector ::
  V.Vector a %1 ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector =
  GHC.noinline $
    Unsafe.toLinear \source linear ->
      GrowableVector
        ( Ref.new
            ( Header
                (V.length source)
                (unsafePerformIO (V.unsafeThaw source))
            )
            linear
        )

{- | \(O(n)\). Move every initialized element into GC ownership, then freeze
exactly that prefix.

Spare capacity is neither exposed nor materialized.
-}
toVector ::
  (Movable a) =>
  GrowableVector a %1 ->
  Ur (V.Vector a)
{-# NOINLINE toVector #-}
toVector =
  GHC.noinline $
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer ->
          let !frozen =
                unsafePerformIO do
                  moveInitialized logicalSize buffer
                  V.unsafeFreeze (MV.unsafeTake logicalSize buffer)
           in Ur frozen

-- | \(O(n)\). Consume the owner and materialize its initialized prefix as a list.
toList ::
  (Movable a) =>
  GrowableVector a %1 ->
  Ur [a]
{-# INLINE toList #-}
toList = Ur.lift V.toList . toVector

moveInitialized ::
  (Movable a) =>
  Int ->
  MV.IOVector a ->
  NonLinear.IO ()
{-# INLINE moveInitialized #-}
moveInitialized !logicalSize buffer = go 0
  where
    go !index
      | index >= logicalSize = NonLinear.pure ()
      | otherwise = do
          value <- MV.unsafeRead buffer index
          case move value of
            Ur !moved -> MV.unsafeWrite buffer index moved
          go (index + 1)

consumeInitialized ::
  (Consumable a) =>
  Int ->
  MV.IOVector a %1 ->
  ()
{-# INLINE consumeInitialized #-}
consumeInitialized =
  Unsafe.toLinear2 \logicalSize buffer ->
    let go !index
          | index >= logicalSize = NonLinear.pure ()
          | otherwise = do
              value <- MV.unsafeRead buffer index
              let !() = consume value
              go (index + 1)
     in unsafePerformIO (go 0)

toRefMut ::
  Mut α (GrowableVector a) %1 ->
  Mut α (Ref.Ref (Header a))
{-# INLINE toRefMut #-}
toRefMut =
  unsafeMapAlias
    (Unsafe.toLinear \(GrowableVector ref) -> ref)

fromRefMut ::
  Mut α (Ref.Ref (Header a)) %1 ->
  Mut α (GrowableVector a)
{-# INLINE fromRefMut #-}
fromRefMut =
  unsafeMapAlias
    (Unsafe.toLinear GrowableVector)

withHeader ::
  (α >= β) =>
  (Header a %1 -> BO β (result, Header a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE withHeader #-}
withHeader action vector = Control.do
  (result, ref) <- RefBorrow.update action (toRefMut vector)
  Control.pure (result, fromRefMut ref)

-- | \(O(1)\). Return the number of initialized elements and thread the borrow.
size ::
  Borrow bk α (GrowableVector a) %1 ->
  (Ur Int, Borrow bk α (GrowableVector a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize _, duplicateRef) ->
        pop (aff duplicateRef) `lseq` (Ur logicalSize, vector)

-- | \(O(1)\). Return the backing allocation size and thread the borrow.
capacity ::
  Borrow bk α (GrowableVector a) %1 ->
  (Ur Int, Borrow bk α (GrowableVector a))
{-# INLINE capacity #-}
capacity =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq` (Ur (MV.length buffer), vector)

{- | Borrow the element at an index in the initialized prefix.

This consumes the growable borrow. The growable owner can be recovered only
through its enclosing lender after the returned element borrow ends. Use
'withContent' for repeated no-growth access.
-}
get ::
  (HasCallStack, α >= β) =>
  Int ->
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE get #-}
get index vector = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  if index < 0 || index >= logicalSize
    then
      error
        ( "get: index "
            <> show index
            <> " out of bounds for length "
            <> show logicalSize
        )
        vector
    else unsafeGet index vector

-- | Unchecked 'get'. The index must satisfy @0 <= index < size@.
unsafeGet ::
  (α >= β) =>
  Int ->
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeGet #-}
unsafeGet =
  Unsafe.toLinear2 \index (UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef)
          `lseq` UnsafeAlias
          Control.<$> unsafeSystemIOToBO (MV.unsafeRead buffer index)

-- | Borrow the first initialized element. Fails when the vector is empty.
head ::
  (HasCallStack, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Borrow the last initialized element. Fails when the vector is empty.
last ::
  (HasCallStack, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE last #-}
last vector = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  if logicalSize <= 0
    then error "last: empty vector" vector
    else unsafeGet (logicalSize - 1) vector

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeLast #-}
unsafeLast vector = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  unsafeGet (logicalSize - 1) vector

-- | Copy the element at an index through a shared borrow.
copyAt ::
  (HasCallStack, Copyable a, α >= β) =>
  Int ->
  Share α (GrowableVector a) ->
  BO β (Ur a)
{-# INLINE copyAt #-}
copyAt index = checkedCopyAt "copyAt" index

-- | Unchecked 'copyAt'. The index must satisfy @0 <= index < size@.
unsafeCopyAt ::
  (Copyable a, α >= β) =>
  Int ->
  Share α (GrowableVector a) ->
  BO β (Ur a)
{-# INLINE unsafeCopyAt #-}
unsafeCopyAt =
  Unsafe.toLinear2 \index (UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef)
          `lseq` unsafeSystemIOToBO do
            !value <- MV.unsafeRead buffer index
            let !copied = copy (UnsafeAlias value)
            NonLinear.pure (Ur copied)

checkedCopyAt ::
  (HasCallStack, Copyable a, α >= β) =>
  NonLinear.String ->
  Int ->
  Share α (GrowableVector a) ->
  BO β (Ur a)
{-# INLINE checkedCopyAt #-}
checkedCopyAt =
  Unsafe.toLinear3 \operation index (UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize buffer, duplicateRef) ->
        pop (aff duplicateRef)
          `lseq` if index < 0 || index >= logicalSize
            then
              error
                ( operation
                    <> ": index "
                    <> show index
                    <> " out of bounds for length "
                    <> show logicalSize
                )
                buffer
            else unsafeSystemIOToBO do
              !value <- MV.unsafeRead buffer index
              let !copied = copy (UnsafeAlias value)
              NonLinear.pure (Ur copied)

-- | Copy the element at an index and return the mutable growable borrow.
copyAtMut ::
  (HasCallStack, Copyable a, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Ur a, Mut α (GrowableVector a))
{-# INLINE copyAtMut #-}
copyAtMut index vector = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  if index < 0 || index >= logicalSize
    then
      error
        ( "copyAtMut: index "
            <> show index
            <> " out of bounds for length "
            <> show logicalSize
        )
        vector
    else unsafeCopyAtMut index vector

-- | Unchecked 'copyAtMut'. The index must satisfy @0 <= index < size@.
unsafeCopyAtMut ::
  (Copyable a, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Ur a, Mut α (GrowableVector a))
{-# INLINE unsafeCopyAtMut #-}
unsafeCopyAtMut =
  Unsafe.toLinear2 \index vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef)
          `lseq` unsafeSystemIOToBO do
            !value <- MV.unsafeRead buffer index
            let !copied = copy (UnsafeAlias value)
            NonLinear.pure (Ur copied, vector)

-- | Replace an initialized element and return the displaced value.
set ::
  (HasCallStack, α >= β) =>
  Int ->
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (a, Mut α (GrowableVector a))
{-# INLINE set #-}
set index value vector = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  if index < 0 || index >= logicalSize
    then
      error
        ( "set: index "
            <> show index
            <> " out of bounds for length "
            <> show logicalSize
        )
        value
        vector
    else unsafeSet index value vector

-- | Unchecked 'set'. The index must satisfy @0 <= index < size@.
unsafeSet ::
  (α >= β) =>
  Int ->
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (a, Mut α (GrowableVector a))
{-# INLINE unsafeSet #-}
unsafeSet =
  Unsafe.toLinear3 \index !value vector ->
    withHeader
      ( Unsafe.toLinear \(Header logicalSize buffer) ->
          unsafeSystemIOToBO do
            !oldValue <- MV.unsafeRead buffer index
            MV.unsafeWrite buffer index value
            NonLinear.pure (oldValue, Header logicalSize buffer)
      )
      vector

-- | Linearly transform an initialized element and return an auxiliary result.
update ::
  (HasCallStack, α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE update #-}
update index action vector = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  if index < 0 || index >= logicalSize
    then
      error
        ( "update: index "
            <> show index
            <> " out of bounds for length "
            <> show logicalSize
        )
        action
        vector
    else unsafeUpdate index action vector

-- | Unchecked 'update'. The index must satisfy @0 <= index < size@.
unsafeUpdate ::
  (α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index action vector =
  withHeader
    ( Unsafe.toLinear \(Header logicalSize buffer) -> Control.do
        value <- unsafeSystemIOToBO (MV.unsafeRead buffer index)
        (!result, !updatedValue) <- action value
        buffer <- writeAt index updatedValue buffer
        Control.pure (result, Header logicalSize buffer)
    )
    vector

-- | Linearly transform an initialized element.
modify ::
  (HasCallStack, α >= β) =>
  Int ->
  (a %1 -> a) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE modify #-}
modify index function vector = Control.do
  ((), vector) <-
    update
      index
      (Control.pure . ((),) . function)
      vector
  Control.pure vector

-- | Unchecked 'swap'. Both indices must satisfy @0 <= index < size@.
unsafeSwap ::
  (α >= β) =>
  Mut α (GrowableVector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector a))
{-# INLINE unsafeSwap #-}
unsafeSwap =
  Unsafe.toLinear3 \vector first second -> Control.do
    ((), vector) <-
      withHeader
        ( Unsafe.toLinear \(Header logicalSize buffer) ->
            unsafeSystemIOToBO do
              MV.unsafeSwap buffer first second
              NonLinear.pure ((), Header logicalSize buffer)
        )
        vector
    Control.pure vector

-- | Swap two initialized elements.
swap ::
  (HasCallStack, α >= β) =>
  Mut α (GrowableVector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector a))
{-# INLINE swap #-}
swap vector first second = DataFlow.do
  (Ur logicalSize, vector) <- size vector
  if first
    < 0
    || first
    >= logicalSize
    || second
    < 0
    || second
    >= logicalSize
    then
      error
        ( "swap: indices "
            <> show (first, second)
            <> " out of bounds for length "
            <> show logicalSize
        )
        vector
    else unsafeSwap vector first second

{- | Borrow several initialized elements mutably without validation.

Every index must satisfy @0 <= index < size@, and the indices must be
pairwise distinct. Violating distinctness can create aliased mutable borrows
and a data race when they are used in parallel.
-}
unsafeIndicesMut ::
  (α >= β) =>
  Mut α (GrowableVector a) %1 ->
  [Int] %1 ->
  BO β [Mut α a]
{-# INLINE unsafeIndicesMut #-}
unsafeIndicesMut vector =
  Fixed.unsafeIndicesMut (getContents vector)

{- | Borrow several initialized elements mutably.

Fails if any index is out of bounds or if an index occurs more than once.
-}
indicesMut ::
  (HasCallStack, α >= β) =>
  Mut α (GrowableVector a) %1 ->
  [Int] %1 ->
  BO β [Mut α a]
{-# INLINE indicesMut #-}
indicesMut =
  Unsafe.toLinear2 \vector indices ->
    case size vector of
      (Ur logicalSize, vector)
        | any
            ( \index ->
                move index & \(Ur index) ->
                  index < 0 || index >= logicalSize
            )
            indices ->
            error
              ( "indicesMut: indices out of bounds: "
                  <> show indices
                  <> " for length "
                  <> show logicalSize
              )
              vector
        | NonLinear.length indices
            > IntSet.size (IntSet.fromList indices) ->
            error ("indicesMut: duplicate indices: " <> show indices) vector
        | otherwise ->
            Fixed.unsafeIndicesMut (getContents vector) indices

{- | Ensure that the absolute capacity is at least the requested value.

The requested capacity must be non-negative. Logical size and initialized
contents do not change. Reallocation destructively transfers the initialized
prefix into fresh storage.
-}
reserve ::
  (HasCallStack, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE reserve #-}
reserve requested vector
  | requested < 0 =
      error ("reserve: negative capacity " <> show requested) vector
  | otherwise = Control.do
      ((), vector) <-
        withHeader
          ( Unsafe.toLinear \(Header logicalSize buffer) -> Control.do
              grown <- growTo logicalSize requested buffer
              Control.pure ((), Header logicalSize grown)
          )
          vector
      Control.pure vector

{- | Ensure capacity for at least the current size plus the requested amount.

The additional amount must be non-negative. Logical size and initialized
contents do not change.
-}
reserveAdditional ::
  (HasCallStack, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE reserveAdditional #-}
reserveAdditional additional vector
  | additional < 0 =
      error
        ("reserveAdditional: negative additional capacity " <> show additional)
        vector
  | otherwise = Control.do
      ((), vector) <-
        withHeader
          ( Unsafe.toLinear \(Header logicalSize buffer) ->
              let !required =
                    checkedAdd "reserveAdditional" logicalSize additional
               in Control.do
                    grown <- growTo logicalSize required buffer
                    Control.pure ((), Header logicalSize grown)
          )
          vector
      Control.pure vector

{- | Append one linearly supplied element to the initialized prefix.

Reallocation, when required, destructively transfers the old initialized
prefix into fresh storage.
-}
push ::
  (HasCallStack, α >= β) =>
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE push #-}
push =
  Unsafe.toLinear2 \ !value vector -> Control.do
    ((), vector) <-
      withHeader
        ( Unsafe.toLinear \(Header logicalSize buffer) ->
            let !required = checkedAdd "push" logicalSize 1
                !target = growthTarget (MV.length buffer) required
             in Control.do
                  grown <- growTo logicalSize target buffer
                  grown <- writeAt logicalSize value grown
                  Control.pure ((), Header required grown)
        )
        vector
    Control.pure vector

-- | Append copies of all elements of an immutable boxed vector.
extend ::
  (HasCallStack, α >= β) =>
  V.Vector a ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE extend #-}
extend source vector = Control.do
  ((), vector) <-
    withHeader
      ( Unsafe.toLinear \(Header logicalSize buffer) ->
          let !sourceSize = V.length source
              !required = checkedAdd "extend" logicalSize sourceSize
              !target = growthTarget (MV.length buffer) required
           in Control.do
                grown <- growTo logicalSize target buffer
                grown <- copyImmutableInto source logicalSize grown
                Control.pure ((), Header required grown)
      )
      vector
  Control.pure vector

copyImmutable :: V.Vector a -> Int -> MV.IOVector a -> NonLinear.IO ()
{-# INLINE copyImmutable #-}
copyImmutable source offset target =
  V.copy (MV.unsafeSlice offset (V.length source) target) source

copyImmutableInto ::
  V.Vector a ->
  Int ->
  MV.IOVector a %1 ->
  BO β (MV.IOVector a)
{-# INLINE copyImmutableInto #-}
copyImmutableInto source offset =
  Unsafe.toLinear \target -> unsafeSystemIOToBO do
    copyImmutable source offset target
    NonLinear.pure target

writeAt ::
  Int ->
  a %1 ->
  MV.IOVector a %1 ->
  BO β (MV.IOVector a)
{-# INLINE writeAt #-}
writeAt =
  Unsafe.toLinear3 \index value target -> unsafeSystemIOToBO do
    MV.unsafeWrite target index value
    NonLinear.pure target

growTo ::
  Int ->
  Int ->
  MV.IOVector a %1 ->
  BO β (MV.IOVector a)
{-# INLINE growTo #-}
growTo =
  Unsafe.toLinear3 \logicalSize requested buffer ->
    let !oldCapacity = MV.length buffer
     in if requested <= oldCapacity
          then Control.pure buffer
          else unsafeSystemIOToBO do
            grown <- MV.unsafeNew requested
            MV.unsafeCopy
              (MV.unsafeTake logicalSize grown)
              (MV.unsafeTake logicalSize buffer)
            NonLinear.pure grown

growthTarget :: Int -> Int -> Int
{-# INLINE growthTarget #-}
growthTarget oldCapacity required
  | required <= oldCapacity = oldCapacity
  | oldCapacity <= 0 = required `max` 1
  | oldCapacity > maxBound `quot` 2 = required
  | otherwise = required `max` (oldCapacity * 2)

checkedAdd :: (HasCallStack) => NonLinear.String -> Int -> Int -> Int
{-# INLINE checkedAdd #-}
checkedAdd operation left right
  | right > maxBound - left =
      error (operation <> ": capacity overflow")
  | otherwise = left + right

{- | Project a growable borrow to a fixed borrow of its initialized prefix.

This consumes one occurrence of the growable borrow, preserves its borrow kind
and lifetime, and performs one header read. The result exposes neither spare
capacity nor growth. A mutable result may be split using the fixed-vector API;
the mutable growable owner becomes recoverable only after every resulting
fixed borrow has ended. A shared input follows the ordinary unrestricted
'Share' rules.
-}
getContents ::
  Borrow bk α (GrowableVector a) %1 ->
  Borrow bk α (Fixed.Vector a)
{-# INLINE getContents #-}
getContents =
  Unsafe.toLinear \(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize buffer, duplicateRef) ->
        pop (aff duplicateRef)
          `lseq` UnsafeAlias
            (Fixed.Internal.unsafeFromMutableSlice 0 logicalSize buffer)

{- | Borrow the fixed initialized prefix in a rank-2 no-growth scope.

The callback and returned growable borrow preserve the input borrow kind. The
callback receives one linear occurrence for either kind; use 'move' on shared
content when unrestricted use is desired. For a mutable input, the growable
borrow is restored only after the callback result is produced and the fixed
view has ended.

Note [Uniformly linear content callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ideally the callback arrow would use @BorrowMultiplicity bk@, making a
shared callback unrestricted. GHC 9.12 rejects that signature because type
families cannot witness multiplicity equality (GHC #19517). Keep one linear
callback occurrence for both borrow kinds until that limitation is removed;
shared callers can use 'move' to recover unrestricted use.
-}
withContent ::
  Borrow bk α (GrowableVector a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector a) %1 ->
    BO (β /\ α) result
  ) %1 ->
  BO α (result, Borrow bk α (GrowableVector a))
{-# INLINE withContent #-}
withContent =
  Unsafe.toLinear2 \vector action ->
    unsafeSrunBO_ $
      action (getContents (Unsafe.coerce vector))
        Control.<&> \result -> (result, vector)

-- | A result-discarding variant of 'withContent'.
withContent_ ::
  (Consumable result) =>
  Borrow bk α (GrowableVector a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector a) %1 ->
    BO (β /\ α) result
  ) %1 ->
  BO α (Borrow bk α (GrowableVector a))
{-# INLINE withContent_ #-}
withContent_ vector action =
  withContent vector action Control.<&> \(result, vector) ->
    consume result `lseq` vector
