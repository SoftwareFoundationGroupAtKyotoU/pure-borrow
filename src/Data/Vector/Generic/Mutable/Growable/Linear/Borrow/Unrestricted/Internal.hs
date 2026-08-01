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

module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.Internal (
  module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (aff, pop)
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Internal (unsafeSrunBO_)
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Clone
import Control.Monad.Borrow.Pure.Copyable
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as GM
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Fixed
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.Internal qualified as Fixed.Internal
import Data.Vector.Mutable (RealWorld)
import GHC.Exts qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.TypeError
import Prelude.Linear hiding (getContents, head, last)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data Header v a where
  Header ::
    {-# UNPACK #-} !Int ->
    !(G.Mutable v RealWorld a) %1 ->
    Header v a

-- | A growable vector with a stable header and replaceable generic backing.
data GrowableVector v a where
  GrowableVector ::
    !(Ref.Ref (Header v a)) %1 ->
    GrowableVector v a

type role Header nominal nominal

type role GrowableVector nominal nominal

instance LinearOnly (GrowableVector v a) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance
  ( Unsatisfiable
      (ShowType (GrowableVector v a) :<>: Text " cannot be copied!")
  ) =>
  Copyable (GrowableVector v a)
  where
  copy = unsatisfiable

instance Consumable (GrowableVector v a) where
  consume = Unsafe.toLinear \_ -> ()
  {-# INLINE consume #-}

instance (G.Vector v a) => Clone (GrowableVector v a) where
  clone =
    Unsafe.toLinear \(UnsafeAlias (GrowableVector ref)) ->
      case Ref.unsafeReadRef ref of
        (Header logicalSize buffer, duplicateRef) ->
          pop (aff duplicateRef) `lseq` Control.do
            cloned <-
              unsafeSystemIOToBO do
                target <- GM.unsafeNew (GM.length buffer)
                GM.unsafeCopy
                  (GM.unsafeTake logicalSize target)
                  (GM.unsafeTake logicalSize buffer)
                NonLinear.pure target
            linear <- askLinearly
            Control.pure
              (GrowableVector (Ref.new (Header logicalSize cloned) linear))
  {-# INLINE clone #-}

allocateBuffer ::
  (G.Vector v a) =>
  Int ->
  Linearly %1 ->
  G.Mutable v RealWorld a
{-# NOINLINE allocateBuffer #-}
allocateBuffer =
  GHC.noinline \count linear ->
    linear `lseq` unsafePerformIO (GM.unsafeNew count)

cloneBuffer ::
  (G.Vector v a) =>
  v a ->
  Linearly %1 ->
  G.Mutable v RealWorld a
{-# NOINLINE cloneBuffer #-}
cloneBuffer =
  GHC.noinline \source linear ->
    linear `lseq` unsafePerformIO (G.thaw source)

-- | \(O(1)\). Construct an empty vector with zero capacity.
empty :: (G.Vector v a) => Linearly %1 -> GrowableVector v a
{-# NOINLINE empty #-}
empty = withCapacity 0

{- | \(O(n)\). Construct @count@ initialized copies of a value.

As in @vector@, a negative count produces an empty vector.
-}
constant ::
  (G.Vector v a) =>
  Int ->
  a ->
  Linearly %1 ->
  GrowableVector v a
{-# NOINLINE constant #-}
constant =
  GHC.noinline \count value linear ->
    fromVector (G.replicate count value) linear

-- | \(O(n)\). Copy an unrestricted list into a new vector.
fromList ::
  (G.Vector v a) =>
  [a] ->
  Linearly %1 ->
  GrowableVector v a
{-# NOINLINE fromList #-}
fromList =
  GHC.noinline \values linear ->
    fromVector (G.fromList values) linear

{- | \(O(1)\). Construct an empty vector with the requested capacity.

The capacity must be non-negative. Spare storage is not considered initialized.
-}
withCapacity ::
  (HasCallStack, G.Vector v a) =>
  Int ->
  Linearly %1 ->
  GrowableVector v a
{-# NOINLINE withCapacity #-}
withCapacity =
  GHC.noinline \requested linear ->
    if requested < 0
      then error ("withCapacity: negative capacity " <> show requested) linear
      else
        dup linear & \(bufferLinear, refLinear) ->
          GrowableVector
            ( Ref.new
                (Header 0 (allocateBuffer requested bufferLinear))
                refLinear
            )

-- | \(O(n)\). Copy an immutable vector into a new growable owner.
fromVector ::
  (G.Vector v a) =>
  v a ->
  Linearly %1 ->
  GrowableVector v a
{-# NOINLINE fromVector #-}
fromVector =
  GHC.noinline \source linear ->
    dup linear & \(bufferLinear, refLinear) ->
      GrowableVector
        ( Ref.new
            (Header (G.length source) (cloneBuffer source bufferLinear))
            refLinear
        )

{- | \(O(1)\). Take ownership of a mutable vector without copying.

The complete source slice is treated as initialized. The caller must not retain
any alias that can access its allocation.
-}
unsafeFromMutable ::
  (G.Vector v a) =>
  G.Mutable v state a %1 ->
  Linearly %1 ->
  GrowableVector v a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear \source linear ->
    GrowableVector
      ( Ref.new
          (Header (GM.length source) (Unsafe.coerce source))
          linear
      )

{- | \(O(1)\). Unsafely take ownership of an immutable vector's storage.

The complete source is initialized. No immutable alias may be observed after
this operation because subsequent growth and mutation reuse its storage.
-}
unsafeFromVector ::
  (G.Vector v a) =>
  v a %1 ->
  Linearly %1 ->
  GrowableVector v a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector =
  GHC.noinline $
    Unsafe.toLinear \source linear ->
      GrowableVector
        ( Ref.new
            ( Header
                (G.length source)
                (unsafePerformIO (G.unsafeThaw source))
            )
            linear
        )

{- | \(O(1)\). Consume the owner and freeze exactly its initialized prefix.

No element crosses an ownership boundary: entries were GC-owned already. Spare
capacity is neither exposed nor copied.
-}
toVector ::
  (G.Vector v a) =>
  GrowableVector v a %1 ->
  Ur (v a)
{-# NOINLINE toVector #-}
toVector =
  GHC.noinline $
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer ->
          Ur
            ( unsafePerformIO
                (G.unsafeFreeze (GM.unsafeTake logicalSize buffer))
            )

-- | \(O(n)\). Consume the owner and return its initialized prefix as a list.
toList ::
  (G.Vector v a) =>
  GrowableVector v a %1 ->
  Ur [a]
{-# INLINE toList #-}
toList = Ur.lift G.toList . toVector

{- | \(O(n)\). Copy the initialized prefix into an immutable vector and thread
the live growable borrow.
-}
copyToVector ::
  (G.Vector v a, α >= β) =>
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur (v a), Borrow bk α (GrowableVector v a))
{-# INLINE copyToVector #-}
copyToVector =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            snapshot <- G.freeze (GM.unsafeTake logicalSize buffer)
            NonLinear.pure (Ur snapshot, vector)

toRefMut ::
  Mut α (GrowableVector v a) %1 ->
  Mut α (Ref.Ref (Header v a))
{-# INLINE toRefMut #-}
toRefMut =
  unsafeMapAlias
    (Unsafe.toLinear \(GrowableVector ref) -> ref)

fromRefMut ::
  Mut α (Ref.Ref (Header v a)) %1 ->
  Mut α (GrowableVector v a)
{-# INLINE fromRefMut #-}
fromRefMut =
  unsafeMapAlias
    (Unsafe.toLinear GrowableVector)

withHeader ::
  (α >= β) =>
  (Header v a %1 -> BO β (result, Header v a)) %1 ->
  Mut α (GrowableVector v a) %1 ->
  BO β (result, Mut α (GrowableVector v a))
{-# INLINE withHeader #-}
withHeader action vector = Control.do
  (result, ref) <- RefBorrow.update action (toRefMut vector)
  Control.pure (result, fromRefMut ref)

-- | \(O(1)\). Return the number of initialized elements and thread the borrow.
size ::
  Borrow bk α (GrowableVector v a) %1 ->
  (Ur Int, Borrow bk α (GrowableVector v a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize _, duplicateRef) ->
        pop (aff duplicateRef) `lseq` (Ur logicalSize, vector)

-- | \(O(1)\). Return the backing allocation size and thread the borrow.
capacity ::
  (G.Vector v a) =>
  Borrow bk α (GrowableVector v a) %1 ->
  (Ur Int, Borrow bk α (GrowableVector v a))
{-# INLINE capacity #-}
capacity =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq` (Ur (GM.length buffer), vector)

-- | Read an initialized element and thread the growable borrow.
get ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur a, Borrow bk α (GrowableVector v a))
{-# INLINE get #-}
get index vector =
  case size vector of
    (Ur logicalSize, vector) ->
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
  (G.Vector v a, α >= β) =>
  Int ->
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur a, Borrow bk α (GrowableVector v a))
{-# INLINE unsafeGet #-}
unsafeGet index =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            value <- GM.unsafeRead buffer index
            NonLinear.pure (Ur value, vector)

-- | Read the first initialized element.
head ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur a, Borrow bk α (GrowableVector v a))
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (G.Vector v a, α >= β) =>
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur a, Borrow bk α (GrowableVector v a))
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Read the last initialized element.
last ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur a, Borrow bk α (GrowableVector v a))
{-# INLINE last #-}
last vector =
  case size vector of
    (Ur logicalSize, vector) ->
      if logicalSize <= 0
        then error "last: empty vector" vector
        else unsafeGet (logicalSize - 1) vector

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (G.Vector v a, α >= β) =>
  Borrow bk α (GrowableVector v a) %1 ->
  BO β (Ur a, Borrow bk α (GrowableVector v a))
{-# INLINE unsafeLast #-}
unsafeLast vector =
  case size vector of
    (Ur logicalSize, vector) -> unsafeGet (logicalSize - 1) vector

-- | Read an initialized element through a shared borrow.
copyAt ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Share α (GrowableVector v a) ->
  BO β (Ur a)
{-# INLINE copyAt #-}
copyAt index vector = Control.do
  (value, vector) <- get index vector
  Control.pure (consume vector `lseq` value)

-- | Unchecked 'copyAt'. The index must satisfy @0 <= index < size@.
unsafeCopyAt ::
  (G.Vector v a, α >= β) =>
  Int ->
  Share α (GrowableVector v a) ->
  BO β (Ur a)
{-# INLINE unsafeCopyAt #-}
unsafeCopyAt index vector = Control.do
  (value, vector) <- unsafeGet index vector
  Control.pure (consume vector `lseq` value)

-- | Read an initialized element and retain the mutable growable borrow.
copyAtMut ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Ur a, Mut α (GrowableVector v a))
{-# INLINE copyAtMut #-}
copyAtMut = get

-- | Unchecked 'copyAtMut'. The index must satisfy @0 <= index < size@.
unsafeCopyAtMut ::
  (G.Vector v a, α >= β) =>
  Int ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Ur a, Mut α (GrowableVector v a))
{-# INLINE unsafeCopyAtMut #-}
unsafeCopyAtMut = unsafeGet

-- | Replace an initialized element and return the displaced value.
set ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Ur a, Mut α (GrowableVector v a))
{-# INLINE set #-}
set index value vector =
  case size vector of
    (Ur logicalSize, vector) ->
      if index < 0 || index >= logicalSize
        then
          error
            ( "set: index "
                <> show index
                <> " out of bounds for length "
                <> show logicalSize
            )
            vector
        else unsafeSet index value vector

-- | Unchecked 'set'. The index must satisfy @0 <= index < size@.
unsafeSet ::
  (G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Ur a, Mut α (GrowableVector v a))
{-# INLINE unsafeSet #-}
unsafeSet index value =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            oldValue <- GM.unsafeExchange buffer index value
            NonLinear.pure (Ur oldValue, vector)

-- | Replace an initialized element and discard the displaced GC-owned value.
write ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE write #-}
write index value vector =
  case size vector of
    (Ur logicalSize, vector) ->
      if index < 0 || index >= logicalSize
        then
          error
            ( "write: index "
                <> show index
                <> " out of bounds for length "
                <> show logicalSize
            )
            vector
        else unsafeWrite index value vector

-- | Unchecked 'write'. The index must satisfy @0 <= index < size@.
unsafeWrite ::
  (G.Vector v a, α >= β) =>
  Int ->
  a ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE unsafeWrite #-}
unsafeWrite index value =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            GM.unsafeWrite buffer index value
            NonLinear.pure vector

-- | Transform an initialized element and return an unrestricted result.
update ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  (a -> BO β (Ur result, Ur a)) ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Ur result, Mut α (GrowableVector v a))
{-# INLINE update #-}
update index action vector =
  case size vector of
    (Ur logicalSize, vector) ->
      if index < 0 || index >= logicalSize
        then
          error
            ( "update: index "
                <> show index
                <> " out of bounds for length "
                <> show logicalSize
            )
            vector
        else unsafeUpdate index action vector

-- | Unchecked 'update'. The index must satisfy @0 <= index < size@.
unsafeUpdate ::
  (G.Vector v a, α >= β) =>
  Int ->
  (a -> BO β (Ur result, Ur a)) ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Ur result, Mut α (GrowableVector v a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index action =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq` Control.do
          Ur value <-
            unsafeSystemIOToBO do
              value <- GM.unsafeRead buffer index
              NonLinear.pure (Ur value)
          (result, Ur updatedValue) <- action value
          () <- unsafeSystemIOToBO (GM.unsafeWrite buffer index updatedValue)
          Control.pure (result, vector)

-- | Transform an initialized element.
modify ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  (a -> a) ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE modify #-}
modify index function vector = Control.do
  (Ur (), vector) <-
    update
      index
      (\value -> Control.pure (Ur (), Ur (function value)))
      vector
  Control.pure vector

-- | Swap two initialized elements.
swap ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Mut α (GrowableVector v a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE swap #-}
swap vector first second =
  case size vector of
    (Ur logicalSize, vector) ->
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

-- | Unchecked 'swap'. Both indices must satisfy @0 <= index < size@.
unsafeSwap ::
  (G.Vector v a, α >= β) =>
  Mut α (GrowableVector v a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE unsafeSwap #-}
unsafeSwap =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) first second ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            GM.unsafeSwap buffer first second
            NonLinear.pure vector

{- | Ensure at least the requested absolute capacity.

The request must be non-negative. Logical size and initialized contents do not
change.
-}
reserve ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
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

The additional amount must be non-negative.
-}
reserveAdditional ::
  (HasCallStack, G.Vector v a, α >= β) =>
  Int ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
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

-- | Append one unrestricted element to the initialized prefix.
push ::
  (HasCallStack, G.Vector v a, α >= β) =>
  a ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE push #-}
push value vector = Control.do
  ((), vector) <-
    withHeader
      ( Unsafe.toLinear \(Header logicalSize buffer) ->
          let !required = checkedAdd "push" logicalSize 1
              !target = growthTarget (GM.length buffer) required
           in Control.do
                grown <- growTo logicalSize target buffer
                grown <- writeAt logicalSize value grown
                Control.pure ((), Header required grown)
      )
      vector
  Control.pure vector

-- | Append all elements of an immutable vector.
extend ::
  (HasCallStack, G.Vector v a, α >= β) =>
  v a ->
  Mut α (GrowableVector v a) %1 ->
  BO β (Mut α (GrowableVector v a))
{-# INLINE extend #-}
extend source vector = Control.do
  ((), vector) <-
    withHeader
      ( Unsafe.toLinear \(Header logicalSize buffer) ->
          let !sourceSize = G.length source
              !required = checkedAdd "extend" logicalSize sourceSize
              !target = growthTarget (GM.length buffer) required
           in Control.do
                grown <- growTo logicalSize target buffer
                grown <- copyImmutableInto source logicalSize grown
                Control.pure ((), Header required grown)
      )
      vector
  Control.pure vector

copyImmutable ::
  (G.Vector v a) =>
  v a ->
  Int ->
  G.Mutable v RealWorld a ->
  NonLinear.IO ()
{-# INLINE copyImmutable #-}
copyImmutable source offset target =
  G.copy
    (GM.unsafeSlice offset (G.length source) target)
    source

copyImmutableInto ::
  (G.Vector v a) =>
  v a ->
  Int ->
  G.Mutable v RealWorld a %1 ->
  BO β (G.Mutable v RealWorld a)
{-# INLINE copyImmutableInto #-}
copyImmutableInto source offset =
  Unsafe.toLinear \target -> unsafeSystemIOToBO do
    copyImmutable source offset target
    NonLinear.pure target

writeAt ::
  (G.Vector v a) =>
  Int ->
  a ->
  G.Mutable v RealWorld a %1 ->
  BO β (G.Mutable v RealWorld a)
{-# INLINE writeAt #-}
writeAt index value =
  Unsafe.toLinear \target -> unsafeSystemIOToBO do
    GM.unsafeWrite target index value
    NonLinear.pure target

growTo ::
  (G.Vector v a) =>
  Int ->
  Int ->
  G.Mutable v RealWorld a %1 ->
  BO β (G.Mutable v RealWorld a)
{-# INLINE growTo #-}
growTo =
  Unsafe.toLinear3 \logicalSize requested buffer ->
    let !oldCapacity = GM.length buffer
     in if requested <= oldCapacity
          then Control.pure buffer
          else unsafeSystemIOToBO do
            grown <- GM.unsafeNew requested
            GM.unsafeCopy
              (GM.unsafeTake logicalSize grown)
              (GM.unsafeTake logicalSize buffer)
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

{- | Project a growable borrow to the fixed initialized prefix.

The result preserves the borrow kind and lifetime and exposes neither spare
capacity nor growth. A mutable result may be split with the fixed unrestricted
vector API. The growable borrow becomes recoverable only after all resulting
fixed borrows have ended.

Each call performs one header read. Where a transaction branches, prefer
projecting once at its entry --

@
let %1 !content = 'getContents' borrow
@

-- over projecting separately inside each branch. Both are correct and consume
the growable occurrence exactly once; the entry form simply gives the
optimizer one read to place rather than one per surviving branch, which
matters for code size in transactions with several control exits.
-}
getContents ::
  (G.Vector v a) =>
  Borrow bk α (GrowableVector v a) %1 ->
  Borrow bk α (Fixed.Vector v a)
{-# INLINE getContents #-}
getContents =
  Unsafe.toLinear \(UnsafeAlias (GrowableVector ref)) ->
    -- SAFETY: unsafeReadRef exposes the current header while returning the same
    -- borrowed Ref handle. Discarding that returned handle does not free the
    -- authoritative header retained by an enclosing lender. The input
    -- growable occurrence is consumed by this projection and cannot be used
    -- for reserve or growth. The result preserves its borrow kind and lifetime
    -- and exposes exactly the initialized prefix, so mutable access cannot
    -- coexist with growth and shared access remains read-only. The fixed safe
    -- API can split this slice but cannot resize it, reveal spare capacity, or
    -- consume/freeze the growable backing owner. Nominal backend and element
    -- roles prevent selecting operations for a different representation.
    case Ref.unsafeReadRef ref of
      (Header logicalSize buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          UnsafeAlias
            (Fixed.Internal.unsafeFromMutableSlice 0 logicalSize buffer)

{- | Borrow the fixed initialized prefix in a rank-2 no-growth scope.

The callback receives one linear occurrence for either borrow kind. Shared
callers may use 'move' to regain unrestricted use. A mutable growable borrow is
restored only after the callback result is produced and the fixed view ends.
-}
withContent ::
  (G.Vector v a) =>
  Borrow bk α (GrowableVector v a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector v a) %1 ->
    BO (β /\ α) result
  ) %1 ->
  BO α (result, Borrow bk α (GrowableVector v a))
{-# INLINE withContent #-}
withContent =
  Unsafe.toLinear2 \vector action ->
    -- SAFETY: the coercion changes only the phantom lifetime of this one
    -- retained borrow occurrence. unsafeSrunBO_ chooses a fresh rigid
    -- sublifetime and invokes action exactly once. Linearity keeps vector
    -- inaccessible until action has consumed every fixed slice and returned;
    -- the result type cannot mention the fresh lifetime. Only then is the
    -- original growable occurrence restored. This is a normal-return
    -- guarantee; the module promises no owner recovery after an exception.
    unsafeSrunBO_ $
      action (getContents (Unsafe.coerce vector))
        Control.<&> \result -> (result, vector)

-- | A result-discarding variant of 'withContent'.
withContent_ ::
  (G.Vector v a, Consumable result) =>
  Borrow bk α (GrowableVector v a) %1 ->
  ( forall β.
    Borrow bk (β /\ α) (Fixed.Vector v a) %1 ->
    BO (β /\ α) result
  ) %1 ->
  BO α (Borrow bk α (GrowableVector v a))
{-# INLINE withContent_ #-}
withContent_ vector action =
  withContent vector action Control.<&> \(result, vector) ->
    consume result `lseq` vector
