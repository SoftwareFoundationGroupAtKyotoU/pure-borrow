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

module Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow.Internal (
  module Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow.Internal,
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
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Unrestricted.Linear qualified as Ur
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Fixed
import Data.Vector.Unboxed.Mutable.Linear.Borrow.Internal qualified as Fixed.Internal
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
    !(UM.IOVector a) %1 ->
    Header a

-- | Growable unboxed linear mutable vector.
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

instance (U.Unbox a, Consumable a) => Consumable (GrowableVector a) where
  consume =
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer -> consumeInitialized logicalSize buffer
  -- Reaches 'unsafePerformIO' through 'consumeInitialized'. See the note
  -- there: this must not be duplicated across call sites.
  {-# NOINLINE consume #-}

allocateBuffer ::
  (U.Unbox a) =>
  Int ->
  Linearly %1 ->
  UM.IOVector a
{-# NOINLINE allocateBuffer #-}
allocateBuffer =
  GHC.noinline \count linear ->
    linear `lseq` unsafePerformIO (UM.unsafeNew count)

cloneBuffer ::
  (U.Unbox a) =>
  U.Vector a ->
  Linearly %1 ->
  UM.IOVector a
{-# NOINLINE cloneBuffer #-}
cloneBuffer =
  GHC.noinline \source linear ->
    linear `lseq` unsafePerformIO (U.thaw source)

-- | \(O(1)\). Construct an empty vector with zero capacity.
empty :: (U.Unbox a) => Linearly %1 -> GrowableVector a
{-# NOINLINE empty #-}
empty = withCapacity 0

-- | \(O(n)\). Construct @n@ initialized copies of a value.
constant ::
  (U.Unbox a) =>
  Int ->
  a ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE constant #-}
constant =
  GHC.noinline \count value linear ->
    fromVector (U.replicate count value) linear

-- | \(O(n)\). Move a linear list into a new vector.
fromList ::
  (U.Unbox a) =>
  [a] %1 ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE fromList #-}
fromList =
  GHC.noinline $
    Unsafe.toLinear2 \values linear ->
      dup linear & \(bufferLinear, refLinear) ->
        case Fixed.fromList values bufferLinear of
          Fixed.Internal.Vector buffer ->
            GrowableVector
              (Ref.new (Header (UM.length buffer) buffer) refLinear)

{- | \(O(1)\). Construct an empty vector with requested capacity.

The capacity must be non-negative. Spare storage is not initialized.
-}
withCapacity ::
  (HasCallStack, U.Unbox a) =>
  Int ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE withCapacity #-}
withCapacity =
  GHC.noinline \requested linear ->
    if requested < 0
      then error ("withCapacity: negative capacity " <> show requested) linear
      else
        dup linear & \(bufferLinear, refLinear) ->
          GrowableVector
            (Ref.new (Header 0 (allocateBuffer requested bufferLinear)) refLinear)

-- | \(O(n)\). Copy an immutable unboxed vector into a new owner.
fromVector ::
  (U.Unbox a) =>
  U.Vector a ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE fromVector #-}
fromVector =
  GHC.noinline \source linear ->
    dup linear & \(bufferLinear, refLinear) ->
      GrowableVector
        ( Ref.new
            (Header (U.length source) (cloneBuffer source bufferLinear))
            refLinear
        )

{- | \(O(1)\). Take ownership of a mutable unboxed vector.

The complete source slice must be initialized, and the caller must retain no
alias or overlapping slice.
-}
unsafeFromMutable ::
  (U.Unbox a) =>
  UM.MVector state a %1 ->
  Linearly %1 ->
  GrowableVector a
{-# INLINE unsafeFromMutable #-}
unsafeFromMutable =
  Unsafe.toLinear \source linear ->
    GrowableVector
      ( Ref.new
          (Header (UM.length source) (Unsafe.coerce source))
          linear
      )

{- | \(O(1)\). Unsafely take ownership of immutable unboxed storage.

No immutable alias, including an overlapping slice, may be observed after
this operation.
-}
unsafeFromVector ::
  (U.Unbox a) =>
  U.Vector a %1 ->
  Linearly %1 ->
  GrowableVector a
{-# NOINLINE unsafeFromVector #-}
unsafeFromVector =
  GHC.noinline $
    Unsafe.toLinear \source linear ->
      GrowableVector
        ( Ref.new
            ( Header
                (U.length source)
                (unsafePerformIO (U.unsafeThaw source))
            )
            linear
        )

{- | \(O(n)\). Move every initialized element into GC ownership, then freeze
that prefix.

Spare capacity is not exposed.
-}
toVector ::
  (U.Unbox a, Movable a) =>
  GrowableVector a %1 ->
  Ur (U.Vector a)
{-# NOINLINE toVector #-}
toVector =
  GHC.noinline $
    Unsafe.toLinear \(GrowableVector ref) ->
      case Ref.free ref of
        Header logicalSize buffer ->
          let !frozen =
                unsafePerformIO do
                  moveInitialized logicalSize buffer
                  U.unsafeFreeze (UM.unsafeTake logicalSize buffer)
           in Ur frozen

-- | \(O(n)\). Consume the owner and materialize its initialized prefix.
toList ::
  (U.Unbox a, Movable a) =>
  GrowableVector a %1 ->
  Ur [a]
{-# INLINE toList #-}
toList = Ur.lift U.toList . toVector

moveInitialized ::
  (U.Unbox a, Movable a) =>
  Int ->
  UM.IOVector a ->
  NonLinear.IO ()
{-# INLINE moveInitialized #-}
moveInitialized !logicalSize buffer = go 0
  where
    go !index
      | index >= logicalSize = NonLinear.pure ()
      | otherwise = do
          value <- UM.unsafeRead buffer index
          case move value of
            Ur !moved -> UM.unsafeWrite buffer index moved
          go (index + 1)

{- | Consume the initialized prefix, releasing each element exactly once.

The traversal only reads the buffer, but it runs under 'unsafePerformIO'.
That makes the binding a trusted boundary rather than an ordinary pure
function: were it inlined, GHC could duplicate the call across use sites, or
float it out of a scope, and each copy would consume the elements again. The
'NOINLINE' keeps exactly one occurrence, so the exactly-once discipline the
linear types promise is preserved in the generated code too.
-}
consumeInitialized ::
  (U.Unbox a, Consumable a) =>
  Int ->
  UM.IOVector a %1 ->
  ()
{-# NOINLINE consumeInitialized #-}
consumeInitialized =
  Unsafe.toLinear2 \logicalSize buffer ->
    let go !index
          | index >= logicalSize = NonLinear.pure ()
          | otherwise = do
              value <- UM.unsafeRead buffer index
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

-- | \(O(1)\). Return logical size and thread the borrow.
size ::
  (U.Unbox a) =>
  Borrow bk α (GrowableVector a) %1 ->
  (Ur Int, Borrow bk α (GrowableVector a))
{-# INLINE size #-}
size =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize _, duplicateRef) ->
        pop (aff duplicateRef) `lseq` (Ur logicalSize, vector)

-- | \(O(1)\). Return allocation capacity and thread the borrow.
capacity ::
  (U.Unbox a) =>
  Borrow bk α (GrowableVector a) %1 ->
  (Ur Int, Borrow bk α (GrowableVector a))
{-# INLINE capacity #-}
capacity =
  Unsafe.toLinear \vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq` (Ur (UM.length buffer), vector)

-- | Borrow an initialized element at an index.
get ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
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
  (U.Unbox a, α >= β) =>
  Int ->
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeGet #-}
unsafeGet =
  Unsafe.toLinear2 \index (UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          UnsafeAlias
            Control.<$> unsafeSystemIOToBO (UM.unsafeRead buffer index)

-- | Borrow the first initialized element.
head ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE head #-}
head = get 0

-- | Unchecked 'head'. The vector must be non-empty.
unsafeHead ::
  (U.Unbox a, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeHead #-}
unsafeHead = unsafeGet 0

-- | Borrow the last initialized element.
last ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE last #-}
last vector =
  case size vector of
    (Ur logicalSize, vector) ->
      if logicalSize <= 0
        then error "last: empty vector" vector
        else unsafeGet (logicalSize - 1) vector

-- | Unchecked 'last'. The vector must be non-empty.
unsafeLast ::
  (U.Unbox a, α >= β) =>
  Borrow bk α (GrowableVector a) %1 ->
  BO β (Borrow bk α a)
{-# INLINE unsafeLast #-}
unsafeLast vector =
  case size vector of
    (Ur logicalSize, vector) -> unsafeGet (logicalSize - 1) vector

-- | Copy an initialized element through a shared borrow.
copyAt ::
  (HasCallStack, U.Unbox a, Copyable a, α >= β) =>
  Int ->
  Share α (GrowableVector a) ->
  BO β (Ur a)
{-# INLINE copyAt #-}
copyAt index vector = Control.do
  Ur !value <- move Control.<$> get index vector
  Control.pure $! Ur $! copy value

-- | Copy an initialized element and retain the mutable borrow.
copyAtMut ::
  (HasCallStack, U.Unbox a, Copyable a, α >= β) =>
  Int ->
  Mut α (GrowableVector a) %1 ->
  BO β (Ur a, Mut α (GrowableVector a))
{-# INLINE copyAtMut #-}
copyAtMut =
  Unsafe.toLinear2 \index vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          if index < 0 || index >= logicalSize
            then
              error
                ( "copyAtMut: index "
                    <> show index
                    <> " out of bounds for length "
                    <> show logicalSize
                )
                vector
            else unsafeSystemIOToBO do
              !value <- UM.unsafeRead buffer index
              let !copied = copy (UnsafeAlias value)
              NonLinear.pure (Ur copied, vector)

-- | Replace an initialized element and return the displaced value.
set ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (a, Mut α (GrowableVector a))
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
            value
            vector
        else unsafeSet index value vector

-- | Unchecked 'set'. The index must satisfy @0 <= index < size@.
unsafeSet ::
  (U.Unbox a, α >= β) =>
  Int ->
  a %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (a, Mut α (GrowableVector a))
{-# INLINE unsafeSet #-}
unsafeSet =
  Unsafe.toLinear3 \index !value vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            !oldValue <- UM.unsafeExchange buffer index value
            NonLinear.pure (oldValue, vector)

-- | Linearly transform an initialized element and return an auxiliary result.
update ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
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
            action
            vector
        else unsafeUpdate index action vector

{- | Unchecked 'update'. The index must satisfy @0 <= index < size@.

The callback must return exactly one replacement before the growable borrow is
restored. No exceptional owner recovery is claimed.
-}
unsafeUpdate ::
  (U.Unbox a, α >= β) =>
  Int ->
  (a %1 -> BO β (result, a)) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (result, Mut α (GrowableVector a))
{-# INLINE unsafeUpdate #-}
unsafeUpdate index =
  Unsafe.toLinear2 \action vector@(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq` Control.do
          value <- unsafeSystemIOToBO (UM.unsafeRead buffer index)
          (!result, !updatedValue) <- action value
          () <-
            unsafeSystemIOToBO
              (Unsafe.toLinear3 UM.unsafeWrite buffer index updatedValue)
          Control.pure (result, vector)

-- | Linearly transform an initialized element.
modify ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Int ->
  (a %1 -> a) %1 ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE modify #-}
modify index function vector = Control.do
  ((), vector) <-
    update
      index
      (\value -> Control.pure ((), function value))
      vector
  Control.pure vector

-- | Swap two initialized elements.
swap ::
  (HasCallStack, U.Unbox a, α >= β) =>
  Mut α (GrowableVector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector a))
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
  (U.Unbox a, α >= β) =>
  Mut α (GrowableVector a) %1 ->
  Int ->
  Int ->
  BO β (Mut α (GrowableVector a))
{-# INLINE unsafeSwap #-}
unsafeSwap =
  Unsafe.toLinear3 \vector@(UnsafeAlias (GrowableVector ref)) first second ->
    case Ref.unsafeReadRef ref of
      (Header _ buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          unsafeSystemIOToBO do
            UM.unsafeSwap buffer first second
            NonLinear.pure vector

-- | Ensure at least the requested absolute capacity.
reserve ::
  (HasCallStack, U.Unbox a, α >= β) =>
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

-- | Ensure capacity for at least current size plus the requested amount.
reserveAdditional ::
  (HasCallStack, U.Unbox a, α >= β) =>
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

-- | Append one linearly supplied element.
push ::
  (HasCallStack, U.Unbox a, α >= β) =>
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
                !target = growthTarget (UM.length buffer) required
             in Control.do
                  grown <- growTo logicalSize target buffer
                  grown <- writeAt logicalSize value grown
                  Control.pure ((), Header required grown)
        )
        vector
    Control.pure vector

-- | Append copies of all elements of an immutable unboxed vector.
extend ::
  (HasCallStack, U.Unbox a, α >= β) =>
  U.Vector a ->
  Mut α (GrowableVector a) %1 ->
  BO β (Mut α (GrowableVector a))
{-# INLINE extend #-}
extend source vector = Control.do
  ((), vector) <-
    withHeader
      ( Unsafe.toLinear \(Header logicalSize buffer) ->
          let !sourceSize = U.length source
              !required = checkedAdd "extend" logicalSize sourceSize
              !target = growthTarget (UM.length buffer) required
           in Control.do
                grown <- growTo logicalSize target buffer
                grown <- copyImmutableInto source logicalSize grown
                Control.pure ((), Header required grown)
      )
      vector
  Control.pure vector

copyImmutable ::
  (U.Unbox a) =>
  U.Vector a ->
  Int ->
  UM.IOVector a ->
  NonLinear.IO ()
{-# INLINE copyImmutable #-}
copyImmutable source offset target =
  U.copy (UM.unsafeSlice offset (U.length source) target) source

copyImmutableInto ::
  (U.Unbox a) =>
  U.Vector a ->
  Int ->
  UM.IOVector a %1 ->
  BO β (UM.IOVector a)
{-# INLINE copyImmutableInto #-}
copyImmutableInto source offset =
  Unsafe.toLinear \target -> unsafeSystemIOToBO do
    copyImmutable source offset target
    NonLinear.pure target

writeAt ::
  (U.Unbox a) =>
  Int ->
  a %1 ->
  UM.IOVector a %1 ->
  BO β (UM.IOVector a)
{-# INLINE writeAt #-}
writeAt =
  Unsafe.toLinear3 \index value target -> unsafeSystemIOToBO do
    UM.unsafeWrite target index value
    NonLinear.pure target

growTo ::
  (U.Unbox a) =>
  Int ->
  Int ->
  UM.IOVector a %1 ->
  BO β (UM.IOVector a)
{-# INLINE growTo #-}
growTo =
  Unsafe.toLinear3 \logicalSize requested buffer ->
    let !oldCapacity = UM.length buffer
     in if requested <= oldCapacity
          then Control.pure buffer
          else unsafeSystemIOToBO do
            grown <- UM.unsafeNew requested
            UM.unsafeCopy
              (UM.unsafeTake logicalSize grown)
              (UM.unsafeTake logicalSize buffer)
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

{- | Project a growable borrow to its fixed initialized prefix.

The projection preserves borrow kind and lifetime and exposes no spare
capacity or growth operation.

Where a transaction branches, prefer projecting once at its entry --
@let %1 !content = 'getContents' borrow@ -- over projecting separately inside
each branch. Both are correct and consume the growable occurrence exactly
once; the entry form simply gives the optimizer one header read to place
rather than one per surviving branch.
-}
getContents ::
  (U.Unbox a) =>
  Borrow bk α (GrowableVector a) %1 ->
  Borrow bk α (Fixed.Vector a)
{-# INLINE getContents #-}
getContents =
  Unsafe.toLinear \(UnsafeAlias (GrowableVector ref)) ->
    case Ref.unsafeReadRef ref of
      (Header logicalSize buffer, duplicateRef) ->
        pop (aff duplicateRef) `lseq`
          UnsafeAlias
            (Fixed.Internal.unsafeFromMutableSlice 0 logicalSize buffer)

-- | Borrow the fixed initialized prefix in a rank-2 no-growth scope.
withContent ::
  (U.Unbox a) =>
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
  (U.Unbox a, Consumable result) =>
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
