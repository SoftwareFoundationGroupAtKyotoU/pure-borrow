{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Unboxed.Mutable.Linear.BorrowSpec (
  module Data.Vector.Unboxed.Mutable.Linear.BorrowSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy), copyMut)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Vector
import Data.Vector.Unboxed.Mutable.Linear.TypingCases
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import PureBorrow.Internal.Bench.Unboxed qualified as UnboxedBench
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data Tracked = Tracked !(IORef Int) !Int

instance Consumable (U.DoNotUnboxLazy Tracked) where
  consume =
    Unsafe.toLinear \(U.DoNotUnboxLazy (Tracked counter _)) ->
      unsafePerformIO (modifyIORef' counter NonLinear.succ)

instance Consumable (U.DoNotUnboxStrict Tracked) where
  consume =
    Unsafe.toLinear \(U.DoNotUnboxStrict (Tracked counter _)) ->
      unsafePerformIO (modifyIORef' counter NonLinear.succ)

data MoveTracked = MoveTracked !(IORef Int) !Int !Bool

type UnboxedMoveTracked = U.DoNotUnboxLazy MoveTracked

instance Consumable UnboxedMoveTracked where
  consume = Unsafe.toLinear \_ -> ()

instance Dupable UnboxedMoveTracked where
  dup2 = Unsafe.toLinear \value -> (value, value)

instance Movable UnboxedMoveTracked where
  move =
    Unsafe.toLinear
      \(U.DoNotUnboxLazy (MoveTracked moves value _)) ->
        case unsafePerformIO (modifyIORef' moves NonLinear.succ) of
          () -> Ur (U.DoNotUnboxLazy (MoveTracked moves value True))

materializeMoveTracked :: IORef Int -> [(Int, Bool)]
materializeMoveTracked moves =
  linearly \linear ->
    case Vector.toVector
      ( Vector.fromVector
          ( U.fromList
              [ U.DoNotUnboxLazy (MoveTracked moves 10 False)
              , U.DoNotUnboxLazy (MoveTracked moves 20 False)
              ]
          )
          linear
      ) of
      Ur vector ->
        NonLinear.map
          ( \(U.DoNotUnboxLazy (MoveTracked _ value wasMoved)) ->
              (value, wasMoved)
          )
          (U.toList vector)

discardMaterializedMoveTracked :: IORef Int -> ()
discardMaterializedMoveTracked moves =
  linearly \linear ->
    case Vector.toVector
      ( Vector.fromVector
          ( U.fromList
              [ U.DoNotUnboxLazy (MoveTracked moves 10 False)
              , U.DoNotUnboxLazy (MoveTracked moves 20 False)
              ]
          )
          linear
      ) of
      Ur _ -> ()

data CopyTracked = CopyTracked !(IORef Int) !(IORef Int) !Int

type UnboxedCopyTracked = U.DoNotUnboxLazy CopyTracked

instance Copyable UnboxedCopyTracked where
  copy =
    Unsafe.toLinear
      \(UnsafeAlias value@(U.DoNotUnboxLazy (CopyTracked copies retired _))) ->
        case unsafePerformIO do
          retirementCount <- readIORef retired
          if retirementCount == 0
            then modifyIORef' copies NonLinear.succ
            else NonLinear.error "copy invoked after source retirement" of
          () -> value

instance Consumable UnboxedCopyTracked where
  consume =
    Unsafe.toLinear
      \(U.DoNotUnboxLazy (CopyTracked _ retired _)) ->
        unsafePerformIO (modifyIORef' retired NonLinear.succ)

freezeList :: Vector.Vector Int %1 -> [Int]
freezeList array =
  case Vector.toList array of
    Ur values -> values

freezeLength :: Vector.Vector Int %1 -> Int
freezeLength vector =
  case Vector.toVector vector of
    Ur frozen -> U.length frozen

roundTrip :: [Int]
roundTrip =
  linearly \linear ->
    freezeList (Vector.fromList [1, 2, 3, 4] linear)

test_construction :: TestTree
test_construction =
  testGroup
    "construction"
    [ testCase "empty has no elements" do
        linearly (\linear -> freezeList (Vector.empty linear)) @?= []
    , testCase "constant initializes every element" do
        linearly (\linear -> freezeList (Vector.constant 3 (7 :: Int) linear))
          @?= [7, 7, 7]
    , testCase "fromList moves every element exactly once" do
        roundTrip @?= [1, 2, 3, 4]
    , testCase "fromVector copies an immutable vector" do
        linearly
          (\linear -> freezeList (Vector.fromVector (U.fromList [4, 5, 6]) linear))
          @?= [4, 5, 6]
    , testCase "ordinary sources need no Copyable instance" do
        counter <- newIORef 0
        _ <-
          Exception.evaluate $
            linearly \linear ->
              dup linear & \(constantLinear, vectorLinear) ->
                consume
                  ( Vector.constant
                      2
                      (U.DoNotUnboxLazy (Tracked counter 1))
                      constantLinear
                  )
                  `lseq` consume
                    ( Vector.fromVector
                        (U.singleton (U.DoNotUnboxLazy (Tracked counter 2)))
                        vectorLinear
                    )
        consumed <- readIORef counter
        consumed @?= 3
    , testCase "materialization invokes move for every owned element" do
        moves <- newIORef 0
        materializeMoveTracked moves @?= [(10, True), (20, True)]
        moveCount <- readIORef moves
        moveCount @?= 2
    , testCase "discarding materialization still invokes every move" do
        moves <- newIORef 0
        _ <- Exception.evaluate (discardMaterializedMoveTracked moves)
        moveCount <- readIORef moves
        moveCount @?= 2
    , testCase "unsafeFromVector takes ownership of the source" do
        linearly
          ( \linear ->
              freezeList
                (Vector.unsafeFromVector (U.fromList [8, 9 :: Int]) linear)
          )
          @?= [8, 9]
    , testCase "unsafeFromMutable takes ownership of the complete slice" do
        linearly
          ( \linear ->
              freezeList
                ( Vector.unsafeFromMutable
                    (unsafePerformIO (UM.replicate 2 (11 :: Int)))
                    linear
                )
          )
          @?= [11, 11]
    ]

mirroredSurface ::
  ( ((Int, Int, Int, Int), (Int, Int, Int))
  , [Int]
  )
mirroredSurface =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [1, 2, 3] ownerLinear)
      (Ur logicalSize, array) <- Control.pure (Vector.size array)
      (Ur middle, array) <-
        reborrowing array \short -> Control.do
          element <- Vector.get 1 short
          Control.pure (copyMut element)
      (Ur first, array) <-
        reborrowing array \short -> Control.do
          element <- Vector.head short
          Control.pure (copyMut element)
      (Ur final, array) <-
        reborrowing array \short -> Control.do
          element <- Vector.last short
          Control.pure (copyMut element)
      (Ur copied, array) <- Vector.copyAtMut 1 array
      (old, array) <- Vector.set 1 20 array
      (auxiliary, array) <-
        Vector.update
          1
          ( \value ->
              case dup value of
                (auxiliary, replacement) ->
                  Control.pure (auxiliary, replacement + 1)
          )
          array
      array <- Vector.modify 0 (+ 10) array
      array <- Vector.swap array 0 2
      let !() = consume array
      pureAfter
        (
          ( (logicalSize, middle, first, final)
          , (copied, old, auxiliary)
          )
        , freezeList (reclaim lend)
        )

test_mirroredSurface :: TestTree
test_mirroredSurface =
  testCase "supports borrowed and copied reads, replacement, update, modify, and swap" do
    mirroredSurface @?= (((3, 2, 1, 3), (2, 2, 20)), [3, 21, 11])

retireCopiedResult ::
  (Ur UnboxedCopyTracked, Mut α (Vector.Vector UnboxedCopyTracked)) %1 ->
  Vector.Vector UnboxedCopyTracked %1 ->
  Int
retireCopiedResult =
  Unsafe.toLinear2 \(copiedResult, borrowed) owner ->
    consume borrowed `lseq`
      consume owner `lseq`
        case copiedResult of
          Ur (U.DoNotUnboxLazy (CopyTracked _ _ value)) -> value

copyAtMutAfterRetirement :: IORef Int -> IORef Int -> Int
copyAtMutAfterRetirement copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Vector.fromList
              [U.DoNotUnboxLazy (CopyTracked copies retired 10)]
              ownerLinear
          )
      copiedResult <- Vector.copyAtMut 0 vector
      pureAfter (retireCopiedResult copiedResult (reclaim lend))

test_copyAtMutStrictness :: TestTree
test_copyAtMutStrictness =
  testCase "copyAtMut completes copying before mutable recovery" do
    copies <- newIORef 0
    retired <- newIORef 0
    copyAtMutAfterRetirement copies retired @?= 10
    copyCount <- readIORef copies
    copyCount @?= 1
    retirementCount <- readIORef retired
    retirementCount @?= 1

sharedReads :: ((Int, Int), [Int])
sharedReads =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [5, 6, 7] ownerLinear)
      share array & \(Ur shared) -> Control.do
        Ur first <- Vector.copyAt 0 shared
        Ur second <- Vector.copyAt 1 shared
        pureAfter ((first, second), freezeList (reclaim lend))

test_sharedReads :: TestTree
test_sharedReads =
  testCase "copies repeatedly through a shared borrow" do
    sharedReads @?= ((5, 6), [5, 6, 7])

snapshotThenMutate :: ([Int], [Int])
snapshotThenMutate =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [1, 2, 3] ownerLinear)
      (Ur snapshot, array) <- Vector.copyToVector array
      array <- Vector.modify 0 (+ 100) array
      let !() = consume array
      pureAfter (U.toList snapshot, freezeList (reclaim lend))

trackedSnapshot :: IORef Int -> IORef Int -> [Int]
trackedSnapshot copies retired =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <-
        borrowM
          ( Vector.fromList
              [ U.DoNotUnboxLazy (CopyTracked copies retired 10)
              , U.DoNotUnboxLazy (CopyTracked copies retired 20)
              ]
              ownerLinear
          )
      (Ur snapshot, array) <- Vector.copyToVector array
      let !() = consume array
      pureAfter
        ( consume (reclaim lend) `lseq`
            NonLinear.map
              (\(U.DoNotUnboxLazy (CopyTracked _ _ value)) -> value)
              (U.toList snapshot)
        )

test_copyToVector :: TestTree
test_copyToVector =
  testGroup
    "copyToVector"
    [ testCase "copies a stable snapshot while leaving a mutable owner live" do
        snapshotThenMutate @?= ([1, 2, 3], [101, 2, 3])
    , testCase "accepts a shared borrow" do
        sharedSnapshot @?= ([1, 2, 3], [1, 2, 3])
    , testCase "invokes copy for every element while retaining the owner" do
        copies <- newIORef 0
        retired <- newIORef 0
        trackedSnapshot copies retired @?= [10, 20]
        copyCount <- readIORef copies
        copyCount @?= 2
        retirementCount <- readIORef retired
        retirementCount @?= 2
    ]

sharedSnapshot :: ([Int], [Int])
sharedSnapshot =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [1, 2, 3] ownerLinear)
      share array & \(Ur shared) -> Control.do
        (Ur snapshot, shared) <- Vector.copyToVector shared
        let !() = consume shared
        pureAfter (U.toList snapshot, freezeList (reclaim lend))

parallelSplit :: Int -> [Int]
parallelSplit splitIndex =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [1, 2, 3, 4] ownerLinear)
      let !(left, right) = Vector.splitAt splitIndex array
      (Ur leftSize, left) <- Control.pure (Vector.size left)
      (Ur rightSize, right) <- Control.pure (Vector.size right)
      consume
        Control.<$> parBO
          ( if leftSize > 0
              then Vector.modify 0 (+ 10) left
              else Control.pure left
          )
          ( if rightSize > 0
              then Vector.modify 0 (+ 20) right
              else Control.pure right
          )
      pureAfter (freezeList (reclaim lend))

splitSizes :: Int -> (Int, Int)
splitSizes splitIndex =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [1, 2, 3, 4] ownerLinear)
      case Vector.splitAt splitIndex array of
        (left0, right0) ->
          case Vector.size left0 of
            (Ur leftSize, left) ->
              case Vector.size right0 of
                (Ur rightSize, right) -> DataFlow.do
                  consume left
                  consume right
                  pureAfter
                    ( leftSize
                    , rightSize + freezeLength (reclaim lend) - 4
                    )

test_splitAt :: TestTree
test_splitAt =
  testGroup
    "splitAt"
    [ testCase "separates disjoint ranges for parallel mutation" do
        parallelSplit 2 @?= [11, 2, 23, 4]
    , testCase "accepts the lower boundary" do
        splitSizes 0 @?= (0, 4)
    , testCase "accepts the upper boundary" do
        splitSizes 4 @?= (4, 0)
    , testCase "clamps a negative index" do
        splitSizes (-3) @?= (0, 4)
    , testCase "clamps an oversized index" do
        splitSizes 10 @?= (4, 0)
    ]

trackedLifecycle :: IORef Int -> Int
trackedLifecycle counter =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <-
        borrowM
          ( Vector.fromList
              [ U.DoNotUnboxLazy (Tracked counter 10)
              , U.DoNotUnboxLazy (Tracked counter 20)
              ]
              ownerLinear
          )
      (displaced, array) <-
        Vector.set 0 (U.DoNotUnboxLazy (Tracked counter 30)) array
      let !() = consume displaced
      (oldLabel, array) <-
        Vector.update
          1
          ( \(U.DoNotUnboxLazy (Tracked elementCounter label)) ->
              case dup label of
                (oldLabel, updatedLabel) ->
                  Control.pure
                    ( oldLabel
                    , U.DoNotUnboxLazy
                        (Tracked elementCounter (updatedLabel + 1))
                    )
          )
          array
      let !() = consume array
      pureAfter (consume (reclaim lend) `lseq` oldLabel)

test_nonCopyableElements :: TestTree
test_nonCopyableElements =
  testGroup
    "non-Copyable elements"
    [ testCase "get preserves a nested Ref identity" do
        borrowedRefAlias @?= 42
    , testCase "moves and retires lazy boxed-backed elements exactly once" do
        counter <- newIORef 0
        oldLabel <- Exception.evaluate (trackedLifecycle counter)
        oldLabel @?= 20
        retired <- readIORef counter
        retired @?= 3
    , testCase "moves and retires strict boxed-backed elements exactly once" do
        counter <- newIORef 0
        result <- Exception.evaluate (strictTrackedLifecycle counter)
        result @?= ()
        retired <- readIORef counter
        retired @?= 2
    ]

borrowedRefAlias :: Int
borrowedRefAlias =
  linearly \linear -> DataFlow.do
    (refLinear, remainingLinear) <- dup linear
    (ownerLinear, runLinear) <- dup remainingLinear
    runBO runLinear Control.do
      (array, lend) <-
        borrowM
          ( Vector.fromList
              [U.DoNotUnboxLazy (LinearElement (Ref.new 1 refLinear))]
              ownerLinear
          )
      ((), array) <-
        reborrowing array \short -> Control.do
          element <- nonCopyableGet short
          modifyBorrowedRef element
      (observed, array) <-
        reborrowing array \short -> Control.do
          element <- nonCopyableGet short
          copyBorrowedRef element
      let !() = consume array
      pureAfter (consume (reclaim lend) `lseq` observed)

asBorrowedRef ::
  Mut α BoxedLinearElement %1 ->
  Mut α (Ref.Ref Int)
asBorrowedRef = upcast

modifyBorrowedRef ::
  Mut α BoxedLinearElement %1 ->
  BO α ()
modifyBorrowedRef element = Control.do
  ref <- RefBorrow.modify (+ 41) (asBorrowedRef element)
  Control.pure (consume ref)

copyBorrowedRef ::
  Mut α BoxedLinearElement %1 ->
  BO α Int
copyBorrowedRef = RefBorrow.copyRef . asBorrowedRef

strictTrackedLifecycle :: IORef Int -> ()
strictTrackedLifecycle counter =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <-
        borrowM
          ( Vector.fromList
              [U.DoNotUnboxStrict (Tracked counter 10)]
              ownerLinear
          )
      (displaced, array) <-
        Vector.set 0 (U.DoNotUnboxStrict (Tracked counter 20)) array
      let
        !() = consume displaced
        !() = consume array
      pureAfter (consume (reclaim lend))

assertErrorPrefix :: NonLinear.String -> a -> Assertion
assertErrorPrefix expectedPrefix value = do
  result <- Exception.try @Exception.ErrorCall $ Exception.evaluate value
  case result of
    Left exception ->
      assertBool
        ("unexpected error: " <> Exception.displayException exception)
        (expectedPrefix `List.isPrefixOf` Exception.displayException exception)
    Right _ -> assertFailure ("expected error beginning with " <> expectedPrefix)

getOutOfBounds :: Int -> Int
getOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [10, 20, 30] ownerLinear)
      (Ur value, array) <-
        reborrowing array \short -> Control.do
          element <- Vector.get index short
          Control.pure (copyMut element)
      let !() = consume array
      pureAfter (value + freezeLength (reclaim lend))

copyAtMutOutOfBounds :: Int -> Int
copyAtMutOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [10, 20, 30] ownerLinear)
      (Ur value, array) <- Vector.copyAtMut index array
      let !() = consume array
      pureAfter (value + freezeLength (reclaim lend))

setOutOfBounds :: Int -> Int
setOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [10, 20, 30] ownerLinear)
      (old, array) <- Vector.set index 0 array
      let !() = consume array
      pureAfter (old + freezeLength (reclaim lend))

updateOutOfBounds :: Int -> Int
updateOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [10, 20, 30] ownerLinear)
      (old, array) <-
        Vector.update
          index
          ( \value ->
              case dup value of
                (old, replacement) -> Control.pure (old, replacement)
          )
          array
      let !() = consume array
      pureAfter (old + freezeLength (reclaim lend))

swapOutOfBounds :: Int -> Int
swapOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.fromList [10, 20, 30] ownerLinear)
      array <- Vector.swap array 0 index
      let !() = consume array
      pureAfter (freezeLength (reclaim lend))

test_bounds :: TestTree
test_bounds =
  testGroup
    "bounds"
    [ testCase "get rejects a negative index" do
        assertErrorPrefix
          "get: index -1 out of bounds for length 3"
          (getOutOfBounds (-1))
    , testCase "get rejects the upper bound" do
        assertErrorPrefix
          "get: index 3 out of bounds for length 3"
          (getOutOfBounds 3)
    , testCase "copyAtMut rejects a negative index" do
        assertErrorPrefix
          "get: index -1 out of bounds for length 3"
          (copyAtMutOutOfBounds (-1))
    , testCase "copyAtMut rejects the upper bound" do
        assertErrorPrefix
          "get: index 3 out of bounds for length 3"
          (copyAtMutOutOfBounds 3)
    , testCase "set rejects a negative index" do
        assertErrorPrefix
          "set: index -1 out of bounds for length 3"
          (setOutOfBounds (-1))
    , testCase "set rejects the upper bound" do
        assertErrorPrefix
          "set: index 3 out of bounds for length 3"
          (setOutOfBounds 3)
    , testCase "update rejects a negative index" do
        assertErrorPrefix
          "update: index -1 out of bounds for length 3"
          (updateOutOfBounds (-1))
    , testCase "update rejects the upper bound" do
        assertErrorPrefix
          "update: index 3 out of bounds for length 3"
          (updateOutOfBounds 3)
    , testCase "swap rejects a negative index" do
        assertErrorPrefix
          "swap: indices (0,-1) out of bounds for length 3"
          (swapOutOfBounds (-1))
    , testCase "swap rejects the upper bound" do
        assertErrorPrefix
          "swap: indices (0,3) out of bounds for length 3"
          (swapOutOfBounds 3)
    , testCase "head rejects an empty vector" do
        assertErrorPrefix
          "get: index 0 out of bounds for length 0"
          (getOutOfBoundsOnEmpty Vector.head)
    , testCase "last rejects an empty vector" do
        assertErrorPrefix
          "last: empty vector"
          (getOutOfBoundsOnEmpty Vector.last)
    ]

getOutOfBoundsOnEmpty ::
  (forall α. Mut α (Vector.Vector Int) %1 -> BO α (Mut α Int)) ->
  Int
getOutOfBoundsOnEmpty operation =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (array, lend) <- borrowM (Vector.empty ownerLinear)
      (Ur value, array) <-
        reborrowing array \short -> Control.do
          element <- operation short
          Control.pure (copyMut element)
      let !() = consume array
      pureAfter (value + freezeLength (reclaim lend))

test_typingBoundaries :: TestTree
test_typingBoundaries =
  testGroup
    "typing boundaries"
    [ expectDeferredTypeError
        "unboxed Vector element role is nominal"
        "Couldn't match type"
        badElementCoercion
    , expectDeferredTypeError
        "unboxed Vector cannot be coerced to a boxed Vector"
        "Couldn't match representation of type"
        badUnboxedToBoxed
    , expectDeferredTypeError
        "a boxed Vector cannot be coerced to an unboxed Vector"
        "Couldn't match representation of type"
        badBoxedToUnboxed
    , expectDeferredTypeError
        "an unboxed Vector borrow cannot swap lifetime indices"
        "Couldn't match type"
        badLifetimeSwap
    , expectDeferredTypeError
        "unboxed Vector has no generic split"
        "DistributesAlias Unboxed.Vector"
        badSplit
    , expectDeferredTypeError
        "unboxed Vector cannot be copied"
        "cannot be copied!"
        badDuplicate
    , expectDeferredTypeError
        "Movable alone does not permit copyAt"
        "Copyable (U.DoNotUnboxLazy MovableOnly)"
        badNonCopyableCopyAtCase
    , expectDeferredTypeError
        "Movable alone does not permit copyAtMut"
        "Copyable (U.DoNotUnboxLazy MovableOnly)"
        badNonCopyableCopyAtMutCase
    ]
  where
    expectDeferredTypeError description expectedFragment value =
      testCase description do
        result <- Exception.try @Exception.SomeException (Exception.evaluate value)
        case result of
          Left exception ->
            assertBool
              ("unexpected deferred type error: " <> Exception.displayException exception)
              (expectedFragment `List.isInfixOf` Exception.displayException exception)
          Right _ ->
            assertFailure
              ("expected deferred type error containing " <> expectedFragment)

test_benchmarkRoots :: TestTree
test_benchmarkRoots =
  testGroup
    "benchmark roots"
    [ testGroup
        ("length " <> show length_)
        [ testCase "fixed kernel roots agree" do
            let input =
                  U.generate length_ (\index -> index `NonLinear.rem` 17)
            UnboxedBench.pureBorrowFixedUnboxedKernel input
              @?= UnboxedBench.directFixedUnboxedKernel input
        , testCase "fixed public-materialization roots agree" do
            let input =
                  U.generate length_ (\index -> index `NonLinear.rem` 17)
            UnboxedBench.pureBorrowFixedUnboxedMaterialization input
              @?= UnboxedBench.directFixedUnboxedMaterialization input
        ]
    | length_ <- [0, 1, 257, 1024 * 1024]
    ]
