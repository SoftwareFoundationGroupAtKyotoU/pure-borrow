{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Generic.Mutable.Linear.Borrow.UnrestrictedSpec (
  module Data.Vector.Generic.Mutable.Linear.Borrow.UnrestrictedSpec,
) where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Clone (Clone (clone))
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.TypingCases
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

freezeBoxed :: Vector.Vector V.Vector Int %1 -> [Int]
freezeBoxed vector =
  case Vector.toVector vector of
    Ur frozen -> V.toList frozen

freezeUnboxed :: Vector.Vector U.Vector Int %1 -> [Int]
freezeUnboxed vector =
  case Vector.toVector vector of
    Ur frozen -> U.toList frozen

freezePrimitive :: Vector.Vector P.Vector Int %1 -> [Int]
freezePrimitive vector =
  case Vector.toVector vector of
    Ur frozen -> P.toList frozen

test_construction :: TestTree
test_construction =
  testGroup
    "construction and backends"
    [ testCase "boxed round trip" do
        linearly
          ( \linear ->
              freezeBoxed (Vector.fromList @V.Vector [1, 2, 3] linear)
          )
          @?= [1, 2, 3]
    , testCase "unboxed round trip" do
        linearly
          ( \linear ->
              freezeUnboxed (Vector.fromList @U.Vector [1, 2, 3] linear)
          )
          @?= [1, 2, 3]
    , testCase "primitive backend remains extensible" do
        linearly
          ( \linear ->
              freezePrimitive (Vector.fromList @P.Vector [1, 2, 3] linear)
          )
          @?= [1, 2, 3]
    , testCase "constant and empty" do
        linearly
          ( \linear ->
              case dup linear of
                (emptyLinear, constantLinear) ->
                  ( freezeBoxed (Vector.empty @V.Vector @Int emptyLinear)
                  , freezeUnboxed
                      (Vector.constant @U.Vector 3 (7 :: Int) constantLinear)
                  )
          )
          @?= ([], [7, 7, 7])
    , testCase "fromVector copies its immutable source" do
        let source = V.fromList [1, 2, 3]
            result =
              linearly \linear -> DataFlow.do
                (ownerLinear, runLinear) <- dup linear
                runBO runLinear Control.do
                  (vector, lend) <-
                    borrowM (Vector.fromVector source ownerLinear)
                  (Ur _, vector) <- Vector.set 0 99 vector
                  let !() = consume vector
                  pureAfter (freezeBoxed (reclaim lend))
        V.toList source @?= [1, 2, 3]
        result @?= [99, 2, 3]
    ]

boxedOperations :: ((Int, Int, Int), [Int])
boxedOperations =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2, 3] ownerLinear)
      (Ur first, vector) <- Vector.get 0 vector
      (Ur old, vector) <- Vector.set 1 20 vector
      (Ur auxiliary, vector) <-
        Vector.update
          2
          (\value -> Control.pure (Ur (value * 2), Ur (value + 1)))
          vector
      vector <- Vector.modify 0 (\value -> value NonLinear.+ 10) vector
      vector <- Vector.write 1 21 vector
      vector <- Vector.swap vector 0 2
      let !() = consume vector
      pureAfter ((first, old, auxiliary), freezeBoxed (reclaim lend))

sharedReads :: (Int, Int)
sharedReads =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [11, 22] ownerLinear)
      ((first, second), vector) <-
        sharing vector \shared -> Control.do
          Ur first <- Vector.copyAt 0 shared
          Ur second <- Vector.copyAt 1 shared
          Control.pure (first, second)
      let !() = consume vector
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur _ -> (first, second)
        )

ordinaryElementMultiplicity :: (Int, [Int])
ordinaryElementMultiplicity =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2] ownerLinear)
      let replacement = 7
      (Ur displaced, vector) <- Vector.set 0 replacement vector
      vector <- Vector.write 1 replacement vector
      let !() = consume vector
      pureAfter
        ( displaced + displaced
        , freezeBoxed (reclaim lend)
        )

test_operations :: TestTree
test_operations =
  testGroup
    "unrestricted element operations"
    [ testCase "get, set, update, modify, and swap" do
        boxedOperations @?= ((1, 2, 6), [4, 21, 11])
    , testCase "a shared borrow may be read repeatedly" do
        sharedReads @?= (11, 22)
    , testCase "inputs, callbacks, and displaced values are unrestricted" do
        ordinaryElementMultiplicity @?= (2, [7, 7])
    ]

splitBoxed :: [Int]
splitBoxed =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2, 3, 4] ownerLinear)
      let !(left, right) = Vector.splitAt 2 vector
      (left, right) <-
        parBO
          (Vector.modify 0 (\value -> value NonLinear.+ 10) left)
          (Vector.modify 1 (\value -> value NonLinear.+ 20) right)
      let !() = consume (left, right)
      pureAfter (freezeBoxed (reclaim lend))

splitUnboxed :: [Int]
splitUnboxed =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @U.Vector [1, 2, 3, 4] ownerLinear)
      let !(left, right) = Vector.splitAt 2 vector
      (left, right) <-
        parBO
          (Vector.modify 1 (\value -> value NonLinear.+ 10) left)
          (Vector.modify 0 (\value -> value NonLinear.+ 20) right)
      let !() = consume (left, right)
      pureAfter (freezeUnboxed (reclaim lend))

nestedBoundarySplits :: [Int]
nestedBoundarySplits =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2, 3, 4] ownerLinear)
      let !(emptyLeft, rest) = Vector.splitAt 0 vector
          !(prefix, emptyRight) = Vector.splitAt 4 rest
          !(firstTwo, latterTwo) = Vector.splitAt 2 prefix
          !(third, fourth) = Vector.splitAt 1 latterTwo
      firstTwo <-
        Vector.modify 0 (\value -> value NonLinear.+ 10) firstTwo
      third <-
        Vector.modify 0 (\value -> value NonLinear.+ 20) third
      fourth <-
        Vector.modify 0 (\value -> value NonLinear.+ 30) fourth
      let !() = consume (emptyLeft, emptyRight, firstTwo, third, fourth)
      pureAfter (freezeBoxed (reclaim lend))

sharedSplitReads :: (Int, Int)
sharedSplitReads =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @U.Vector [10, 20, 30, 40] ownerLinear)
      ((Ur leftValue, Ur rightValue), vector) <-
        sharing vector \shared -> Control.do
          let !(left, right) = Vector.splitAt 2 shared
          parBO
            (Vector.copyAt 1 left)
            (Vector.copyAt 0 right)
      let !() = consume vector
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur _ -> (leftValue, rightValue)
        )

test_split :: TestTree
test_split =
  testGroup
    "split ownership"
    [ testCase "boxed disjoint slices mutate in parallel" do
        splitBoxed @?= [11, 2, 3, 24]
    , testCase "unboxed disjoint slices mutate in parallel" do
        splitUnboxed @?= [1, 12, 23, 4]
    , testCase "boundary and nested splits preserve disjoint ownership" do
        nestedBoundarySplits @?= [11, 2, 23, 34]
    , testCase "shared split ranges can be read in parallel" do
        sharedSplitReads @?= (20, 30)
    ]

data Tracked = Tracked
  { capabilityCalls :: !(IORef Int)
  , trackedValue :: !Int
  }

recordCapability :: Tracked -> Tracked
recordCapability tracked =
  case unsafePerformIO
    (modifyIORef' (capabilityCalls tracked) NonLinear.succ) of
    () -> tracked

instance Consumable Tracked where
  consume =
    Unsafe.toLinear \tracked ->
      recordCapability tracked `NonLinear.seq` ()

instance Dupable Tracked where
  dup2 =
    Unsafe.toLinear \tracked ->
      (recordCapability tracked, recordCapability tracked)

instance Movable Tracked where
  move = Unsafe.toLinear \tracked -> Ur (recordCapability tracked)

instance Copyable Tracked where
  copy =
    Unsafe.toLinear \(UnsafeAlias tracked) ->
      recordCapability tracked

capabilityFreeLifecycle :: IORef Int -> ([Int], [Int], [Int])
capabilityFreeLifecycle calls =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Vector.fromVector
              (V.fromList [Tracked calls 1, Tracked calls 2])
              ownerLinear
          )
      (cloned, vector) <- sharing vector (\shared -> clone shared)
      (Ur snapshot, vector) <- Vector.copyToVector vector
      (Ur first, vector) <- Vector.copyAtMut 0 vector
      (Ur old, vector) <- Vector.set 1 (Tracked calls 3) vector
      vector <- Vector.write 0 (Tracked calls 4) vector
      let
        !() = consume vector
        clonedValues =
          case Vector.toVector cloned of
            Ur frozen -> NonLinear.map trackedValue (V.toList frozen)
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur frozen ->
              ( NonLinear.map trackedValue (V.toList snapshot)
              , clonedValues
              , [trackedValue first, trackedValue old]
                  <> NonLinear.map trackedValue (V.toList frozen)
              )
        )

test_capabilities :: TestTree
test_capabilities =
  testGroup
    "capability-free elements"
    [ testCase "all fixed operations avoid element capability callbacks" do
        calls <- newIORef 0
        capabilityFreeLifecycle calls
          @?= ([1, 2], [1, 2], [1, 2, 4, 3])
        count <- readIORef calls
        count @?= 0
    , testCase "a clone has independent mutable backing" do
        cloneIndependence @?= ([1, 2], [11, 2])
    ]

cloneIndependence :: ([Int], [Int])
cloneIndependence =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2] ownerLinear)
      (cloned, vector) <- sharing vector (\shared -> clone shared)
      vector <-
        Vector.modify 0 (\value -> value NonLinear.+ 10) vector
      let
        !() = consume vector
        clonedValues = freezeBoxed cloned
      pureAfter
        ( clonedValues
        , freezeBoxed (reclaim lend)
        )

referenceAliasValue :: IORef Int -> Int
referenceAliasValue reference =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromVector (V.singleton reference) ownerLinear)
      (Ur observed, vector) <- Vector.get 0 vector
      let !() = consume vector
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur frozen ->
              unsafePerformIO do
                modifyIORef'
                  observed
                  (\value -> value NonLinear.+ 41)
                readIORef (V.head frozen)
        )

test_aliasing :: TestTree
test_aliasing =
  testCase "boxed entries deliberately remain GC-owned aliases" do
    reference <- newIORef 1
    referenceAliasValue reference @?= 42
    originalValue <- readIORef reference
    originalValue @?= 42

getOutOfBounds :: Int -> Int
getOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2, 3] ownerLinear)
      (Ur value, vector) <- Vector.get index vector
      let !() = consume vector
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur frozen -> value + V.length frozen
        )

writeOutOfBounds :: Int -> Int
writeOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1, 2, 3] ownerLinear)
      vector <- Vector.write index 4 vector
      let !() = consume vector
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur frozen -> V.sum frozen
        )

assertErrorPrefix :: NonLinear.String -> a -> Assertion
assertErrorPrefix expectedPrefix value = do
  result <- Exception.try @Exception.ErrorCall (Exception.evaluate value)
  case result of
    Left exception ->
      assertBool
        ("unexpected error: " <> Exception.displayException exception)
        (expectedPrefix `List.isPrefixOf` Exception.displayException exception)
    Right _ ->
      assertFailure ("expected error beginning with " <> expectedPrefix)

test_bounds :: TestTree
test_bounds =
  testGroup
    "bounds"
    [ testCase "negative index" do
        assertErrorPrefix "get: index -1 out of bounds" (getOutOfBounds (-1))
    , testCase "upper bound" do
        assertErrorPrefix "get: index 3 out of bounds" (getOutOfBounds 3)
    , testCase "write checks its lower bound" do
        assertErrorPrefix "write: index -1 out of bounds" (writeOutOfBounds (-1))
    , testCase "write checks its upper bound" do
        assertErrorPrefix "write: index 3 out of bounds" (writeOutOfBounds 3)
    ]

assertDeferredTypeError :: NonLinear.String -> a -> Assertion
assertDeferredTypeError expectedFragment value = do
  result <- Exception.try @Exception.SomeException (Exception.evaluate value)
  case result of
    Left exception ->
      assertBool
        ("unexpected deferred error: " <> Exception.displayException exception)
        (expectedFragment `List.isInfixOf` Exception.displayException exception)
    Right _ ->
      assertFailure
        ("expected deferred type error containing " <> expectedFragment)

test_typing :: TestTree
test_typing =
  testGroup
    "typing boundaries"
    [ testCase "backend role is nominal" do
        assertDeferredTypeError "Couldn't match type" badBackendCoercion
    , testCase "element role is nominal" do
        assertDeferredTypeError "Couldn't match type" badElementCoercionCase
    , testCase "ownership families cannot be coerced" do
        assertDeferredTypeError "representation" badOwnershipCoercionCase
    , testCase "get cannot manufacture an element borrow" do
        assertDeferredTypeError "Couldn't match" badElementBorrowCase
    , testCase "the mutable owner cannot be copied" do
        assertDeferredTypeError "cannot be copied!" badDuplicate
    , testCase "shared borrows cannot mutate" do
        assertDeferredTypeError "Couldn't match" badMutateSharedCase
    ]
