{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.UnrestrictedSpec (
  module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.UnrestrictedSpec,
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
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.TypingCases
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Fixed
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

freezeBoxed :: Growable.GrowableVector V.Vector Int %1 -> [Int]
freezeBoxed vector =
  case Growable.toVector vector of
    Ur frozen -> V.toList frozen

freezeUnboxed :: Growable.GrowableVector U.Vector Int %1 -> [Int]
freezeUnboxed vector =
  case Growable.toVector vector of
    Ur frozen -> U.toList frozen

freezePrimitive :: Growable.GrowableVector P.Vector Int %1 -> [Int]
freezePrimitive vector =
  case Growable.toVector vector of
    Ur frozen -> P.toList frozen

boxedGrowth :: ((Int, Int), [Int])
boxedGrowth =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.empty @V.Vector @Int ownerLinear)
      vector <- Growable.push 1 vector
      vector <- Growable.extend (V.fromList [2, 3]) vector
      vector <- Growable.reserve 10 vector
      (Ur logicalSize, vector) <- Control.pure (Growable.size vector)
      (Ur allocated, vector) <- Control.pure (Growable.capacity vector)
      let !() = consume vector
      pureAfter
        ( (logicalSize, allocated)
        , freezeBoxed (reclaim lend)
        )

unboxedGrowth :: [Int]
unboxedGrowth =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @U.Vector [1, 2] ownerLinear)
      vector <- Growable.push 3 vector
      vector <- Growable.extend (U.fromList [4, 5]) vector
      let !() = consume vector
      pureAfter (freezeUnboxed (reclaim lend))

primitiveGrowth :: [Int]
primitiveGrowth =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.withCapacity @P.Vector @Int 1 ownerLinear)
      vector <- Growable.push 7 vector
      vector <- Growable.push 8 vector
      vector <- Growable.push 9 vector
      let !() = consume vector
      pureAfter (freezePrimitive (reclaim lend))

fromVectorCopy :: ([Int], [Int])
fromVectorCopy =
  let source = V.fromList [1, 2, 3]
      result =
        linearly \linear -> DataFlow.do
          (ownerLinear, runLinear) <- dup linear
          runBO runLinear Control.do
            (vector, lend) <-
              borrowM (Growable.fromVector source ownerLinear)
            vector <- Growable.write 0 9 vector
            let !() = consume vector
            pureAfter (freezeBoxed (reclaim lend))
   in (V.toList source, result)

unsafeMutableRoundTrip :: [Int]
unsafeMutableRoundTrip =
  linearly \linear ->
    freezeBoxed
      ( Growable.unsafeFromMutable
          (unsafePerformIO (V.thaw (V.fromList [1, 2])))
          linear
      )

unsafeVectorRoundTrip :: [Int]
unsafeVectorRoundTrip =
  linearly \linear ->
    freezeUnboxed
      ( Growable.unsafeFromVector
          (U.fromList [3, 4])
          linear
      )

test_backends :: TestTree
test_backends =
  testGroup
    "construction, growth, and backends"
    [ testCase "boxed backend preserves size and requested capacity" do
        let ((logicalSize, allocated), values) = boxedGrowth
        logicalSize @?= 3
        assertBool "capacity did not grow to the request" (allocated >= 10)
        values @?= [1, 2, 3]
    , testCase "unboxed backend grows and extends" do
        unboxedGrowth @?= [1, 2, 3, 4, 5]
    , testCase "primitive backend grows repeatedly" do
        primitiveGrowth @?= [7, 8, 9]
    , testCase "constant and empty materialize only initialized entries" do
        linearly
          ( \linear ->
              case dup linear of
                (emptyLinear, constantLinear) ->
                  ( freezeBoxed
                      (Growable.empty @V.Vector @Int emptyLinear)
                  , freezeUnboxed
                      ( Growable.constant @U.Vector
                          3
                          (4 :: Int)
                          constantLinear
                      )
                  )
          )
          @?= ([], [4, 4, 4])
    , testCase "fromVector copies its immutable source" do
        fromVectorCopy @?= ([1, 2, 3], [9, 2, 3])
    , testCase "unsafe constructors adopt complete initialized sources" do
        unsafeMutableRoundTrip @?= [1, 2]
        unsafeVectorRoundTrip @?= [3, 4]
    ]

boxedOperations :: ((Int, Int, Int), [Int], [Int])
boxedOperations =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2, 3] ownerLinear)
      (Ur first, vector) <- Growable.get 0 vector
      (Ur old, vector) <- Growable.set 1 20 vector
      vector <- Growable.write 2 30 vector
      (Ur auxiliary, vector) <-
        Growable.update
          0
          (\value -> Control.pure (Ur (value * 10), Ur (value + 1)))
          vector
      vector <-
        Growable.modify 1 (\value -> value NonLinear.+ 1) vector
      vector <- Growable.swap vector 0 2
      vector <- Growable.push 40 vector
      vector <- Growable.extend (V.fromList [50, 60]) vector
      (Ur snapshot, vector) <- Growable.copyToVector vector
      let !() = consume vector
      pureAfter
        ( (first, old, auxiliary)
        , V.toList snapshot
        , freezeBoxed (reclaim lend)
        )

sharedReads :: (Int, Int, Int)
sharedReads =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [11, 22, 33] ownerLinear)
      ((first, second, third), vector) <-
        sharing vector \shared -> Control.do
          Ur first <- Growable.copyAt 0 shared
          Ur second <- Growable.unsafeCopyAt 1 shared
          (third, shared) <-
            Growable.withContent shared \content ->
              move content & \(Ur content) -> Control.do
                Ur third <- Fixed.copyAt 2 content
                Control.pure third
          Control.pure
            (consume shared `lseq` (first, second, third))
      let !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur _ -> (first, second, third)
        )

ordinaryElementMultiplicity :: (Int, [Int])
ordinaryElementMultiplicity =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2] ownerLinear)
      let replacement = 7
      (Ur displaced, vector) <- Growable.set 0 replacement vector
      vector <- Growable.write 1 replacement vector
      vector <- Growable.push replacement vector
      let !() = consume vector
      pureAfter
        ( displaced + displaced
        , freezeBoxed (reclaim lend)
        )

test_operations :: TestTree
test_operations =
  testGroup
    "unrestricted element operations"
    [ testCase "get, set, write, update, modify, swap, push, and snapshot" do
        boxedOperations
          @?= ( (1, 2, 10)
              , [30, 21, 2, 40, 50, 60]
              , [30, 21, 2, 40, 50, 60]
              )
    , testCase "shared and fixed-content reads need no copy capability" do
        sharedReads @?= (11, 22, 33)
    , testCase "inputs, callbacks, and displaced values are unrestricted" do
        ordinaryElementMultiplicity @?= (2, [7, 7, 7])
    ]

mirroredSurface :: ([Int], Int, [Int])
mirroredSurface =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2] ownerLinear)
      vector <- Growable.reserveAdditional 5 vector
      (Ur allocated, vector) <- Control.pure (Growable.capacity vector)
      (Ur first, vector) <- Growable.head vector
      (Ur unsafeFirst, vector) <- Growable.unsafeHead vector
      (Ur final, vector) <- Growable.last vector
      (Ur unsafeFinal, vector) <- Growable.unsafeLast vector
      (Ur copiedFirst, vector) <- Growable.copyAtMut 0 vector
      (Ur copiedFinal, vector) <- Growable.unsafeCopyAtMut 1 vector
      vector <-
        Growable.withContent_ vector \content -> Control.do
          content <- Fixed.modify 0 (\value -> value NonLinear.+ 10) content
          Control.pure (consume content)
      let !() = consume vector
      pureAfter
        ( [first, unsafeFirst, final, unsafeFinal, copiedFirst, copiedFinal]
        , allocated
        , freezeBoxed (reclaim lend)
        )

test_mirroredSurface :: TestTree
test_mirroredSurface =
  testCase "mirrors read helpers, reserveAdditional, and withContent_" do
    let (observed, allocated, values) = mirroredSurface
    observed @?= [1, 1, 2, 2, 1, 2]
    assertBool "additional reserve did not grow capacity" (allocated >= 7)
    values @?= [11, 2]

contentSplit :: [Int]
contentSplit =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2, 3, 4] ownerLinear)
      ((), vector) <-
        Growable.withContent vector \content -> Control.do
          let !(left, right) = Fixed.splitAt 2 content
          (left, right) <-
            parBO
              (Fixed.modify 0 (\value -> value NonLinear.+ 10) left)
              (Fixed.modify 1 (\value -> value NonLinear.+ 20) right)
          Control.pure (consume (left, right))
      vector <- Growable.push 5 vector
      let !() = consume vector
      pureAfter (freezeBoxed (reclaim lend))

directContentLength :: (Int, [Int])
directContentLength =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2] ownerLinear)
      vector <- Growable.reserve 16 vector
      case Fixed.size (Growable.getContents vector) of
        (Ur logicalSize, content) ->
          let !() = consume content
           in pureAfter (logicalSize, freezeBoxed (reclaim lend))

test_content :: TestTree
test_content =
  testGroup
    "fixed content scopes"
    [ testCase "split mutation is safe before growth resumes" do
        contentSplit @?= [11, 2, 3, 24, 5]
    , testCase "direct projection excludes spare capacity" do
        directContentLength @?= (2, [1, 2])
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

trackedValues :: V.Vector Tracked -> [Int]
trackedValues vector =
  NonLinear.map trackedValue (V.toList vector)

capabilityFreeLifecycle ::
  IORef Int ->
  ([Int], [Int], [Int], [Int])
capabilityFreeLifecycle calls =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              (V.fromList [Tracked calls 1, Tracked calls 2])
              ownerLinear
          )
      vector <- Growable.reserve 8 vector
      vector <- Growable.push (Tracked calls 3) vector
      (cloned, vector) <- sharing vector (\shared -> clone shared)
      (Ur snapshot, vector) <- Growable.copyToVector vector
      (Ur first, vector) <- Growable.get 0 vector
      (Ur old, vector) <- Growable.set 1 (Tracked calls 4) vector
      vector <- Growable.write 0 (Tracked calls 5) vector
      let
        !() = consume vector
        clonedValues =
          case Growable.toVector cloned of
            Ur frozen -> trackedValues frozen
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen ->
              ( trackedValues snapshot
              , clonedValues
              , [trackedValue first, trackedValue old]
              , trackedValues frozen
              )
        )

referenceAliasAfterGrowth :: IORef Int -> [Int]
referenceAliasAfterGrowth reference =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromVector (V.singleton reference) ownerLinear)
      vector <- Growable.reserve 32 vector
      vector <- Growable.push reference vector
      (Ur observed, vector) <- Growable.get 0 vector
      let !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen ->
              unsafePerformIO do
                modifyIORef'
                  observed
                  (\value -> value NonLinear.+ 41)
                NonLinear.mapM readIORef (V.toList frozen)
        )

test_capabilities :: TestTree
test_capabilities =
  testGroup
    "capability-free GC-owned elements"
    [ testCase "the complete lifecycle invokes no element capability" do
        calls <- newIORef 0
        capabilityFreeLifecycle calls
          @?= ( [1, 2, 3]
              , [1, 2, 3]
              , [1, 2]
              , [5, 4, 3]
              )
        readIORef calls NonLinear.>>= (@?= 0)
    , testCase "growth preserves deliberate boxed aliases" do
        reference <- newIORef 1
        referenceAliasAfterGrowth reference @?= [42, 42]
        readIORef reference NonLinear.>>= (@?= 42)
    ]

getOutOfBounds :: Int -> Int
getOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2, 3] ownerLinear)
      (Ur value, vector) <- Growable.get index vector
      let !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen -> value + V.length frozen
        )

writeOutOfBounds :: Int -> Int
writeOutOfBounds index =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (Growable.fromList @V.Vector [1, 2, 3] ownerLinear)
      vector <- Growable.write index 4 vector
      let !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen -> V.sum frozen
        )

negativeCapacity :: Int
negativeCapacity =
  linearly \linear ->
    case Growable.toVector
      (Growable.withCapacity @V.Vector @Int (-1) linear) of
      Ur frozen -> V.length frozen

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
    [ testCase "get rejects a negative index" do
        assertErrorPrefix "get: index -1 out of bounds" (getOutOfBounds (-1))
    , testCase "get rejects the upper bound" do
        assertErrorPrefix "get: index 3 out of bounds" (getOutOfBounds 3)
    , testCase "write rejects a negative index" do
        assertErrorPrefix "write: index -1 out of bounds" (writeOutOfBounds (-1))
    , testCase "write rejects the upper bound" do
        assertErrorPrefix "write: index 3 out of bounds" (writeOutOfBounds 3)
    , testCase "construction rejects negative capacity" do
        assertErrorPrefix "withCapacity: negative capacity -1" negativeCapacity
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
    , testCase "element ownership cannot be coerced" do
        assertDeferredTypeError "representation" badOwnershipCoercionCase
    , testCase "get cannot manufacture an element borrow" do
        assertDeferredTypeError "Couldn't match" badElementBorrowCase
    , testCase "the mutable owner cannot be copied" do
        assertDeferredTypeError "cannot be copied!" badDuplicate
    , testCase "shared borrows cannot mutate" do
        assertDeferredTypeError "Couldn't match" badMutateSharedCase
    , testCase "shared borrows cannot grow" do
        assertDeferredTypeError "Couldn't match" badGrowSharedCase
    , testCase "fixed content cannot escape its scope" do
        assertDeferredTypeError "Couldn't match" badContentEscapeCase
    ]
