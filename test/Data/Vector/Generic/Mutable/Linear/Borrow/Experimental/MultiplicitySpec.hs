{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.MultiplicitySpec (
  module Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.MultiplicitySpec,
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
import Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity qualified as Vector
import Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity.TypingCases
import GHC.Exts (Multiplicity (One))
import GHC.IO (unsafePerformIO)
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

data Tracked = Tracked
  { copyCalls :: !(IORef Int)
  , consumeCalls :: !(IORef Int)
  , duplicateCalls :: !(IORef Int)
  , moveCalls :: !(IORef Int)
  , trackedValue :: !Int
  }

instance Copyable Tracked where
  copy =
    Unsafe.toLinear \(UnsafeAlias tracked) ->
      case unsafePerformIO do
        consumed <- readIORef (consumeCalls tracked)
        if consumed == 0
          then modifyIORef' (copyCalls tracked) NonLinear.succ
          else NonLinear.error "copy invoked after source retirement" of
        () -> tracked

instance Consumable Tracked where
  consume =
    Unsafe.toLinear \tracked ->
      unsafePerformIO
        (modifyIORef' (consumeCalls tracked) NonLinear.succ)

instance Dupable Tracked where
  dup2 =
    Unsafe.toLinear \tracked ->
      case unsafePerformIO
        (modifyIORef' (duplicateCalls tracked) NonLinear.succ) of
        () -> (tracked, tracked)

instance Movable Tracked where
  move =
    Unsafe.toLinear \tracked ->
      case unsafePerformIO
        (modifyIORef' (moveCalls tracked) NonLinear.succ) of
        () -> Ur tracked

newTracked ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  Int ->
  Tracked
newTracked = Tracked

owningVector ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  Linearly %1 ->
  Vector.Vector One V.Vector Tracked
owningVector copies consumes duplicates moves =
  Vector.fromList @V.Vector
    [ newTracked copies consumes duplicates moves 10
    , newTracked copies consumes duplicates moves 20
    ]

consumeOwningVector ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  ()
consumeOwningVector copies consumes duplicates moves =
  linearly \linear ->
    consume (owningVector copies consumes duplicates moves linear)

getOwningElement ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  ()
getOwningElement copies consumes duplicates moves =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (owningVector copies consumes duplicates moves ownerLinear)
      element <- Vector.get 0 vector
      let !() = consume element
      pureAfter (consume (reclaim lend))

writeOwningElement ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  ()
writeOwningElement copies consumes duplicates moves =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (owningVector copies consumes duplicates moves ownerLinear)
      vector <-
        Vector.write
          0
          (newTracked copies consumes duplicates moves 30)
          vector
      let !() = consume vector
      pureAfter (consume (reclaim lend))

setOwningElement ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  ()
setOwningElement copies consumes duplicates moves =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (owningVector copies consumes duplicates moves ownerLinear)
      (displaced, vector) <-
        Vector.set
          0
          (newTracked copies consumes duplicates moves 30)
          vector
      let
        !() = consume displaced
        !() = consume vector
      pureAfter (consume (reclaim lend))

cloneOwningVector ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  ()
cloneOwningVector copies consumes duplicates moves =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (owningVector copies consumes duplicates moves ownerLinear)
      (cloned, vector) <- sharing vector (\shared -> clone shared)
      let !() = consume vector
      pureAfter
        (consume cloned `lseq` consume (reclaim lend))

finishCopied :: Tracked %1 -> Int
finishCopied =
  Unsafe.toLinear \(Tracked copies consumes duplicates moves value) ->
    consume (Tracked copies consumes duplicates moves value) `lseq` value

copyAtBeforeRecovery ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  Int
copyAtBeforeRecovery copies consumes duplicates moves =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (owningVector copies consumes duplicates moves ownerLinear)
      (copied, vector) <- Vector.copyAtMut 0 vector
      let !() = consume vector
      pureAfter
        ( consume (reclaim lend) `lseq`
            finishCopied copied
        )

copyVectorBeforeRecovery ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  [Int]
copyVectorBeforeRecovery copies consumes duplicates moves =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          (owningVector copies consumes duplicates moves ownerLinear)
      (Ur snapshot, vector) <- Vector.copyToVector vector
      let !() = consume vector
      pureAfter
        ( consume (reclaim lend) `lseq`
            NonLinear.map trackedValue (V.toList snapshot)
        )

moveOwningVector ::
  IORef Int ->
  IORef Int ->
  IORef Int ->
  IORef Int ->
  [Int]
moveOwningVector copies consumes duplicates moves =
  linearly \linear ->
    case Vector.toVector
      (owningVector copies consumes duplicates moves linear) of
      Ur vector -> NonLinear.map trackedValue (V.toList vector)

withCounters ::
  (IORef Int -> IORef Int -> IORef Int -> IORef Int -> Assertion) ->
  Assertion
withCounters assertion = do
  copies <- newIORef 0
  consumes <- newIORef 0
  duplicates <- newIORef 0
  moves <- newIORef 0
  assertion copies consumes duplicates moves

test_owning :: TestTree
test_owning =
  testGroup
    "owning elements"
    [ testCase "consuming the vector consumes every element" $
        withCounters \copies consumes duplicates moves -> do
          Exception.evaluate
            (consumeOwningVector copies consumes duplicates moves)
          readIORef consumes NonLinear.>>= (@?= 2)
    , testCase "get transfers the vector borrow to the element" $
        withCounters \copies consumes duplicates moves -> do
          Exception.evaluate
            (getOwningElement copies consumes duplicates moves)
          readIORef consumes NonLinear.>>= (@?= 2)
    , testCase "write consumes the displaced element exactly once" $
        withCounters \copies consumes duplicates moves -> do
          Exception.evaluate
            (writeOwningElement copies consumes duplicates moves)
          readIORef consumes NonLinear.>>= (@?= 3)
    , testCase "set returns the displaced element to its caller" $
        withCounters \copies consumes duplicates moves -> do
          Exception.evaluate
            (setOwningElement copies consumes duplicates moves)
          readIORef consumes NonLinear.>>= (@?= 3)
    , testCase "clone duplicates every owned element" $
        withCounters \copies consumes duplicates moves -> do
          Exception.evaluate
            (cloneOwningVector copies consumes duplicates moves)
          readIORef duplicates NonLinear.>>= (@?= 2)
          readIORef consumes NonLinear.>>= (@?= 4)
    , testCase "copyAtMut completes copying before owner recovery" $
        withCounters \copies consumes duplicates moves -> do
          copyAtBeforeRecovery copies consumes duplicates moves @?= 10
          readIORef copies NonLinear.>>= (@?= 1)
    , testCase "copyToVector copies every element before owner recovery" $
        withCounters \copies consumes duplicates moves -> do
          copyVectorBeforeRecovery copies consumes duplicates moves
            @?= [10, 20]
          readIORef copies NonLinear.>>= (@?= 2)
    , testCase "toVector moves every element into GC ownership" $
        withCounters \copies consumes duplicates moves -> do
          moveOwningVector copies consumes duplicates moves @?= [10, 20]
          readIORef moves NonLinear.>>= (@?= 2)
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
    [ testCase "owning get cannot also return the vector borrow" do
        assertDeferredTypeError "Couldn't match" badOwningGetCase
    , testCase "owning consumption requires Consumable elements" do
        assertDeferredTypeError "Consumable NoCapabilities" badConsumeCase
    , testCase "owning clone requires Dupable elements" do
        assertDeferredTypeError "Dupable" badCloneCase
    , testCase "owning copyToVector requires Copyable elements" do
        assertDeferredTypeError "Copyable" badCopyCase
    ]
