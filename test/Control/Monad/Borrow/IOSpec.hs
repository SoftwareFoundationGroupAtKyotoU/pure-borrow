{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}

module Control.Monad.Borrow.IOSpec (
  module Control.Monad.Borrow.IOSpec,
) where

import Control.Concurrent.DivideConquer.Linear qualified as DivideConquer
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO
import Control.Monad.Borrow.IO
import Control.Monad.Borrow.IO.TypingCases
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Vector qualified as Vector
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as BorrowVector
import Prelude.Linear qualified as Linear
import System.IO.Linear qualified as LinearIO
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

test_runners :: TestTree
test_runners =
  testGroup
    "BIO runners"
    [ testCase "effects are ordered with surrounding IO" do
        events <- newIORef ["before"]
        observed <- withBIO Control.do
          Linear.Ur () <- liftSystemIOU (modifyIORef' events (<> ["inside"]))
          Linear.Ur snapshot <- liftSystemIOU (readIORef events)
          pureAfter (Linear.Ur snapshot)
        modifyIORef' events (<> ["after"])
        observed @?= ["before", "inside"]
        readIORef events >>= (@?= ["before", "inside", "after"])
    , testCase "runBIOLend returns the owner after the lifetime ends" do
        result <- LinearIO.withLinearIO $ runBIOLend Control.do
          (mut, lend) <- borrowM (Linear.Ur (41 :: Int))
          Linear.consume mut `Linear.lseq` Control.pure lend
        result @?= 41
    , testCase "runBIO_ returns a direct linear result" do
        result <- LinearIO.withLinearIO $ runBIO_ (Control.pure (Linear.Ur (42 :: Int)))
        result @?= 42
    , testCase "liftBO preserves a pure computation inside BIO" do
        result <- withBIO Control.do
          answer <- liftBO (Control.pure (Linear.Ur (43 :: Int)))
          pureAfter answer
        result @?= 43
    , testCase "nested borrow scopes preserve effect order and mutations" do
        events <- newIORef ([] :: [String])
        result <- withBIO Control.do
          (mut, lend) <- borrowLinearlyM (BorrowVector.fromVector (Vector.singleton (0 :: Int)))
          Linear.Ur () <- liftSystemIOU (modifyIORef' events (<> ["before"]))
          restored <- reborrowing_ mut \inner -> Control.do
            Linear.Ur () <- liftSystemIOU (modifyIORef' events (<> ["inside"]))
            updated <- BorrowVector.write 0 37 inner
            Linear.consume updated `Linear.lseq` Control.pure ()
          Linear.Ur () <- liftSystemIOU (modifyIORef' events (<> ["after"]))
          Linear.consume restored `Linear.lseq` pureAfter (BorrowVector.toVector (reclaim lend))
        result @?= Vector.singleton 37
        readIORef events >>= (@?= ["before", "inside", "after"])
    , testCase "parallel BIO branches mutate disjoint slices" do
        events <- newIORef ([] :: [Int])
        result <- withBIO Control.do
          (mut, lend) <- borrowLinearlyM (BorrowVector.fromVector (Vector.fromList [0 :: Int, 0]))
          case BorrowVector.splitAt 1 mut of
            (left, right) -> Control.do
              (leftResult, rightResult) <-
                parBO
                  ( Control.do
                      Linear.Ur () <- liftSystemIOU (atomicModifyIORef' events (\seen -> (seen <> [1], ())))
                      BorrowVector.write 0 10 left
                  )
                  ( Control.do
                      Linear.Ur () <- liftSystemIOU (atomicModifyIORef' events (\seen -> (seen <> [2], ())))
                      BorrowVector.write 0 20 right
                  )
              Linear.consume (leftResult, rightResult) `Linear.lseq`
                pureAfter (BorrowVector.toVector (reclaim lend))
        result @?= Vector.fromList [10, 20]
        seen <- readIORef events
        assertBool "both branches completed" (1 `elem` seen && 2 `elem` seen)
    , testCase "the BIO scheduler completes effectful callbacks before returning" do
        callbacks <- newIORef (0 :: Int, 0 :: Int)
        result <- withBIO Control.do
          (mut :: Mut α (BorrowVector.Vector Vector.Vector Int), lend) <- borrowLinearlyM (BorrowVector.fromVector (Vector.fromList [5 :: Int, 2, 4, 1, 3]))
          let workload = DivideConquer.qsortDC' @Vector.Vector @Int @α 2
              effectful =
                workload
                  { DivideConquer.divide = \context slice -> Control.do
                      Linear.Ur () <- liftSystemIOU (atomicModifyIORef' callbacks (\(started, finished) -> ((started + 1, finished), ())))
                      divided <- DivideConquer.divide workload context slice
                      Linear.Ur () <- liftSystemIOU (atomicModifyIORef' callbacks (\(started, finished) -> ((started, finished + 1), ())))
                      Control.pure divided
                  }
          sorted <- DivideConquer.divideAndConquer (mkStdGen 42) 2 effectful mut
          Linear.consume sorted `Linear.lseq` pureAfter (BorrowVector.toVector (reclaim lend))
        result @?= Vector.fromList [1 .. 5]
        (started, finished) <- readIORef callbacks
        assertBool "the scheduler visited multiple subdivisions" (started > 1)
        finished @?= started
    ]

test_worldBoundaries :: TestTree
test_worldBoundaries =
  testGroup
    "world boundaries"
    [ expectDeferred "Pure rejects IO lifting" ["Impure", "Pure"] badPureLift
    , expectDeferred "nominal roles prevent BIO-to-BO coercion" ["Pure", "RealWorld"] badWorldCoercion
    , expectDeferred "unsafeLiftBIO still requires an impure destination" ["Impure", "Pure"] badPureUnsafeLift
    , expectDeferred "runBIO_ rejects a custom world" ["CustomWorld", "RealWorld"] badCustomRunner
    , expectDeferred "execBIO rejects a custom source world" ["CustomWorld", "RealWorld"] badCustomExecSource
    , expectDeferred "execBIO does not lift into a custom destination" ["BO'", "IO"] badCustomExecTarget
    , expectDeferred "boxed ST adapters reject BIO callbacks" ["Pure", "RealWorld"] badBoxedSTAdapter
    ]
  where
    expectDeferred description fragments action = testCase description do
      result <- try @SomeException (action >>= evaluate)
      case result of
        Left exception ->
          assertBool ("unexpected deferred type error: " <> displayException exception) $
            all (`isInfixOf` displayException exception) fragments
        Right _ -> assertFailure "an operation crossed a protected world boundary"
