{-# LANGUAGE BlockArguments #-}

module Control.Monad.Borrow.Pure.OrderingSpec (
  module Control.Monad.Borrow.Pure.OrderingSpec,
) where

import Control.Exception qualified as Exception
import Control.Monad.Borrow.Pure.OrderingSpec.Kernels qualified as Optimised
import Control.Monad.Borrow.Pure.OrderingSpec.Owners qualified as Owners
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- | The same kernels compiled at @-O0@ run in the component @pure-borrow-unoptimised@.
test_ordering :: TestTree
test_ordering =
  testGroup
    "reads and writes through borrows are ordered by the state token"
    [ testGroup
        "-O2"
        [ testCase "Ref.Borrow.update writes before the scope ends" do
            Optimised.refUpdateWrites @?= (0, 1)
        , testCase "HashMap.take_ leaves an empty table behind" do
            Optimised.hashMapTakeEmpties @?= ([(1, 10), (2, 20)], [(3, 30)])
        , testCase "a growable size read after reclaim sees the push" do
            Optimised.growableSizeAfterReclaim @?= (3, 4)
        , testCase "a hash map size read after reclaim sees the insert" do
            Optimised.hashMapSizeAfterReclaim @?= (2, 3)
        , testCase "an owner reclaimed after a scope is not served from an earlier dup2" do
            Optimised.refDupReclaimFresh @?= (1, 0)
        , testCase "an owner reclaimed after a scope does not hand back a reference it gave away" do
            Optimised.refDupReclaimOwners @?= (100, 1)
        ]
    ]

test_owners :: TestTree
test_owners =
  testGroup
    "an owner reclaimed after a scope sees the scope's writes"
    [ testGroup
        "-O2"
        [ testCase path (result @?= (1, 0))
        | (path, result) <- Owners.ownerKernels
        ]
    ]

test_earlyReclaim :: TestTree
test_earlyReclaim =
  testGroup
    "reclaiming needs the evidence that the lifetime has ended"
    [ testCase "reclaim forces a bottom end token supplied through withEnd" (throwsInstead Owners.earlyReclaim)
    , testCase "reclaim forces a bottom end token supplied through withDict" (throwsInstead Owners.earlyReclaimWithDict)
    ]
  where
    throwsInstead :: Int -> Assertion
    throwsInstead value = do
      result <- Exception.try @Exception.SomeException (Exception.evaluate value)
      case result of
        Left _ -> pure ()
        Right reclaimedEarly ->
          assertFailure ("the owner was reclaimed while borrowed, giving " <> show reclaimedEarly)
