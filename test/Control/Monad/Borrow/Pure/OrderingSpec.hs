{-# LANGUAGE BlockArguments #-}

module Control.Monad.Borrow.Pure.OrderingSpec (
  module Control.Monad.Borrow.Pure.OrderingSpec,
) where

import Control.Exception qualified as Exception
import Control.Monad.Borrow.Pure.OrderingSpec.Kernels qualified as Optimised
import Control.Monad.Borrow.Pure.OrderingSpec.Owners qualified as Owners
import Control.Monad.Borrow.Pure.OrderingSpec.Unoptimised qualified as Unoptimised
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

test_ordering :: TestTree
test_ordering =
  testGroup
    "reads and writes through borrows are ordered by the state token"
    [ testGroup
        "-O0"
        ( cases
            Unoptimised.refUpdateWrites
            Unoptimised.hashMapTakeEmpties
            Unoptimised.growableSizeAfterReclaim
            Unoptimised.hashMapSizeAfterReclaim
            Unoptimised.refDupReclaimFresh
            Unoptimised.refDupReclaimOwners
        )
    , testGroup
        "-O2"
        ( cases
            Optimised.refUpdateWrites
            Optimised.hashMapTakeEmpties
            Optimised.growableSizeAfterReclaim
            Optimised.hashMapSizeAfterReclaim
            Optimised.refDupReclaimFresh
            Optimised.refDupReclaimOwners
        )
    ]
  where
    cases refUpdate hashMapTake growableSize hashMapSize refDupFresh refDupOwners =
      [ testCase "Ref.Borrow.update writes before the scope ends" do
          refUpdate @?= (0, 1)
      , testCase "HashMap.take_ leaves an empty table behind" do
          hashMapTake @?= ([(1, 10), (2, 20)], [(3, 30)])
      , testCase "a growable size read after reclaim sees the push" do
          growableSize @?= (3, 4)
      , testCase "a hash map size read after reclaim sees the insert" do
          hashMapSize @?= (2, 3)
      , testCase "an owner reclaimed after a scope is not served from an earlier dup2" do
          refDupFresh @?= (1, 0)
      , testCase "an owner reclaimed after a scope does not hand back a reference it gave away" do
          refDupOwners @?= (100, 1)
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
