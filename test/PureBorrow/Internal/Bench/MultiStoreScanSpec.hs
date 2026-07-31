{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module PureBorrow.Internal.Bench.MultiStoreScanSpec (
  module PureBorrow.Internal.Bench.MultiStoreScanSpec,
) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.List (isInfixOf)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import PureBorrow.Internal.Bench.MultiStoreScan
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

test_multiStoreScanDirectEvidence :: TestTree
test_multiStoreScanDirectEvidence =
  testCase "multi-store scan direct control preserves its frozen trace and digest" do
    let result = multiStoreScanDirectRoot multiStoreScanDirectInput
    visitedNodes (resultSummary result) @?= 4096
    elementReads (resultSummary result) @?= 24576
    elementWrites (resultSummary result) @?= 1742
    headerReads (resultSummary result) @?= 3
    validationReads (resultSummary result) @?= 8198
    finalDigest (resultSummary result) @?= 7192365686207673759
    resultVisitedIndices result @?= U.fromList [0 .. 4095]
    V.length (resultEvents result) @?= 26318
    resultEventDigest result @?= -6999049615496738955
    U.length (resultMarks result) @?= 4096
    U.length (resultScores result) @?= 4096
    let output =
          multiStoreScanDirectBenchmarkRoot multiStoreScanDirectInput
    outputDigest output @?= finalDigest (resultSummary result)
    outputMarks output @?= resultMarks result
    outputScores output @?= resultScores result

test_multiStoreScanPureBorrowEvidence :: TestTree
test_multiStoreScanPureBorrowEvidence =
  testCase "all-unrestricted Pure Borrow shapes preserve the exact trajectory" do
    let direct = multiStoreScanDirectRoot multiStoreScanDirectInput
        directOutput =
          multiStoreScanDirectBenchmarkRoot multiStoreScanDirectInput
    multiStoreScanPureBorrowDirectRoot multiStoreScanDirectInput
      @?= direct
    multiStoreScanPureBorrowNestedRoot multiStoreScanDirectInput
      @?= direct
    multiStoreScanPureBorrowDirectBenchmarkRoot multiStoreScanDirectInput
      @?= directOutput
    multiStoreScanPureBorrowNestedBenchmarkRoot multiStoreScanDirectInput
      @?= directOutput

test_multiStoreScanRejectsInvalidInputs :: TestTree
test_multiStoreScanRejectsInvalidInputs =
  testCase "all-unrestricted Pure Borrow shapes reject invalid inputs" do
    let standard = multiStoreScanDirectInput
        expectedDiagnostic =
          "multi-store scan requires six 4096-element vectors, in-range next indices, and zero links"
        invalidInputs =
          [ standard {inputNext = U.replicate 4095 0}
          , standard {inputWeight = U.replicate 4097 0}
          , standard {inputMark = U.replicate 4095 0}
          , standard {inputPayload = V.replicate 4097 (0, 0)}
          , standard {inputScore = U.replicate 4095 0}
          , standard {inputLink = U.replicate 4097 0}
          , standard {inputNext = U.replicate 4096 (-1)}
          , standard {inputNext = U.replicate 4096 4096}
          , standard {inputLink = U.replicate 4096 1}
          ]
        roots =
          [ multiStoreScanPureBorrowDirectRoot
          , multiStoreScanPureBorrowNestedRoot
          ]
    mapM_
      ( \root ->
          mapM_
            ( \input -> do
                outcome <-
                  try @SomeException (evaluate (root input))
                case outcome of
                  Left exception ->
                    let diagnostic = displayException exception
                     in assertBool
                          ("unexpected invalid-input diagnostic: " <> diagnostic)
                          (expectedDiagnostic `isInfixOf` diagnostic)
                  Right _ ->
                    assertFailure "invalid multi-store input was accepted"
            )
            invalidInputs
      )
      roots
