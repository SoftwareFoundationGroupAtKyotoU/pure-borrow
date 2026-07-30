{-# LANGUAGE BlockArguments #-}

module PureBorrow.Internal.Bench.MultiStoreScanSpec (
  module PureBorrow.Internal.Bench.MultiStoreScanSpec,
) where

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
    finalDigest (resultSummary result) @?= 7192365686207673759
    U.length (resultMarks result) @?= 4096
    U.length (resultScores result) @?= 4096
    let output =
          multiStoreScanDirectBenchmarkRoot multiStoreScanDirectInput
    outputDigest output @?= finalDigest (resultSummary result)
    outputMarks output @?= resultMarks result
    outputScores output @?= resultScores result
