{-# LANGUAGE BlockArguments #-}

module PureBorrow.Internal.Bench.Worklist.ResumeSpec (
  module PureBorrow.Internal.Bench.Worklist.ResumeSpec,
) where

import Data.Vector.Unboxed qualified as U
import PureBorrow.Internal.Bench.Worklist.Resume
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

test_worklistOpenOnceEvidence :: TestTree
test_worklistOpenOnceEvidence =
  testCase "open-once traversal preserves the frozen trace and direct output" do
    mapM_
      ( \target -> do
          let direct = worklistDirectOpenOnceRoot target
          summary direct @?= expectedSummary target 4 0 0 0
          assertFrozenOutput target direct
          worklistPureBorrowOpenOnceRoot target @?= direct
      )
      [minBound .. maxBound]

test_worklistReopenEvidence :: TestTree
test_worklistReopenEvidence =
  testCase "flat and nested reopen shapes preserve every frozen growth trace" do
    mapM_
      ( \(growth, target, flatOpens, nestedOpens, resumes, updates, growths) -> do
          let directFlat =
                worklistDirectReopenRoot FlatReopen growth target
              directNested =
                worklistDirectReopenRoot NestedReopen growth target
          summary directFlat
            @?= expectedSummary
              target
              flatOpens
              resumes
              updates
              growths
          summary directNested
            @?= expectedSummary
              target
              nestedOpens
              resumes
              updates
              growths
          assertFrozenOutput target directFlat
          assertSameTraversalOutput directFlat directNested
          worklistPureBorrowFlatReopenRoot growth target
            @?= directFlat
          worklistPureBorrowNestedReopenRoot growth target
            @?= directNested
      )
      reopenCases

test_worklistSeededEvidence :: TestTree
test_worklistSeededEvidence =
  testCase "fresh-run seeds preserve direct and Pure Borrow equivalence" do
    mapM_
      ( \seed ->
          mapM_
            ( \target ->
                worklistPureBorrowOpenOnceRootWithSeed seed target
                  @?= worklistDirectOpenOnceRootWithSeed seed target
            )
            [minBound .. maxBound]
      )
      [1, 37]
    mapM_
      ( \(label, root) -> do
          let seed1 = root 1
              seed37 = root 37
          assertBool
            (label <> " must include the seed in its digest")
            (finalDigest (summary seed1) /= finalDigest (summary seed37))
          root 1 @?= seed1
          root 37 @?= seed37
          root 1 @?= seed1
      )
      [ ("open-once", \seed -> worklistPureBorrowOpenOnceRootWithSeed seed Drain)
      ,
        ( "dense nested reopen"
        , \seed ->
            worklistPureBorrowNestedReopenRootWithSeed
              seed
              DenseGrowth
              Drain
        )
      ]
    mapM_
      ( \seed ->
          mapM_
            ( \(growth, target) -> do
                worklistPureBorrowFlatReopenRootWithSeed seed growth target
                  @?= worklistDirectReopenRootWithSeed
                    seed
                    FlatReopen
                    growth
                    target
                worklistPureBorrowNestedReopenRootWithSeed seed growth target
                  @?= worklistDirectReopenRootWithSeed
                    seed
                    NestedReopen
                    growth
                    target
            )
            [ (growth, target)
            | growth <- [minBound .. maxBound]
            , target <- [minBound .. maxBound]
            ]
      )
      [1, 37]

reopenCases ::
  [ ( WorklistGrowth
    , WorklistTarget
    , Int
    , Int
    , Int
    , Int
    , Int
    )
  ]
reopenCases =
  [ (NoGrowth, Drain, 84, 44, 20, 41, 0)
  , (NoGrowth, StopEarly, 44, 24, 10, 22, 0)
  , (SparseGrowth, Drain, 272, 138, 67, 131, 8)
  , (SparseGrowth, StopEarly, 100, 52, 24, 50, 7)
  , (DenseGrowth, Drain, 2056, 1030, 513, 934, 21)
  , (DenseGrowth, StopEarly, 692, 348, 172, 346, 19)
  ]

expectedSummary ::
  WorklistTarget ->
  Int ->
  Int ->
  Int ->
  Int ->
  WorklistSummary
expectedSummary target opens resumes updates growths =
  case target of
    Drain ->
      WorklistSummary
        { outcome = Drained
        , visitedNodes = 4096
        , enqueueTransitions = 4095
        , offsetReads = 8192
        , adjacencyReads = 12288
        , payloadReads = 12288
        , markReads = 12288
        , markWrites = 4095
        , queueReads = 4096
        , queueWrites = 4095
        , logWrites = 4096
        , contentOpens = opens
        , resumeBoundaries = resumes
        , headerUpdates = updates
        , bufferGrowths = growths
        , finalDigest = 2728622868939553119
        }
    StopEarly ->
      WorklistSummary
        { outcome = Stopped
        , visitedNodes = 1365
        , enqueueTransitions = 2888
        , offsetReads = 2730
        , adjacencyReads = 4095
        , payloadReads = 4095
        , markReads = 4095
        , markWrites = 2888
        , queueReads = 1365
        , queueWrites = 2888
        , logWrites = 1365
        , contentOpens = opens
        , resumeBoundaries = resumes
        , headerUpdates = updates
        , bufferGrowths = growths
        , finalDigest = 5952155574826728904
        }

assertFrozenOutput :: WorklistTarget -> WorklistOutput -> Assertion
assertFrozenOutput target result = do
  let resultSummary = summary result
      visits = visitedNodes resultSummary
      enqueues = enqueueTransitions resultSummary
  U.sum (finalMarks result) @?= enqueues + 1
  finalState result
    @?= U.fromList
      [ visits
      , enqueues + 1
      , visits
      , enqueues
      , visits
      ]
  U.length (finalQueue result) @?= enqueues + 1
  U.length (finalLog result) @?= visits
  outcome resultSummary
    @?= case target of
      Drain -> Drained
      StopEarly -> Stopped

assertSameTraversalOutput ::
  WorklistOutput ->
  WorklistOutput ->
  Assertion
assertSameTraversalOutput left right = do
  finalMarks left @?= finalMarks right
  finalState left @?= finalState right
  finalQueue left @?= finalQueue right
  finalLog left @?= finalLog right
