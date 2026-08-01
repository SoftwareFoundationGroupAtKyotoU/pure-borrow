{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Internal.Bench.Worklist.Resume (
  WorklistGrowth (..),
  WorklistOutcome (..),
  WorklistReopenShape (..),
  WorklistTarget (..),
  WorklistSummary (..),
  WorklistOutput (..),
  worklistBatchSize,
  worklistInitialCapacity,
  worklistNodeCount,
  worklistDirectOpenOnceRoot,
  worklistDirectOpenOnceRootWithSeed,
  worklistDirectReopenRoot,
  worklistDirectReopenRootWithSeed,
  worklistPureBorrowOpenOnceRoot,
  worklistPureBorrowOpenOnceRootWithSeed,
  worklistPureBorrowOpenOnceWorker,
  worklistPureBorrowOpenOnceEdgeWorker,
  worklistPureBorrowCheckedOpenOnceRoot,
  worklistPureBorrowCheckedOpenOnceRootWithSeed,
  worklistPureBorrowCheckedOpenOnceWorker,
  worklistPureBorrowCheckedOpenOnceEdgeWorker,
  worklistPureBorrowFlatReopenRoot,
  worklistPureBorrowFlatReopenRootWithSeed,
  worklistPureBorrowNestedReopenRoot,
  worklistPureBorrowNestedReopenRootWithSeed,
  worklistPureBorrowResumeWorker,
  worklistPureBorrowResumeEdgeWorker,
  benches,
  defaultMain,
) where

import Control.DeepSeq (NFData)
import Control.Functor.Linear qualified as Control
import Control.Monad (when)
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Borrows (
  Aliases (..),
  Muts,
  reborrowings,
 )
import Control.Monad.ST.Strict (ST, runST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Int (Int64)
import Data.Record.Linear.Borrow.Experimental.PatternMatch (
  RecordLabel,
  (.@),
 )
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Fixed
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Exts qualified as GHC
import GHC.Generics (Generic)
import Prelude.Linear (
  lseq,
  unur,
  (&),
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)
import Test.Tasty.Bench qualified as Bench

data WorklistGrowth
  = NoGrowth
  | NoGrowthBatch64
  | NoGrowthBatch8
  | SparseGrowth
  | DenseGrowth
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (NFData)

data WorklistTarget
  = Drain
  | StopEarly
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (NFData)

data WorklistReopenShape
  = FlatReopen
  | NestedReopen
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (NFData)

data WorklistOutcome
  = Drained
  | Stopped
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data WorklistSummary = WorklistSummary
  { outcome :: !WorklistOutcome
  , visitedNodes :: !Int
  , enqueueTransitions :: !Int
  , offsetReads :: !Int
  , adjacencyReads :: !Int
  , payloadReads :: !Int
  , markReads :: !Int
  , markWrites :: !Int
  , queueReads :: !Int
  , queueWrites :: !Int
  , logWrites :: !Int
  , contentOpens :: !Int
  , resumeBoundaries :: !Int
  , headerUpdates :: !Int
  , bufferGrowths :: !Int
  , finalDigest :: !Int64
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data WorklistOutput = WorklistOutput
  { summary :: !WorklistSummary
  , finalMarks :: !(U.Vector Int)
  , finalState :: !(U.Vector Int)
  , finalQueue :: !(U.Vector Int)
  , finalLog :: !(U.Vector Int)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data UnboxedHeader s
  = UnboxedHeader !Int !(UM.MVector s Int)

data BoxedHeader s
  = BoxedHeader !Int !(MV.MVector s (Int, Int))

data TraversalState = TraversalState
  { stateHead :: !Int
  , stateTail :: !Int
  , stateVisits :: !Int
  , stateEnqueues :: !Int
  , stateLogSize :: !Int
  , stateDigest :: !Int64
  }

data SegmentResult = SegmentResult
  { segmentState :: !TraversalState
  , segmentPendingRev :: ![Int]
  , segmentLogRev :: ![Int]
  , segmentStopped :: !Bool
  }

data ReopenEvidence = ReopenEvidence
  { reopenTraversal :: !TraversalState
  , reopenScopeCount :: !Int
  , reopenHeaderUpdates :: !Int
  , reopenGrowthCount :: !Int
  , reopenOutcome :: !WorklistOutcome
  }

data WorklistFixedRoots = WorklistFixedRoots
  { fixedOffsets :: !(Fixed.Vector U.Vector Int)
  , fixedMarks :: !(Fixed.Vector U.Vector Int)
  , fixedState :: !(Fixed.Vector U.Vector Int)
  }

data WorklistGraphRoots = WorklistGraphRoots
  { graphAdjacency :: !(Growable.GrowableVector U.Vector Int)
  , graphPayload :: !(Growable.GrowableVector V.Vector (Int, Int))
  }

data WorklistFrontierRoots = WorklistFrontierRoots
  { frontierQueue :: !(Growable.GrowableVector U.Vector Int)
  , frontierLog :: !(Growable.GrowableVector U.Vector Int)
  }

data WorklistStore = WorklistStore
  { worklistFixedRoots :: !WorklistFixedRoots
  , worklistGraphRoots :: !WorklistGraphRoots
  , worklistFrontierRoots :: !WorklistFrontierRoots
  }

type FlatWorklist α =
  Muts
    α
    '[ Fixed.Vector U.Vector Int
     , Fixed.Vector U.Vector Int
     , Fixed.Vector U.Vector Int
     , Growable.GrowableVector U.Vector Int
     , Growable.GrowableVector V.Vector (Int, Int)
     , Growable.GrowableVector U.Vector Int
     , Growable.GrowableVector U.Vector Int
     ]

type NestedFrontierWorklist α =
  Muts
    α
    '[ Growable.GrowableVector U.Vector Int
     , Growable.GrowableVector U.Vector Int
     ]

worklistNodeCount :: Int
worklistNodeCount = 4096

worklistDegree :: Int
worklistDegree = 3

worklistEdgeCount :: Int
worklistEdgeCount = worklistNodeCount * worklistDegree

worklistEarlyStop :: Int
worklistEarlyStop = 1365

defaultMain :: IO ()
defaultMain = Bench.defaultMain benches

benches :: [Benchmark]
benches =
  [ bgroup
      "worklist-open-once"
      [ bgroup
          (show target)
          [ bench "direct" $
              nf worklistDirectOpenOnceRoot target
          , bench "pure-borrow" $
              nf worklistPureBorrowOpenOnceRoot target
          , -- Same traversal through the checked public element surface. The
            -- difference against "pure-borrow" is the cost of the checked
            -- facade alone, and is reported separately from the
            -- safe-minus-direct excess.
            bench "pure-borrow-checked" $
              nf worklistPureBorrowCheckedOpenOnceRoot target
          ]
      | target <- [minBound .. maxBound]
      ]
  , bgroup
      "worklist-push-extend-reopen"
      [ bgroup
          (show growth)
          [ bgroup
              (show target)
              [ bench "direct-flat" $
                  nf
                    (worklistDirectReopenRoot FlatReopen growth)
                    target
              , bench "pure-borrow-flat" $
                  nf
                    (worklistPureBorrowFlatReopenRoot growth)
                    target
              , bench "direct-nested" $
                  nf
                    (worklistDirectReopenRoot NestedReopen growth)
                    target
              , bench "pure-borrow-nested" $
                  nf
                    (worklistPureBorrowNestedReopenRoot growth)
                    target
              ]
          | target <- [minBound .. maxBound]
          ]
      | growth <- [minBound .. maxBound]
      ]
  ]

worklistOffsets :: U.Vector Int
worklistOffsets =
  U.generate (worklistNodeCount + 1) \node ->
    node * worklistDegree

worklistAdjacency :: U.Vector Int
worklistAdjacency =
  U.generate worklistEdgeCount \edge ->
    let (node, slot) = edge `quotRem` worklistDegree
     in case slot of
          0 -> (node + 1) `rem` worklistNodeCount
          1 -> (node * 17 + 13) `rem` worklistNodeCount
          _ -> (node * 31 + 7) `rem` worklistNodeCount

worklistPayload :: V.Vector (Int, Int)
worklistPayload =
  V.generate worklistEdgeCount \edge ->
    let !node = edge `quot` worklistDegree
        !neighbor = worklistAdjacency U.! edge
     in (edge `rem` 11, (node + neighbor) `rem` 17)

targetVisits :: WorklistTarget -> Int
targetVisits Drain = worklistNodeCount
targetVisits StopEarly = worklistEarlyStop

initialMarks :: U.Vector Int
initialMarks =
  U.replicate worklistNodeCount 0 U.// [(0, 1)]

initialState :: U.Vector Int
initialState = U.fromListN 5 [0, 1, 0, 0, 0]

data WorklistStorage
  = OpenOnceStorage
  | ReopenStorage !WorklistGrowth

newWorklistStore :: WorklistStorage -> Linearly %1 -> WorklistStore
{-# NOINLINE newWorklistStore #-}
newWorklistStore =
  GHC.noinline \storage linear ->
    dup linear & \(offsetsLinear, rest1) ->
      dup rest1 & \(marksLinear, rest2) ->
        dup rest2 & \(stateLinear, rest3) ->
          dup rest3 & \(adjacencyLinear, rest4) ->
            dup rest4 & \(payloadLinear, rest5) ->
              dup rest5 & \(queueLinear, logLinear) ->
                WorklistStore
                  { worklistFixedRoots =
                      WorklistFixedRoots
                        { fixedOffsets =
                            Fixed.fromVector
                              worklistOffsets
                              offsetsLinear
                        , fixedMarks =
                            Fixed.fromVector
                              initialMarks
                              marksLinear
                        , fixedState =
                            Fixed.fromVector
                              initialState
                              stateLinear
                        }
                  , worklistGraphRoots =
                      WorklistGraphRoots
                        { graphAdjacency =
                            Growable.fromVector
                              worklistAdjacency
                              adjacencyLinear
                        , graphPayload =
                            Growable.fromVector
                              worklistPayload
                              payloadLinear
                        }
                  , worklistFrontierRoots =
                      WorklistFrontierRoots
                        { frontierQueue =
                            case storage of
                              OpenOnceStorage ->
                                Growable.fromVector
                                  ( U.cons
                                      0
                                      ( U.replicate
                                          (worklistNodeCount - 1)
                                          0
                                      )
                                  )
                                  queueLinear
                              ReopenStorage growth ->
                                Growable.withCapacity
                                  (worklistInitialCapacity growth)
                                  queueLinear
                        , frontierLog =
                            case storage of
                              OpenOnceStorage ->
                                Growable.fromVector
                                  (U.replicate worklistNodeCount 0)
                                  logLinear
                              ReopenStorage growth ->
                                Growable.withCapacity
                                  (worklistInitialCapacity growth)
                                  logLinear
                        }
                  }

worklistFixedRootsField ::
  RecordLabel WorklistStore "worklistFixedRoots" WorklistFixedRoots
worklistFixedRootsField = #worklistFixedRoots

worklistGraphRootsField ::
  RecordLabel WorklistStore "worklistGraphRoots" WorklistGraphRoots
worklistGraphRootsField = #worklistGraphRoots

worklistFrontierRootsField ::
  RecordLabel WorklistStore "worklistFrontierRoots" WorklistFrontierRoots
worklistFrontierRootsField = #worklistFrontierRoots

fixedOffsetsField ::
  RecordLabel
    WorklistFixedRoots
    "fixedOffsets"
    (Fixed.Vector U.Vector Int)
fixedOffsetsField = #fixedOffsets

fixedMarksField ::
  RecordLabel
    WorklistFixedRoots
    "fixedMarks"
    (Fixed.Vector U.Vector Int)
fixedMarksField = #fixedMarks

fixedStateField ::
  RecordLabel
    WorklistFixedRoots
    "fixedState"
    (Fixed.Vector U.Vector Int)
fixedStateField = #fixedState

graphAdjacencyField ::
  RecordLabel
    WorklistGraphRoots
    "graphAdjacency"
    (Growable.GrowableVector U.Vector Int)
graphAdjacencyField = #graphAdjacency

graphPayloadField ::
  RecordLabel
    WorklistGraphRoots
    "graphPayload"
    (Growable.GrowableVector V.Vector (Int, Int))
graphPayloadField = #graphPayload

frontierQueueField ::
  RecordLabel
    WorklistFrontierRoots
    "frontierQueue"
    (Growable.GrowableVector U.Vector Int)
frontierQueueField = #frontierQueue

frontierLogField ::
  RecordLabel
    WorklistFrontierRoots
    "frontierLog"
    (Growable.GrowableVector U.Vector Int)
frontierLogField = #frontierLog

worklistDirectOpenOnceRoot ::
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistDirectOpenOnceRoot #-}
worklistDirectOpenOnceRoot =
  worklistDirectOpenOnceRootWithSeed 0

worklistDirectOpenOnceRootWithSeed ::
  Int ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistDirectOpenOnceRootWithSeed #-}
worklistDirectOpenOnceRootWithSeed seed target =
  runST do
    offsets <- U.thaw worklistOffsets
    marks <- U.thaw initialMarks
    state <- U.thaw initialState
    adjacencyHeader <- newUnboxedHeaderFromVector worklistAdjacency
    payloadHeader <- newBoxedHeaderFromVector worklistPayload
    queueHeader <-
      newUnboxedHeaderFromVector
        (U.cons 0 (U.replicate (worklistNodeCount - 1) 0))
    logHeader <-
      newUnboxedHeaderFromVector
        (U.replicate worklistNodeCount 0)
    UnboxedHeader _ adjacency <- readSTRef adjacencyHeader
    BoxedHeader _ payload <- readSTRef payloadHeader
    UnboxedHeader _ queue <- readSTRef queueHeader
    UnboxedHeader _ outputLog <- readSTRef logHeader
    finalTraversal <-
      runOpenOnce
        (targetVisits target)
        offsets
        adjacency
        payload
        marks
        queue
        outputLog
        (TraversalState 0 1 0 0 0 (initialDigestForSeed seed))
    writeTraversalState state finalTraversal
    finishDirect
      (if stateVisits finalTraversal == targetVisits target && target == StopEarly then Stopped else Drained)
      4
      0
      0
      0
      offsets
      adjacency
      payload
      marks
      state
      queue
      outputLog
      finalTraversal

worklistDirectReopenRoot ::
  WorklistReopenShape ->
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistDirectReopenRoot #-}
worklistDirectReopenRoot =
  worklistDirectReopenRootWithSeed 0

worklistDirectReopenRootWithSeed ::
  Int ->
  WorklistReopenShape ->
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistDirectReopenRootWithSeed #-}
worklistDirectReopenRootWithSeed seed reopenShape growth target =
  runST do
    offsets <- U.thaw worklistOffsets
    marks <- U.thaw initialMarks
    state <- U.thaw initialState
    adjacencyHeader <- newUnboxedHeaderFromVector worklistAdjacency
    payloadHeader <- newBoxedHeaderFromVector worklistPayload
    queueHeader <- newUnboxedHeader (worklistInitialCapacity growth) U.empty
    _ <- appendUnboxed queueHeader (U.singleton 0)
    logHeader <- newUnboxedHeader (worklistInitialCapacity growth) U.empty
    (finalTraversal, scopeCount, headerUpdateCount, growthCount, finalOutcome) <-
      case reopenShape of
        FlatReopen ->
          runFlatReopenDirect
            growth
            target
            offsets
            marks
            state
            adjacencyHeader
            payloadHeader
            queueHeader
            logHeader
            (TraversalState 0 1 0 0 0 (initialDigestForSeed seed))
            0
            0
            0
        NestedReopen -> do
          UnboxedHeader _ adjacency <- readSTRef adjacencyHeader
          BoxedHeader _ payload <- readSTRef payloadHeader
          runNestedReopenDirect
            growth
            target
            offsets
            adjacency
            payload
            marks
            state
            queueHeader
            logHeader
            (TraversalState 0 1 0 0 0 (initialDigestForSeed seed))
            0
            0
            0
    UnboxedHeader _ adjacency <- readSTRef adjacencyHeader
    BoxedHeader _ payload <- readSTRef payloadHeader
    UnboxedHeader queueSize queue <- readSTRef queueHeader
    UnboxedHeader logSize outputLog <- readSTRef logHeader
    finishDirect
      finalOutcome
      ( case reopenShape of
          FlatReopen -> 4 * scopeCount
          NestedReopen -> 2 + 2 * scopeCount
      )
      (max 0 (scopeCount - 1))
      headerUpdateCount
      growthCount
      offsets
      adjacency
      payload
      marks
      state
      (UM.unsafeTake queueSize queue)
      (UM.unsafeTake logSize outputLog)
      finalTraversal

worklistPureBorrowOpenOnceRoot ::
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowOpenOnceRoot #-}
worklistPureBorrowOpenOnceRoot =
  worklistPureBorrowOpenOnceRootWithSeed 0

worklistPureBorrowOpenOnceRootWithSeed ::
  Int ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowOpenOnceRootWithSeed #-}
worklistPureBorrowOpenOnceRootWithSeed seed target =
  unur
    ( linearly \linear -> DataFlow.do
        (allocationLinear, borrowLinear) <- dup linear
        store <- newWorklistStore OpenOnceStorage allocationLinear
        runBO borrowLinear Control.do
          (storeBorrow, lender) <- borrowM store
          (Ur traversal, storeBorrow) <-
            reborrowing storeBorrow \local -> Control.do
              let %1 !(fixedRootBorrows, graphRootBorrows, frontierRootBorrows) =
                    local
                      .@ ( worklistFixedRootsField
                         , worklistGraphRootsField
                         , worklistFrontierRootsField
                         )
              let %1 !(offsetsBorrow, marksBorrow, stateBorrow) =
                    fixedRootBorrows
                      .@ (fixedOffsetsField, fixedMarksField, fixedStateField)
              let %1 !(adjacencyBorrow, payloadBorrow) =
                    graphRootBorrows
                      .@ (graphAdjacencyField, graphPayloadField)
              let %1 !(queueBorrow, logBorrow) =
                    frontierRootBorrows
                      .@ (frontierQueueField, frontierLogField)
              let %1 !adjacencyContent =
                    Growable.getContents adjacencyBorrow
              let %1 !payloadContent =
                    Growable.getContents payloadBorrow
              let %1 !queueContent =
                    Growable.getContents queueBorrow
              let %1 !logContent =
                    Growable.getContents logBorrow
              (Ur initial, stateBorrow) <-
                readTraversalState stateBorrow
              let !seededInitial =
                    initial
                      { stateDigest = initialDigestForSeed seed
                      }
              ( Ur traversal
                , offsetsBorrow
                , marksBorrow
                , stateBorrow
                , adjacencyContent
                , payloadContent
                , queueContent
                , logContent
                ) <-
                worklistPureBorrowOpenOnceWorker
                  (targetVisits target)
                  seededInitial
                  offsetsBorrow
                  marksBorrow
                  stateBorrow
                  adjacencyContent
                  payloadContent
                  queueContent
                  logContent
              let !() =
                    consumeWorklistViews
                      offsetsBorrow
                      marksBorrow
                      stateBorrow
                      adjacencyContent
                      payloadContent
                      queueContent
                      logContent
              Control.pure (Ur traversal)
          let !(Ur _) = share storeBorrow
          pureAfter
            ( finishWorklistStore
                (outcomeFor target traversal)
                4
                0
                0
                0
                traversal
                (reclaim lender)
            )
    )

{- | The open-once traversal, reading and writing through the /checked/ public
element surface instead of the unchecked one.

This is the copied-read attribution control. It differs from
'worklistPureBorrowOpenOnceRoot' in exactly one respect: every element access
goes through 'Fixed.copyAtMut' and 'Fixed.write' rather than
'Fixed.unsafeGet' and 'Fixed.unsafeWrite'. Ownership, lifetimes, projection
structure, transition counts and the final digest are identical, so the
allocation difference between the two roots is the cost of the public checked
facade alone: the bounds check, the @size@ call, the @Ur@ boxing, and whatever
survives of the @HasCallStack@ obligation those operations carry.

Both roots must therefore produce equal 'WorklistOutput'. Note that
@copyAtMut@ on this non-element-owning family is defined as @get@; it is not a
@Copyable@ copy, and this control deliberately does not switch to an
element-owning family, which would change the ownership mode and make the
allocation numbers incomparable.
-}
worklistPureBorrowCheckedOpenOnceRoot ::
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowCheckedOpenOnceRoot #-}
worklistPureBorrowCheckedOpenOnceRoot =
  worklistPureBorrowCheckedOpenOnceRootWithSeed 0

worklistPureBorrowCheckedOpenOnceRootWithSeed ::
  Int ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowCheckedOpenOnceRootWithSeed #-}
worklistPureBorrowCheckedOpenOnceRootWithSeed seed target =
  unur
    ( linearly \linear -> DataFlow.do
        (allocationLinear, borrowLinear) <- dup linear
        store <- newWorklistStore OpenOnceStorage allocationLinear
        runBO borrowLinear Control.do
          (storeBorrow, lender) <- borrowM store
          (Ur traversal, storeBorrow) <-
            reborrowing storeBorrow \local -> Control.do
              let %1 !(fixedRootBorrows, graphRootBorrows, frontierRootBorrows) =
                    local
                      .@ ( worklistFixedRootsField
                         , worklistGraphRootsField
                         , worklistFrontierRootsField
                         )
              let %1 !(offsetsBorrow, marksBorrow, stateBorrow) =
                    fixedRootBorrows
                      .@ (fixedOffsetsField, fixedMarksField, fixedStateField)
              let %1 !(adjacencyBorrow, payloadBorrow) =
                    graphRootBorrows
                      .@ (graphAdjacencyField, graphPayloadField)
              let %1 !(queueBorrow, logBorrow) =
                    frontierRootBorrows
                      .@ (frontierQueueField, frontierLogField)
              let %1 !adjacencyContent =
                    Growable.getContents adjacencyBorrow
              let %1 !payloadContent =
                    Growable.getContents payloadBorrow
              let %1 !queueContent =
                    Growable.getContents queueBorrow
              let %1 !logContent =
                    Growable.getContents logBorrow
              (Ur initial, stateBorrow) <-
                readTraversalStateChecked stateBorrow
              let !seededInitial =
                    initial
                      { stateDigest = initialDigestForSeed seed
                      }
              ( Ur traversal
                , offsetsBorrow
                , marksBorrow
                , stateBorrow
                , adjacencyContent
                , payloadContent
                , queueContent
                , logContent
                ) <-
                worklistPureBorrowCheckedOpenOnceWorker
                  (targetVisits target)
                  seededInitial
                  offsetsBorrow
                  marksBorrow
                  stateBorrow
                  adjacencyContent
                  payloadContent
                  queueContent
                  logContent
              let !() =
                    consumeWorklistViews
                      offsetsBorrow
                      marksBorrow
                      stateBorrow
                      adjacencyContent
                      payloadContent
                      queueContent
                      logContent
              Control.pure (Ur traversal)
          let !(Ur _) = share storeBorrow
          pureAfter
            ( finishWorklistStore
                (outcomeFor target traversal)
                4
                0
                0
                0
                traversal
                (reclaim lender)
            )
    )

worklistPureBorrowFlatReopenRoot ::
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowFlatReopenRoot #-}
worklistPureBorrowFlatReopenRoot =
  worklistPureBorrowReopenRoot FlatReopen

worklistPureBorrowFlatReopenRootWithSeed ::
  Int ->
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowFlatReopenRootWithSeed #-}
worklistPureBorrowFlatReopenRootWithSeed seed =
  worklistPureBorrowReopenRootWithSeed seed FlatReopen

worklistPureBorrowNestedReopenRoot ::
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowNestedReopenRoot #-}
worklistPureBorrowNestedReopenRoot =
  worklistPureBorrowReopenRoot NestedReopen

worklistPureBorrowNestedReopenRootWithSeed ::
  Int ->
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowNestedReopenRootWithSeed #-}
worklistPureBorrowNestedReopenRootWithSeed seed =
  worklistPureBorrowReopenRootWithSeed seed NestedReopen

worklistPureBorrowReopenRoot ::
  WorklistReopenShape ->
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowReopenRoot #-}
worklistPureBorrowReopenRoot =
  worklistPureBorrowReopenRootWithSeed 0

worklistPureBorrowReopenRootWithSeed ::
  Int ->
  WorklistReopenShape ->
  WorklistGrowth ->
  WorklistTarget ->
  WorklistOutput
{-# NOINLINE worklistPureBorrowReopenRootWithSeed #-}
worklistPureBorrowReopenRootWithSeed seed reopenShape growth target =
  unur
    ( linearly \linear -> DataFlow.do
        (allocationLinear, borrowLinear) <- dup linear
        store <-
          newWorklistStore
            (ReopenStorage growth)
            allocationLinear
        runBO borrowLinear Control.do
          (storeBorrow, lender) <- borrowM store
          (Ur evidence, storeBorrow) <-
            reborrowing storeBorrow \local -> Control.do
              let %1 !(fixedRootBorrows, graphRootBorrows, frontierRootBorrows) =
                    local
                      .@ ( worklistFixedRootsField
                         , worklistGraphRootsField
                         , worklistFrontierRootsField
                         )
              let %1 !(offsetsBorrow, marksBorrow, stateBorrow) =
                    fixedRootBorrows
                      .@ (fixedOffsetsField, fixedMarksField, fixedStateField)
              let %1 !(adjacencyBorrow, payloadBorrow) =
                    graphRootBorrows
                      .@ (graphAdjacencyField, graphPayloadField)
              let %1 !(queueBorrow, logBorrow) =
                    frontierRootBorrows
                      .@ (frontierQueueField, frontierLogField)
              queueBorrow <- Growable.push 0 queueBorrow
              (Ur initial, stateBorrow) <-
                readTraversalState stateBorrow
              let !seededInitial =
                    initial
                      { stateDigest = initialDigestForSeed seed
                      }
              case reopenShape of
                FlatReopen -> Control.do
                  (Ur evidence, fields) <-
                    runFlatReopenPureBorrow
                      growth
                      target
                      seededInitial
                      (worklistInitialCapacity growth)
                      (worklistInitialCapacity growth)
                      0
                      0
                      0
                      ( offsetsBorrow
                          :- marksBorrow
                          :- stateBorrow
                          :- adjacencyBorrow
                          :- payloadBorrow
                          :- queueBorrow
                          :- logBorrow
                          :- BNil
                      )
                  let !() = consume fields
                  Control.pure (Ur evidence)
                NestedReopen -> Control.do
                  let !(Ur sharedOffsets) = share offsetsBorrow
                  let !(Ur sharedAdjacency) = share adjacencyBorrow
                  let !(Ur sharedPayload) = share payloadBorrow
                  let !adjacencyContent =
                        Growable.getContents sharedAdjacency
                  let !payloadContent =
                        Growable.getContents sharedPayload
                  (Ur evidence, fixedFields) <-
                    reborrowings
                      (marksBorrow :- stateBorrow :- BNil)
                      \case
                        marks :- state :- BNil -> Control.do
                          ( Ur evidence
                            , marks
                            , state
                            , frontierFields
                            ) <-
                            runNestedReopenPureBorrow
                              growth
                              target
                              seededInitial
                              (worklistInitialCapacity growth)
                              (worklistInitialCapacity growth)
                              0
                              0
                              0
                              (subShare sharedOffsets)
                              (subShare adjacencyContent)
                              (subShare payloadContent)
                              marks
                              state
                              (upcast (queueBorrow :- logBorrow :- BNil))
                          let !(Ur _) = share marks
                              !(Ur _) = share state
                              !() = consume frontierFields
                          Control.pure (Ur evidence)
                  let !() = consume fixedFields
                  Control.pure (Ur evidence)
          let !(Ur _) = share storeBorrow
          pureAfter
            ( finishWorklistStore
                (reopenOutcome evidence)
                ( case reopenShape of
                    FlatReopen ->
                      4 * reopenScopeCount evidence
                    NestedReopen ->
                      2 + 2 * reopenScopeCount evidence
                )
                (max 0 (reopenScopeCount evidence - 1))
                (reopenHeaderUpdates evidence)
                (reopenGrowthCount evidence)
                (reopenTraversal evidence)
                (reclaim lender)
            )
    )

runFlatReopenPureBorrow ::
  WorklistGrowth ->
  WorklistTarget ->
  TraversalState ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  FlatWorklist α %1 ->
  BO α (Ur ReopenEvidence, FlatWorklist α)
runFlatReopenPureBorrow
  growth
  target
  current
  queueCapacity
  logCapacity
  scopes
  updates
  growths
  fields
    | stateVisits current >= targetVisits target =
        Control.pure
          ( Ur
              ( reopenEvidence
                  target
                  current
                  scopes
                  updates
                  growths
              )
          , fields
          )
    | stateHead current >= stateTail current =
        Control.pure
          ( Ur
              ReopenEvidence
                { reopenTraversal = current
                , reopenScopeCount = scopes
                , reopenHeaderUpdates = updates
                , reopenGrowthCount = growths
                , reopenOutcome = Drained
                }
          , fields
          )
    | otherwise = Control.do
        (Ur segment, fields) <-
          reborrowings fields \case
            offsets
              :- marks
              :- state
              :- adjacency
              :- payload
              :- queue
              :- outputLog
              :- BNil -> Control.do
                let %1 !adjacencyContent =
                      Growable.getContents adjacency
                let %1 !payloadContent =
                      Growable.getContents payload
                let %1 !queueContent =
                      Growable.getContents queue
                let %1 !logContent =
                      Growable.getContents outputLog
                ( Ur segment
                  , offsets
                  , marks
                  , state
                  , adjacencyContent
                  , payloadContent
                  , queueContent
                  , logContent
                  ) <-
                  worklistPureBorrowResumeWorker
                    (targetVisits target)
                    ( min
                        (stateTail current)
                        (stateHead current + worklistBatchSize growth)
                    )
                    current
                    []
                    []
                    offsets
                    marks
                    state
                    adjacencyContent
                    payloadContent
                    queueContent
                    logContent
                let !() =
                      consumeWorklistViews
                        offsets
                        marks
                        state
                        adjacencyContent
                        payloadContent
                        queueContent
                        logContent
                Control.pure (Ur segment)
        let %1 !( offsets
                    :- marks
                    :- state
                    :- adjacency
                    :- payload
                    :- queue
                    :- outputLog
                    :- BNil
                  ) = fields
        let !pending =
              U.fromList (reverse (segmentPendingRev segment))
            !newLogs =
              U.fromList (reverse (segmentLogRev segment))
            !nextTraversal = segmentState segment
            !( nextQueueCapacity
               , nextLogCapacity
               , nextGrowths
               ) =
                nextCapacities
                  queueCapacity
                  logCapacity
                  growths
                  pending
                  nextTraversal
            !nextUpdates =
              updates
                + (if U.null pending then 0 else 1)
                + 1
            !nextScopes = scopes + 1
        queue <-
          if U.null pending
            then Control.pure queue
            else Growable.extend pending queue
        outputLog <- Growable.extend newLogs outputLog
        state <-
          writeTraversalStatePureBorrow nextTraversal state
        let %1 !nextFields =
              offsets
                :- marks
                :- state
                :- adjacency
                :- payload
                :- queue
                :- outputLog
                :- BNil
        if segmentStopped segment
          then
            Control.pure
              ( Ur
                  ( reopenEvidence
                      target
                      nextTraversal
                      nextScopes
                      nextUpdates
                      nextGrowths
                  )
              , nextFields
              )
          else
            runFlatReopenPureBorrow
              growth
              target
              nextTraversal
              nextQueueCapacity
              nextLogCapacity
              nextScopes
              nextUpdates
              nextGrowths
              nextFields

runNestedReopenPureBorrow ::
  WorklistGrowth ->
  WorklistTarget ->
  TraversalState ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Share α (Fixed.Vector U.Vector Int) ->
  Share α (Fixed.Vector U.Vector Int) ->
  Share α (Fixed.Vector V.Vector (Int, Int)) ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  NestedFrontierWorklist α %1 ->
  BO
    α
    ( Ur ReopenEvidence
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , NestedFrontierWorklist α
    )
runNestedReopenPureBorrow
  growth
  target
  current
  queueCapacity
  logCapacity
  scopes
  updates
  growths
  offsets
  adjacency
  payload
  marks
  state
  fields
    | stateVisits current >= targetVisits target =
        Control.pure
          ( Ur
              ( reopenEvidence
                  target
                  current
                  scopes
                  updates
                  growths
              )
          , marks
          , state
          , fields
          )
    | stateHead current >= stateTail current =
        Control.pure
          ( Ur
              ReopenEvidence
                { reopenTraversal = current
                , reopenScopeCount = scopes
                , reopenHeaderUpdates = updates
                , reopenGrowthCount = growths
                , reopenOutcome = Drained
                }
          , marks
          , state
          , fields
          )
    | otherwise = Control.do
        ((Ur segment, marks, state), fields) <-
          reborrowings fields \case
            queue :- outputLog :- BNil -> Control.do
              let %1 !queueContent =
                    Growable.getContents queue
              let %1 !logContent =
                    Growable.getContents outputLog
              ( Ur segment
                , offsetsOccurrence
                , marks
                , state
                , adjacencyOccurrence
                , payloadOccurrence
                , queueContent
                , logContent
                ) <-
                worklistPureBorrowResumeWorker
                  (targetVisits target)
                  ( min
                      (stateTail current)
                      (stateHead current + worklistBatchSize growth)
                  )
                  current
                  []
                  []
                  offsets
                  marks
                  state
                  adjacency
                  payload
                  queueContent
                  logContent
              let !() = consume offsetsOccurrence
                  !() = consume adjacencyOccurrence
                  !() = consume payloadOccurrence
                  !(Ur _) = share queueContent
                  !(Ur _) = share logContent
              Control.pure (Ur segment, marks, state)
        let %1 !(queue :- outputLog :- BNil) =
              fields
        let !pending =
              U.fromList (reverse (segmentPendingRev segment))
            !newLogs =
              U.fromList (reverse (segmentLogRev segment))
            !nextTraversal = segmentState segment
            !( nextQueueCapacity
               , nextLogCapacity
               , nextGrowths
               ) =
                nextCapacities
                  queueCapacity
                  logCapacity
                  growths
                  pending
                  nextTraversal
            !nextUpdates =
              updates
                + (if U.null pending then 0 else 1)
                + 1
            !nextScopes = scopes + 1
        queue <-
          if U.null pending
            then Control.pure queue
            else Growable.extend pending queue
        outputLog <- Growable.extend newLogs outputLog
        state <-
          writeTraversalStatePureBorrow nextTraversal state
        let %1 !nextFields =
              queue :- outputLog :- BNil
        if segmentStopped segment
          then
            Control.pure
              ( Ur
                  ( reopenEvidence
                      target
                      nextTraversal
                      nextScopes
                      nextUpdates
                      nextGrowths
                  )
              , marks
              , state
              , nextFields
              )
          else
            runNestedReopenPureBorrow
              growth
              target
              nextTraversal
              nextQueueCapacity
              nextLogCapacity
              nextScopes
              nextUpdates
              nextGrowths
              offsets
              adjacency
              payload
              marks
              state
              nextFields

reopenEvidence ::
  WorklistTarget ->
  TraversalState ->
  Int ->
  Int ->
  Int ->
  ReopenEvidence
reopenEvidence target traversal scopes updates growths =
  ReopenEvidence
    { reopenTraversal = traversal
    , reopenScopeCount = scopes
    , reopenHeaderUpdates = updates
    , reopenGrowthCount = growths
    , reopenOutcome =
        case target of
          Drain -> Drained
          StopEarly -> Stopped
    }

nextCapacities ::
  Int ->
  Int ->
  Int ->
  U.Vector Int ->
  TraversalState ->
  (Int, Int, Int)
nextCapacities
  queueCapacity
  logCapacity
  growths
  pending
  traversal =
    let !nextQueueCapacity =
          if U.null pending
            then queueCapacity
            else growthTarget queueCapacity (stateTail traversal)
        !nextLogCapacity =
          growthTarget logCapacity (stateLogSize traversal)
        !nextGrowths =
          growths
            + fromEnum (nextQueueCapacity > queueCapacity)
            + fromEnum (nextLogCapacity > logCapacity)
     in (nextQueueCapacity, nextLogCapacity, nextGrowths)

worklistPureBorrowResumeWorker ::
  ( α >= ζ
  , β >= ζ
  , γ >= ζ
  , δ >= ζ
  , ε >= ζ
  ) =>
  Int ->
  Int ->
  TraversalState ->
  [Int] ->
  [Int] ->
  Borrow bk1 α (Fixed.Vector U.Vector Int) %1 ->
  Mut β (Fixed.Vector U.Vector Int) %1 ->
  Mut γ (Fixed.Vector U.Vector Int) %1 ->
  Borrow bk2 δ (Fixed.Vector U.Vector Int) %1 ->
  Borrow bk3 ε (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut ζ (Fixed.Vector U.Vector Int) %1 ->
  Mut ζ (Fixed.Vector U.Vector Int) %1 ->
  BO
    ζ
    ( Ur SegmentResult
    , Borrow bk1 α (Fixed.Vector U.Vector Int)
    , Mut β (Fixed.Vector U.Vector Int)
    , Mut γ (Fixed.Vector U.Vector Int)
    , Borrow bk2 δ (Fixed.Vector U.Vector Int)
    , Borrow bk3 ε (Fixed.Vector V.Vector (Int, Int))
    , Mut ζ (Fixed.Vector U.Vector Int)
    , Mut ζ (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE worklistPureBorrowResumeWorker #-}
worklistPureBorrowResumeWorker
  stopAfter
  snapshotTail
  current
  pendingRev
  logRev
  offsets
  marks
  state
  adjacency
  payload
  queue
  outputLog
    | stateVisits current >= stopAfter =
        Control.pure
          ( Ur (SegmentResult current pendingRev logRev True)
          , offsets
          , marks
          , state
          , adjacency
          , payload
          , queue
          , outputLog
          )
    | stateHead current >= snapshotTail =
        Control.pure
          ( Ur (SegmentResult current pendingRev logRev False)
          , offsets
          , marks
          , state
          , adjacency
          , payload
          , queue
          , outputLog
          )
    | otherwise = Control.do
        (Ur node, queue) <-
          Fixed.unsafeGet (stateHead current) queue
        (Ur start, offsets) <-
          Fixed.unsafeGet node offsets
        (Ur end, offsets) <-
          Fixed.unsafeGet (node + 1) offsets
        ( Ur (nextTail, nextEnqueues, nextPendingRev, nodeDigest)
          , adjacency
          , payload
          , marks
          ) <-
          worklistPureBorrowResumeEdgeWorker
            start
            end
            (stateTail current)
            (stateEnqueues current)
            pendingRev
            (stateDigest current)
            adjacency
            payload
            marks
        let !logValue = digestToLogValue nodeDigest node
            !nextState =
              TraversalState
                { stateHead = stateHead current + 1
                , stateTail = nextTail
                , stateVisits = stateVisits current + 1
                , stateEnqueues = nextEnqueues
                , stateLogSize = stateLogSize current + 1
                , stateDigest = mixDigest nodeDigest logValue
                }
        worklistPureBorrowResumeWorker
          stopAfter
          snapshotTail
          nextState
          nextPendingRev
          (logValue : logRev)
          offsets
          marks
          state
          adjacency
          payload
          queue
          outputLog

worklistPureBorrowResumeEdgeWorker ::
  (α >= δ, β >= δ, γ >= δ) =>
  Int ->
  Int ->
  Int ->
  Int ->
  [Int] ->
  Int64 ->
  Borrow bk1 α (Fixed.Vector U.Vector Int) %1 ->
  Borrow bk2 β (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut γ (Fixed.Vector U.Vector Int) %1 ->
  BO
    δ
    ( Ur (Int, Int, [Int], Int64)
    , Borrow bk1 α (Fixed.Vector U.Vector Int)
    , Borrow bk2 β (Fixed.Vector V.Vector (Int, Int))
    , Mut γ (Fixed.Vector U.Vector Int)
    )
-- This benchmark-internal export exists only to anchor optimized-Core
-- inspection. Its indices are unchecked; only the benchmark roots establish
-- the offset and neighbor bounds required by the unsafe element operations.
{-# INLINEABLE worklistPureBorrowResumeEdgeWorker #-}
worklistPureBorrowResumeEdgeWorker
  edge
  end
  tailIndex
  enqueues
  pendingRev
  digest
  adjacency
  payload
  marks
    | edge >= end =
        Control.pure
          ( Ur (tailIndex, enqueues, pendingRev, digest)
          , adjacency
          , payload
          , marks
          )
    | otherwise = Control.do
        (Ur neighbor, adjacency) <-
          Fixed.unsafeGet edge adjacency
        (Ur (tag, delta), payload) <-
          Fixed.unsafeGet edge payload
        (Ur marked, marks) <-
          Fixed.unsafeGet neighbor marks
        let !nextDigest =
              mixDigest
                digest
                (neighbor * 31 + tag * 17 + delta + marked)
        if marked == 0
          then Control.do
            marks <- Fixed.unsafeWrite neighbor 1 marks
            worklistPureBorrowResumeEdgeWorker
              (edge + 1)
              end
              (tailIndex + 1)
              (enqueues + 1)
              (neighbor : pendingRev)
              nextDigest
              adjacency
              payload
              marks
          else
            worklistPureBorrowResumeEdgeWorker
              (edge + 1)
              end
              tailIndex
              enqueues
              pendingRev
              nextDigest
              adjacency
              payload
              marks

worklistPureBorrowOpenOnceWorker ::
  Int ->
  TraversalState ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur TraversalState
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE worklistPureBorrowOpenOnceWorker #-}
worklistPureBorrowOpenOnceWorker
  stopAfter
  current
  offsets
  marks
  state
  adjacency
  payload
  queue
  outputLog
    | stateVisits current >= stopAfter
        || stateHead current >= stateTail current = Control.do
        state <- writeTraversalStatePureBorrow current state
        Control.pure
          ( Ur current
          , offsets
          , marks
          , state
          , adjacency
          , payload
          , queue
          , outputLog
          )
    | otherwise = Control.do
        (Ur node, queue) <-
          Fixed.unsafeGet (stateHead current) queue
        (Ur start, offsets) <-
          Fixed.unsafeGet node offsets
        (Ur end, offsets) <-
          Fixed.unsafeGet (node + 1) offsets
        ( Ur (nextTail, nextEnqueues, nodeDigest)
          , adjacency
          , payload
          , marks
          , queue
          ) <-
          worklistPureBorrowOpenOnceEdgeWorker
            start
            end
            (stateTail current)
            (stateEnqueues current)
            (stateDigest current)
            adjacency
            payload
            marks
            queue
        let !logValue = digestToLogValue nodeDigest node
        outputLog <-
          Fixed.unsafeWrite
            (stateLogSize current)
            logValue
            outputLog
        let !nextState =
              TraversalState
                { stateHead = stateHead current + 1
                , stateTail = nextTail
                , stateVisits = stateVisits current + 1
                , stateEnqueues = nextEnqueues
                , stateLogSize = stateLogSize current + 1
                , stateDigest = mixDigest nodeDigest logValue
                }
        worklistPureBorrowOpenOnceWorker
          stopAfter
          nextState
          offsets
          marks
          state
          adjacency
          payload
          queue
          outputLog

worklistPureBorrowOpenOnceEdgeWorker ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur (Int, Int, Int64)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
-- This benchmark-internal export exists only to anchor optimized-Core
-- inspection. Its indices are unchecked; only the benchmark roots establish
-- the edge, neighbor, and queue-capacity bounds required by the unsafe element
-- operations.
{-# INLINEABLE worklistPureBorrowOpenOnceEdgeWorker #-}
worklistPureBorrowOpenOnceEdgeWorker
  edge
  end
  tailIndex
  enqueues
  digest
  adjacency
  payload
  marks
  queue
    | edge >= end =
        Control.pure
          ( Ur (tailIndex, enqueues, digest)
          , adjacency
          , payload
          , marks
          , queue
          )
    | otherwise = Control.do
        (Ur neighbor, adjacency) <-
          Fixed.unsafeGet edge adjacency
        (Ur (tag, delta), payload) <-
          Fixed.unsafeGet edge payload
        (Ur marked, marks) <-
          Fixed.unsafeGet neighbor marks
        let !nextDigest =
              mixDigest
                digest
                (neighbor * 31 + tag * 17 + delta + marked)
        if marked == 0
          then Control.do
            marks <-
              Fixed.unsafeWrite neighbor 1 marks
            queue <-
              Fixed.unsafeWrite tailIndex neighbor queue
            worklistPureBorrowOpenOnceEdgeWorker
              (edge + 1)
              end
              (tailIndex + 1)
              (enqueues + 1)
              nextDigest
              adjacency
              payload
              marks
              queue
          else
            worklistPureBorrowOpenOnceEdgeWorker
              (edge + 1)
              end
              tailIndex
              enqueues
              nextDigest
              adjacency
              payload
              marks
              queue

{- | Checked counterpart of 'worklistPureBorrowOpenOnceWorker'.

Every element access uses the checked public entry point. The traversal,
transition counts and digest are identical to the unchecked worker; only the
element-access surface differs.
-}
worklistPureBorrowCheckedOpenOnceWorker ::
  Int ->
  TraversalState ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur TraversalState
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE worklistPureBorrowCheckedOpenOnceWorker #-}
worklistPureBorrowCheckedOpenOnceWorker
  stopAfter
  current
  offsets
  marks
  state
  adjacency
  payload
  queue
  outputLog
    | stateVisits current >= stopAfter
        || stateHead current >= stateTail current = Control.do
        state <- writeTraversalStateCheckedPureBorrow current state
        Control.pure
          ( Ur current
          , offsets
          , marks
          , state
          , adjacency
          , payload
          , queue
          , outputLog
          )
    | otherwise = Control.do
        (Ur node, queue) <-
          Fixed.copyAtMut (stateHead current) queue
        (Ur start, offsets) <-
          Fixed.copyAtMut node offsets
        (Ur end, offsets) <-
          Fixed.copyAtMut (node + 1) offsets
        ( Ur (nextTail, nextEnqueues, nodeDigest)
          , adjacency
          , payload
          , marks
          , queue
          ) <-
          worklistPureBorrowCheckedOpenOnceEdgeWorker
            start
            end
            (stateTail current)
            (stateEnqueues current)
            (stateDigest current)
            adjacency
            payload
            marks
            queue
        let !logValue = digestToLogValue nodeDigest node
        outputLog <-
          Fixed.write
            (stateLogSize current)
            logValue
            outputLog
        let !nextState =
              TraversalState
                { stateHead = stateHead current + 1
                , stateTail = nextTail
                , stateVisits = stateVisits current + 1
                , stateEnqueues = nextEnqueues
                , stateLogSize = stateLogSize current + 1
                , stateDigest = mixDigest nodeDigest logValue
                }
        worklistPureBorrowCheckedOpenOnceWorker
          stopAfter
          nextState
          offsets
          marks
          state
          adjacency
          payload
          queue
          outputLog

{- | Checked counterpart of 'worklistPureBorrowOpenOnceEdgeWorker'.

Unlike its unchecked sibling this worker establishes its own bounds, so it
carries no unchecked-access proof obligation. It exists to anchor the
checked-surface attribution measurement.
-}
worklistPureBorrowCheckedOpenOnceEdgeWorker ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur (Int, Int, Int64)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# INLINEABLE worklistPureBorrowCheckedOpenOnceEdgeWorker #-}
worklistPureBorrowCheckedOpenOnceEdgeWorker
  edge
  end
  tailIndex
  enqueues
  digest
  adjacency
  payload
  marks
  queue
    | edge >= end =
        Control.pure
          ( Ur (tailIndex, enqueues, digest)
          , adjacency
          , payload
          , marks
          , queue
          )
    | otherwise = Control.do
        (Ur neighbor, adjacency) <-
          Fixed.copyAtMut edge adjacency
        (Ur (tag, delta), payload) <-
          Fixed.copyAtMut edge payload
        (Ur marked, marks) <-
          Fixed.copyAtMut neighbor marks
        let !nextDigest =
              mixDigest
                digest
                (neighbor * 31 + tag * 17 + delta + marked)
        if marked == 0
          then Control.do
            marks <-
              Fixed.write neighbor 1 marks
            queue <-
              Fixed.write tailIndex neighbor queue
            worklistPureBorrowCheckedOpenOnceEdgeWorker
              (edge + 1)
              end
              (tailIndex + 1)
              (enqueues + 1)
              nextDigest
              adjacency
              payload
              marks
              queue
          else
            worklistPureBorrowCheckedOpenOnceEdgeWorker
              (edge + 1)
              end
              tailIndex
              enqueues
              nextDigest
              adjacency
              payload
              marks
              queue

readTraversalStateChecked ::
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur TraversalState
    , Mut α (Fixed.Vector U.Vector Int)
    )
readTraversalStateChecked state = Control.do
  (Ur headIndex, state) <- Fixed.copyAtMut 0 state
  (Ur tailIndex, state) <- Fixed.copyAtMut 1 state
  (Ur visits, state) <- Fixed.copyAtMut 2 state
  (Ur enqueues, state) <- Fixed.copyAtMut 3 state
  (Ur logSize, state) <- Fixed.copyAtMut 4 state
  Control.pure
    ( Ur
        TraversalState
          { stateHead = headIndex
          , stateTail = tailIndex
          , stateVisits = visits
          , stateEnqueues = enqueues
          , stateLogSize = logSize
          , stateDigest = initialDigest
          }
    , state
    )

writeTraversalStateCheckedPureBorrow ::
  TraversalState ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO α (Mut α (Fixed.Vector U.Vector Int))
writeTraversalStateCheckedPureBorrow traversal state = Control.do
  state <- Fixed.write 0 (stateHead traversal) state
  state <- Fixed.write 1 (stateTail traversal) state
  state <- Fixed.write 2 (stateVisits traversal) state
  state <- Fixed.write 3 (stateEnqueues traversal) state
  Fixed.write 4 (stateLogSize traversal) state

readTraversalState ::
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur TraversalState
    , Mut α (Fixed.Vector U.Vector Int)
    )
readTraversalState state = Control.do
  (Ur headIndex, state) <- Fixed.unsafeGet 0 state
  (Ur tailIndex, state) <- Fixed.unsafeGet 1 state
  (Ur visits, state) <- Fixed.unsafeGet 2 state
  (Ur enqueues, state) <- Fixed.unsafeGet 3 state
  (Ur logSize, state) <- Fixed.unsafeGet 4 state
  Control.pure
    ( Ur
        TraversalState
          { stateHead = headIndex
          , stateTail = tailIndex
          , stateVisits = visits
          , stateEnqueues = enqueues
          , stateLogSize = logSize
          , stateDigest = initialDigest
          }
    , state
    )

writeTraversalStatePureBorrow ::
  TraversalState ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO α (Mut α (Fixed.Vector U.Vector Int))
writeTraversalStatePureBorrow traversal state = Control.do
  state <- Fixed.unsafeWrite 0 (stateHead traversal) state
  state <- Fixed.unsafeWrite 1 (stateTail traversal) state
  state <- Fixed.unsafeWrite 2 (stateVisits traversal) state
  state <- Fixed.unsafeWrite 3 (stateEnqueues traversal) state
  Fixed.unsafeWrite 4 (stateLogSize traversal) state

consumeWorklistViews ::
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  ()
consumeWorklistViews
  offsets
  marks
  state
  adjacency
  payload
  queue
  outputLog =
    let !(Ur _) = share offsets
        !(Ur _) = share marks
        !(Ur _) = share state
        !(Ur _) = share adjacency
        !(Ur _) = share payload
        !(Ur _) = share queue
        !(Ur _) = share outputLog
     in ()

outcomeFor :: WorklistTarget -> TraversalState -> WorklistOutcome
outcomeFor target traversal
  | target == StopEarly
      && stateVisits traversal >= targetVisits target =
      Stopped
  | otherwise =
      Drained

runOpenOnce ::
  Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  TraversalState ->
  ST s TraversalState
runOpenOnce stopAfter offsets adjacency payload marks queue outputLog current
  | stateVisits current >= stopAfter =
      pure current
  | stateHead current >= stateTail current =
      pure current
  | otherwise = do
      node <- UM.unsafeRead queue (stateHead current)
      start <- UM.unsafeRead offsets node
      end <- UM.unsafeRead offsets (node + 1)
      (nextTail, nextEnqueues, nodeDigest) <-
        scanEdgesOpenOnce
          start
          end
          adjacency
          payload
          marks
          queue
          (stateTail current)
          (stateEnqueues current)
          (stateDigest current)
      let !logValue = digestToLogValue nodeDigest node
      UM.unsafeWrite outputLog (stateLogSize current) logValue
      let !nextState =
            TraversalState
              { stateHead = stateHead current + 1
              , stateTail = nextTail
              , stateVisits = stateVisits current + 1
              , stateEnqueues = nextEnqueues
              , stateLogSize = stateLogSize current + 1
              , stateDigest = mixDigest nodeDigest logValue
              }
      runOpenOnce
        stopAfter
        offsets
        adjacency
        payload
        marks
        queue
        outputLog
        nextState

scanEdgesOpenOnce ::
  Int ->
  Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  Int ->
  Int ->
  Int64 ->
  ST s (Int, Int, Int64)
scanEdgesOpenOnce edge end adjacency payload marks queue tailIndex enqueues digest
  | edge >= end =
      pure (tailIndex, enqueues, digest)
  | otherwise = do
      neighbor <- UM.unsafeRead adjacency edge
      (tag, delta) <- MV.unsafeRead payload edge
      marked <- UM.unsafeRead marks neighbor
      let !nextDigest =
            mixDigest
              digest
              (neighbor * 31 + tag * 17 + delta + marked)
      if marked == 0
        then do
          UM.unsafeWrite marks neighbor 1
          UM.unsafeWrite queue tailIndex neighbor
          scanEdgesOpenOnce
            (edge + 1)
            end
            adjacency
            payload
            marks
            queue
            (tailIndex + 1)
            (enqueues + 1)
            nextDigest
        else
          scanEdgesOpenOnce
            (edge + 1)
            end
            adjacency
            payload
            marks
            queue
            tailIndex
            enqueues
            nextDigest

runFlatReopenDirect ::
  WorklistGrowth ->
  WorklistTarget ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  STRef s (UnboxedHeader s) ->
  STRef s (BoxedHeader s) ->
  STRef s (UnboxedHeader s) ->
  STRef s (UnboxedHeader s) ->
  TraversalState ->
  Int ->
  Int ->
  Int ->
  ST s (TraversalState, Int, Int, Int, WorklistOutcome)
runFlatReopenDirect
  growth
  target
  offsets
  marks
  state
  adjacencyHeader
  payloadHeader
  queueHeader
  logHeader
  current
  scopes
  updates
  growths
    | stateVisits current >= targetVisits target =
        pure
          ( current
          , scopes
          , updates
          , growths
          , case target of
              Drain -> Drained
              StopEarly -> Stopped
          )
    | stateHead current >= stateTail current =
        pure (current, scopes, updates, growths, Drained)
    | otherwise = do
        UnboxedHeader _ adjacency <- readSTRef adjacencyHeader
        BoxedHeader _ payload <- readSTRef payloadHeader
        (segment, nextScopes, nextUpdates, nextGrowths) <-
          runReopenSegmentDirect
            growth
            target
            offsets
            adjacency
            payload
            marks
            state
            queueHeader
            logHeader
            current
            scopes
            updates
            growths
        if segmentStopped segment
          then
            pure
              ( segmentState segment
              , nextScopes
              , nextUpdates
              , nextGrowths
              , case target of
                  Drain -> Drained
                  StopEarly -> Stopped
              )
          else
            runFlatReopenDirect
              growth
              target
              offsets
              marks
              state
              adjacencyHeader
              payloadHeader
              queueHeader
              logHeader
              (segmentState segment)
              nextScopes
              nextUpdates
              nextGrowths

runNestedReopenDirect ::
  WorklistGrowth ->
  WorklistTarget ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  STRef s (UnboxedHeader s) ->
  STRef s (UnboxedHeader s) ->
  TraversalState ->
  Int ->
  Int ->
  Int ->
  ST s (TraversalState, Int, Int, Int, WorklistOutcome)
runNestedReopenDirect
  growth
  target
  offsets
  adjacency
  payload
  marks
  state
  queueHeader
  logHeader
  current
  scopes
  updates
  growths
    | stateVisits current >= targetVisits target =
        pure
          ( current
          , scopes
          , updates
          , growths
          , case target of
              Drain -> Drained
              StopEarly -> Stopped
          )
    | stateHead current >= stateTail current =
        pure (current, scopes, updates, growths, Drained)
    | otherwise = do
        (segment, nextScopes, nextUpdates, nextGrowths) <-
          runReopenSegmentDirect
            growth
            target
            offsets
            adjacency
            payload
            marks
            state
            queueHeader
            logHeader
            current
            scopes
            updates
            growths
        if segmentStopped segment
          then
            pure
              ( segmentState segment
              , nextScopes
              , nextUpdates
              , nextGrowths
              , case target of
                  Drain -> Drained
                  StopEarly -> Stopped
              )
          else
            runNestedReopenDirect
              growth
              target
              offsets
              adjacency
              payload
              marks
              state
              queueHeader
              logHeader
              (segmentState segment)
              nextScopes
              nextUpdates
              nextGrowths

runReopenSegmentDirect ::
  WorklistGrowth ->
  WorklistTarget ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  STRef s (UnboxedHeader s) ->
  STRef s (UnboxedHeader s) ->
  TraversalState ->
  Int ->
  Int ->
  Int ->
  ST s (SegmentResult, Int, Int, Int)
{-# INLINE runReopenSegmentDirect #-}
runReopenSegmentDirect
  growth
  target
  offsets
  adjacency
  payload
  marks
  state
  queueHeader
  logHeader
  current
  scopes
  updates
  growths = do
    UnboxedHeader queueSize queue <- readSTRef queueHeader
    UnboxedHeader _ outputLog <- readSTRef logHeader
    when (queueSize /= stateTail current) $
      error "worklist queue header and scalar tail diverged"
    when (UM.length outputLog < stateLogSize current) $
      error "worklist log header and scalar frontier diverged"
    segment <-
      runSegment
        (targetVisits target)
        (min queueSize (stateHead current + worklistBatchSize growth))
        offsets
        adjacency
        payload
        marks
        queue
        current
        []
        []
    let !pending = U.fromList (reverse (segmentPendingRev segment))
        !newLogs = U.fromList (reverse (segmentLogRev segment))
    queueGrew <-
      if U.null pending
        then pure False
        else appendUnboxed queueHeader pending
    logGrew <- appendUnboxed logHeader newLogs
    writeTraversalState state (segmentState segment)
    let !nextScopes = scopes + 1
        !nextUpdates =
          updates
            + (if U.null pending then 0 else 1)
            + 1
        !nextGrowths =
          growths
            + fromEnum queueGrew
            + fromEnum logGrew
    pure (segment, nextScopes, nextUpdates, nextGrowths)

runSegment ::
  Int ->
  Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  TraversalState ->
  [Int] ->
  [Int] ->
  ST s SegmentResult
runSegment stopAfter snapshotTail offsets adjacency payload marks queue current queueRev logRev
  | stateVisits current >= stopAfter =
      pure (SegmentResult current queueRev logRev True)
  | stateHead current >= snapshotTail =
      pure (SegmentResult current queueRev logRev False)
  | otherwise = do
      node <- UM.unsafeRead queue (stateHead current)
      start <- UM.unsafeRead offsets node
      end <- UM.unsafeRead offsets (node + 1)
      (nextTail, nextEnqueues, nextQueueRev, nodeDigest) <-
        scanEdgesSegment
          start
          end
          adjacency
          payload
          marks
          (stateTail current)
          (stateEnqueues current)
          queueRev
          (stateDigest current)
      let !logValue = digestToLogValue nodeDigest node
          !nextState =
            TraversalState
              { stateHead = stateHead current + 1
              , stateTail = nextTail
              , stateVisits = stateVisits current + 1
              , stateEnqueues = nextEnqueues
              , stateLogSize = stateLogSize current + 1
              , stateDigest = mixDigest nodeDigest logValue
              }
      runSegment
        stopAfter
        snapshotTail
        offsets
        adjacency
        payload
        marks
        queue
        nextState
        nextQueueRev
        (logValue : logRev)

scanEdgesSegment ::
  Int ->
  Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  Int ->
  Int ->
  [Int] ->
  Int64 ->
  ST s (Int, Int, [Int], Int64)
scanEdgesSegment edge end adjacency payload marks tailIndex enqueues queueRev digest
  | edge >= end =
      pure (tailIndex, enqueues, queueRev, digest)
  | otherwise = do
      neighbor <- UM.unsafeRead adjacency edge
      (tag, delta) <- MV.unsafeRead payload edge
      marked <- UM.unsafeRead marks neighbor
      let !nextDigest =
            mixDigest
              digest
              (neighbor * 31 + tag * 17 + delta + marked)
      if marked == 0
        then do
          UM.unsafeWrite marks neighbor 1
          scanEdgesSegment
            (edge + 1)
            end
            adjacency
            payload
            marks
            (tailIndex + 1)
            (enqueues + 1)
            (neighbor : queueRev)
            nextDigest
        else
          scanEdgesSegment
            (edge + 1)
            end
            adjacency
            payload
            marks
            tailIndex
            enqueues
            queueRev
            nextDigest

finishDirect ::
  WorklistOutcome ->
  Int ->
  Int ->
  Int ->
  Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  TraversalState ->
  ST s WorklistOutput
finishDirect
  finalOutcome
  opens
  resumes
  updates
  growths
  offsets
  adjacency
  payload
  marks
  state
  queue
  outputLog
  traversal = do
    offsetsVector <- U.unsafeFreeze offsets
    adjacencyVector <- U.unsafeFreeze adjacency
    payloadVector <- V.unsafeFreeze payload
    marksVector <- U.unsafeFreeze marks
    stateVector <- U.unsafeFreeze state
    queueVector <-
      U.unsafeFreeze (UM.unsafeTake (stateTail traversal) queue)
    logVector <-
      U.unsafeFreeze (UM.unsafeTake (stateLogSize traversal) outputLog)
    U.length offsetsVector `seq`
      U.length adjacencyVector `seq`
        V.length payloadVector `seq`
          pure
            ( makeWorklistOutput
                finalOutcome
                opens
                resumes
                updates
                growths
                traversal
                marksVector
                stateVector
                queueVector
                logVector
            )

finishWorklistStore ::
  WorklistOutcome ->
  Int ->
  Int ->
  Int ->
  Int ->
  TraversalState ->
  WorklistStore %1 ->
  Ur WorklistOutput
{-# NOINLINE finishWorklistStore #-}
finishWorklistStore
  finalOutcome
  opens
  resumes
  updates
  growths
  traversal
  ( WorklistStore
      (WorklistFixedRoots offsetsOwner marksOwner stateOwner)
      (WorklistGraphRoots adjacencyOwner payloadOwner)
      (WorklistFrontierRoots queueOwner logOwner)
    ) =
    case Fixed.toVector offsetsOwner of
      Ur offsetsVector ->
        case Fixed.toVector marksOwner of
          Ur marksVector ->
            case Fixed.toVector stateOwner of
              Ur stateVector ->
                case Growable.toVector adjacencyOwner of
                  Ur adjacencyVector ->
                    case Growable.toVector payloadOwner of
                      Ur payloadVector ->
                        case Growable.toVector queueOwner of
                          Ur queueVector ->
                            case Growable.toVector logOwner of
                              Ur logVector ->
                                U.length offsetsVector `lseq`
                                  U.length adjacencyVector `lseq`
                                    V.length payloadVector `lseq`
                                      Ur
                                        ( makeWorklistOutput
                                            finalOutcome
                                            opens
                                            resumes
                                            updates
                                            growths
                                            traversal
                                            marksVector
                                            stateVector
                                            ( U.take
                                                (stateTail traversal)
                                                queueVector
                                            )
                                            ( U.take
                                                (stateLogSize traversal)
                                                logVector
                                            )
                                        )

makeWorklistOutput ::
  WorklistOutcome ->
  Int ->
  Int ->
  Int ->
  Int ->
  TraversalState ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Int ->
  WorklistOutput
makeWorklistOutput
  finalOutcome
  opens
  resumes
  updates
  growths
  traversal
  marksVector
  stateVector
  queueVector
  logVector =
    let !digest =
          digestOutput
            (stateDigest traversal)
            marksVector
            stateVector
            queueVector
            logVector
        !visits = stateVisits traversal
        !enqueues = stateEnqueues traversal
     in WorklistOutput
          { summary =
              WorklistSummary
                { outcome = finalOutcome
                , visitedNodes = visits
                , enqueueTransitions = enqueues
                , offsetReads = visits * 2
                , adjacencyReads = visits * worklistDegree
                , payloadReads = visits * worklistDegree
                , markReads = visits * worklistDegree
                , markWrites = enqueues
                , queueReads = visits
                , queueWrites = enqueues
                , logWrites = visits
                , contentOpens = opens
                , resumeBoundaries = resumes
                , headerUpdates = updates
                , bufferGrowths = growths
                , finalDigest = digest
                }
          , finalMarks = marksVector
          , finalState = stateVector
          , finalQueue = queueVector
          , finalLog = logVector
          }

writeTraversalState :: UM.MVector s Int -> TraversalState -> ST s ()
writeTraversalState state traversal = do
  UM.unsafeWrite state 0 (stateHead traversal)
  UM.unsafeWrite state 1 (stateTail traversal)
  UM.unsafeWrite state 2 (stateVisits traversal)
  UM.unsafeWrite state 3 (stateEnqueues traversal)
  UM.unsafeWrite state 4 (stateLogSize traversal)

newUnboxedHeaderFromVector :: U.Vector Int -> ST s (STRef s (UnboxedHeader s))
newUnboxedHeaderFromVector vector = do
  buffer <- U.thaw vector
  newSTRef (UnboxedHeader (U.length vector) buffer)

newBoxedHeaderFromVector :: V.Vector (Int, Int) -> ST s (STRef s (BoxedHeader s))
newBoxedHeaderFromVector vector = do
  buffer <- V.thaw vector
  newSTRef (BoxedHeader (V.length vector) buffer)

newUnboxedHeader :: Int -> U.Vector Int -> ST s (STRef s (UnboxedHeader s))
newUnboxedHeader requested initial = do
  let !capacity = max requested (U.length initial)
  buffer <- UM.new capacity
  U.copy (UM.unsafeTake (U.length initial) buffer) initial
  newSTRef (UnboxedHeader (U.length initial) buffer)

appendUnboxed :: STRef s (UnboxedHeader s) -> U.Vector Int -> ST s Bool
appendUnboxed header values = do
  UnboxedHeader logicalSize buffer <- readSTRef header
  let !required = logicalSize + U.length values
      !oldCapacity = UM.length buffer
      !target = growthTarget oldCapacity required
  (grown, didGrow) <-
    if target <= oldCapacity
      then pure (buffer, False)
      else do
        grown <- UM.new target
        UM.unsafeCopy
          (UM.unsafeTake logicalSize grown)
          (UM.unsafeTake logicalSize buffer)
        pure (grown, True)
  U.copy (UM.unsafeSlice logicalSize (U.length values) grown) values
  writeSTRef header (UnboxedHeader required grown)
  pure didGrow

worklistInitialCapacity :: WorklistGrowth -> Int
worklistInitialCapacity NoGrowth = worklistNodeCount
worklistInitialCapacity NoGrowthBatch64 = worklistNodeCount
worklistInitialCapacity NoGrowthBatch8 = worklistNodeCount
worklistInitialCapacity SparseGrowth = 256
worklistInitialCapacity DenseGrowth = 1

worklistBatchSize :: WorklistGrowth -> Int
worklistBatchSize NoGrowth = 256
worklistBatchSize NoGrowthBatch64 = 64
worklistBatchSize NoGrowthBatch8 = 8
worklistBatchSize SparseGrowth = 64
worklistBatchSize DenseGrowth = 8

growthTarget :: Int -> Int -> Int
growthTarget oldCapacity required
  | required <= oldCapacity = oldCapacity
  | oldCapacity <= 0 = max required 1
  | oldCapacity > maxBound `quot` 2 = required
  | otherwise = max required (oldCapacity * 2)

initialDigest :: Int64
initialDigest = 0x51A7C0DE

initialDigestForSeed :: Int -> Int64
initialDigestForSeed seed =
  initialDigest + fromIntegral seed

digestToLogValue :: Int64 -> Int -> Int
digestToLogValue digest node =
  fromIntegral
    ((digest + fromIntegral (node * 97 + 11)) `rem` 1000000007)

digestOutput ::
  Int64 ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Int ->
  Int64
digestOutput initial marks state queue outputLog =
  let !marksDigest =
        U.ifoldl'
          (\digest index value -> mixDigest digest (index * 17 + value))
          initial
          marks
      !stateDigest =
        U.ifoldl'
          (\digest index value -> mixDigest digest (index * 19 + value))
          marksDigest
          state
      !queueDigest =
        U.ifoldl'
          (\digest index value -> mixDigest digest (index * 23 + value))
          stateDigest
          queue
   in U.ifoldl'
        (\digest index value -> mixDigest digest (index * 29 + value))
        queueDigest
        outputLog

mixDigest :: Int64 -> Int -> Int64
mixDigest digest value =
  (digest * 6364136223846793005)
    + fromIntegral value
    + 1442695040888963407
