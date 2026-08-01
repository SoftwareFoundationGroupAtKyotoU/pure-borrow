{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Internal.Bench.MultiStoreScan (
  MultiStoreScanInput (..),
  MultiStoreScanOutput (..),
  MultiStoreScanResult (..),
  MultiStoreScanSummary (..),
  TraceEvent,
  multiStoreScanDirectInput,
  multiStoreScanDirectBenchmarkRoot,
  multiStoreScanDirectHeaderMatchedBenchmarkRoot,
  multiStoreScanDirectRoot,
  multiStoreScanNodeCount,
  multiStoreScanBoxedContentProjection,
  multiStoreScanPureBorrowDirectBenchmarkRoot,
  multiStoreScanPureBorrowDirectRoot,
  multiStoreScanPureBorrowFixedUnrestrictedBenchmarkRoot,
  multiStoreScanPureBorrowFixedUnrestrictedWorker,
  multiStoreScanPureBorrowNestedBenchmarkRoot,
  multiStoreScanPureBorrowNestedRoot,
  multiStoreScanPureBorrowOwningBenchmarkRoot,
  multiStoreScanPureBorrowOwningWorker,
  multiStoreScanPureBorrowWorker,
  multiStoreScanUnboxedContentProjection,
  benches,
  defaultMain,
) where

import Control.DeepSeq (NFData)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Borrows (
  Aliases (..),
  reborrowings,
 )
import Control.Monad.ST.Strict (ST, runST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.List qualified as List
import Data.Record.Linear.Borrow.Experimental.PatternMatch (
  RecordLabel,
  (.@),
 )
import Data.STRef (STRef, newSTRef, readSTRef)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Fixed
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as OwningBoxedGrowable
import Data.Vector.Mutable.Linear.Borrow qualified as OwningBoxedFixed
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as OwningUnboxedGrowable
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as OwningUnboxedFixed
import GHC.Exts qualified as GHC
import GHC.Generics (Generic)
import GHC.Int (Int64 (I64#))
import Prelude.Linear (
  lseq,
  unur,
  (&),
 )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)
import Test.Tasty.Bench qualified as Bench

data MultiStoreScanInput = MultiStoreScanInput
  { inputNext :: !(U.Vector Int)
  , inputWeight :: !(U.Vector Int)
  , inputMark :: !(U.Vector Int)
  , inputPayload :: !(V.Vector (Int, Int))
  , inputScore :: !(U.Vector Int)
  , inputLink :: !(U.Vector Int)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data MultiStoreScanSummary = MultiStoreScanSummary
  { visitedNodes :: !Int
  , elementReads :: !Int
  , elementWrites :: !Int
  , headerReads :: !Int
  , validationReads :: !Int
  , finalDigest :: !Int64
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data MultiStoreScanResult = MultiStoreScanResult
  { resultSummary :: !MultiStoreScanSummary
  , resultVisitedIndices :: !(U.Vector Int)
  , resultEvents :: !(V.Vector TraceEvent)
  , resultEventDigest :: !Int64
  , resultMarks :: !(U.Vector Int)
  , resultScores :: !(U.Vector Int)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data MultiStoreScanOutput = MultiStoreScanOutput
  { outputDigest :: !Int64
  , outputMarks :: !(U.Vector Int)
  , outputScores :: !(U.Vector Int)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TraceStore
  = NextStore
  | WeightStore
  | MarkStore
  | ScoreStore
  | LinkStore
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data TraceEvent
  = ReadIntEvent !TraceStore !Int !Int
  | ReadPayloadEvent !Int !Int !Int
  | WriteIntEvent !TraceStore !Int !Int
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data AccessTrace = AccessTrace
  { traceVisitedIndicesRev :: ![Int]
  , traceEventsRev :: ![TraceEvent]
  , traceEventDigest :: !Int64
  , traceVisitedNodes :: !Int
  , traceElementReads :: !Int
  , traceElementWrites :: !Int
  , traceHeaderReads :: !Int
  , traceReadDigest :: !Int64
  }

data FixedRoots = FixedRoots
  { next :: !(Fixed.Vector U.Vector Int)
  , weight :: !(Fixed.Vector U.Vector Int)
  , mark :: !(Fixed.Vector U.Vector Int)
  }

data GrowableRoots = GrowableRoots
  { payload :: !(Growable.GrowableVector V.Vector (Int, Int))
  , score :: !(Growable.GrowableVector U.Vector Int)
  , link :: !(Growable.GrowableVector U.Vector Int)
  }

data MultiStore = MultiStore
  { fixedRoots :: !FixedRoots
  , growableRoots :: !GrowableRoots
  }

data OwningFixedRoots = OwningFixedRoots
  { owningNext :: !(OwningUnboxedFixed.Vector Int)
  , owningWeight :: !(OwningUnboxedFixed.Vector Int)
  , owningMark :: !(OwningUnboxedFixed.Vector Int)
  }

data OwningGrowableRoots = OwningGrowableRoots
  { owningPayload :: !(OwningBoxedGrowable.GrowableVector (Int, Int))
  , owningScore :: !(OwningUnboxedGrowable.GrowableVector Int)
  , owningLink :: !(OwningUnboxedGrowable.GrowableVector Int)
  }

data OwningMultiStore = OwningMultiStore
  { owningFixedRoots :: !OwningFixedRoots
  , owningGrowableRoots :: !OwningGrowableRoots
  }

data FixedUnrestrictedStore = FixedUnrestrictedStore
  { fixedUnrestrictedRoots :: !FixedRoots
  , fixedUnrestrictedGrowableRoots :: !OwningGrowableRoots
  }

multiStoreScanBoxedContentProjection ::
  Mut α (Growable.GrowableVector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int))
{-# INLINE multiStoreScanBoxedContentProjection #-}
multiStoreScanBoxedContentProjection = Growable.getContents

multiStoreScanUnboxedContentProjection ::
  Mut α (Growable.GrowableVector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int)
{-# INLINE multiStoreScanUnboxedContentProjection #-}
multiStoreScanUnboxedContentProjection = Growable.getContents

multiStoreScanNodeCount :: Int
multiStoreScanNodeCount = 4096

multiStoreScanDirectInput :: MultiStoreScanInput
multiStoreScanDirectInput =
  MultiStoreScanInput
    { inputNext = U.generate multiStoreScanNodeCount \index -> (index + 1) `rem` multiStoreScanNodeCount
    , inputWeight = U.generate multiStoreScanNodeCount \index -> (index * 17 + 3) `rem` 101
    , inputMark = U.replicate multiStoreScanNodeCount 0
    , inputPayload =
        V.generate multiStoreScanNodeCount \index ->
          (index `rem` 7, index `rem` 13)
    , inputScore = U.generate multiStoreScanNodeCount \index -> (index * 5 + 11) `rem` 97
    , inputLink = U.replicate multiStoreScanNodeCount 0
    }

validateInput :: MultiStoreScanInput -> Int
{-# NOINLINE validateInput #-}
validateInput input
  | U.length (inputNext input)
      == multiStoreScanNodeCount
      && U.length (inputWeight input)
        == multiStoreScanNodeCount
      && U.length (inputMark input)
        == multiStoreScanNodeCount
      && V.length (inputPayload input)
        == multiStoreScanNodeCount
      && U.length (inputScore input)
        == multiStoreScanNodeCount
      && U.length (inputLink input)
        == multiStoreScanNodeCount
      && nextReads
        == multiStoreScanNodeCount
      && linkReads
        == multiStoreScanNodeCount =
      6 + nextReads + linkReads
  | otherwise =
      error
        "multi-store scan requires six 4096-element vectors, in-range next indices, and zero links"
  where
    !nextReads =
      U.foldl'
        ( \count value ->
            if value >= 0 && value < multiStoreScanNodeCount
              then count + 1
              else -multiStoreScanNodeCount
        )
        0
        (inputNext input)
    !linkReads =
      U.foldl'
        ( \count value ->
            if value == 0
              then count + 1
              else -multiStoreScanNodeCount
        )
        0
        (inputLink input)

multiStoreScanDirectRoot :: MultiStoreScanInput -> MultiStoreScanResult
{-# NOINLINE multiStoreScanDirectRoot #-}
multiStoreScanDirectRoot input =
  let !inputValidationReads = validateInput input
   in runST do
        next <- U.thaw (inputNext input)
        weight <- U.thaw (inputWeight input)
        mark <- U.thaw (inputMark input)
        payloadBuffer <- V.thaw (inputPayload input)
        scoreBuffer <- U.thaw (inputScore input)
        linkBuffer <- U.thaw (inputLink input)

        payloadHeader <- newSTRef (multiStoreScanNodeCount, payloadBuffer)
        scoreHeader <- newSTRef (multiStoreScanNodeCount, scoreBuffer)
        linkHeader <- newSTRef (multiStoreScanNodeCount, linkBuffer)
        (_, payload) <- readSTRef payloadHeader
        (_, score) <- readSTRef scoreHeader
        (_, link) <- readSTRef linkHeader

        trace <-
          multiStoreScanTraceWorker
            multiStoreScanNodeCount
            0
            0
            emptyAccessTrace {traceHeaderReads = 3}
            next
            weight
            mark
            payload
            score
            link
        frozenMarks <- U.unsafeFreeze mark
        frozenScores <- U.unsafeFreeze score
        let !digest =
              digestVectors
                (traceReadDigest trace)
                frozenMarks
                frozenScores
        pure
          MultiStoreScanResult
            { resultSummary =
                MultiStoreScanSummary
                  { visitedNodes = traceVisitedNodes trace
                  , elementReads = traceElementReads trace
                  , elementWrites = traceElementWrites trace
                  , headerReads = traceHeaderReads trace
                  , validationReads = inputValidationReads
                  , finalDigest = digest
                  }
            , resultVisitedIndices =
                U.fromListN
                  multiStoreScanNodeCount
                  (reverse (traceVisitedIndicesRev trace))
            , resultEvents =
                V.fromListN
                  (traceElementReads trace + traceElementWrites trace)
                  (reverse (traceEventsRev trace))
            , resultEventDigest = traceEventDigest trace
            , resultMarks = frozenMarks
            , resultScores = frozenScores
            }

emptyAccessTrace :: AccessTrace
emptyAccessTrace =
  AccessTrace
    { traceVisitedIndicesRev = []
    , traceEventsRev = []
    , traceEventDigest = 1_469_598_103_934_665_603
    , traceVisitedNodes = 0
    , traceElementReads = 0
    , traceElementWrites = 0
    , traceHeaderReads = 0
    , traceReadDigest = 0
    }

multiStoreScanTraceWorker ::
  Int ->
  Int ->
  Int ->
  AccessTrace ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  ST s AccessTrace
multiStoreScanTraceWorker !remaining !index !visits trace next weight mark payload score link
  | remaining <= 0 = pure trace
  | otherwise = do
      nextIndex <- UM.unsafeRead next index
      weightValue <- UM.unsafeRead weight index
      markValue <- UM.unsafeRead mark index
      (payloadTag, payloadDelta) <- MV.unsafeRead payload index
      scoreValue <- UM.unsafeRead score index
      linkValue <- UM.unsafeRead link index
      let !shouldWrite =
            (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
          !nextTrace =
            recordTraceVisit
              index
              nextIndex
              weightValue
              markValue
              payloadTag
              payloadDelta
              scoreValue
              linkValue
              shouldWrite
              trace
      if shouldWrite
        then do
          UM.unsafeWrite mark index (markValue + 1)
          UM.unsafeWrite score index (scoreValue + payloadDelta + 1)
        else pure ()
      multiStoreScanTraceWorker
        (remaining - 1)
        ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
        (visits + 1)
        nextTrace
        next
        weight
        mark
        payload
        score
        link

recordTraceVisit ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Bool ->
  AccessTrace ->
  AccessTrace
recordTraceVisit index nextIndex weightValue markValue payloadTag payloadDelta scoreValue linkValue shouldWrite trace =
  let !readEvents =
        [ ReadIntEvent NextStore index nextIndex
        , ReadIntEvent WeightStore index weightValue
        , ReadIntEvent MarkStore index markValue
        , ReadPayloadEvent index payloadTag payloadDelta
        , ReadIntEvent ScoreStore index scoreValue
        , ReadIntEvent LinkStore index linkValue
        ]
      !writeEvents =
        if shouldWrite
          then
            [ WriteIntEvent MarkStore index (markValue + 1)
            , WriteIntEvent
                ScoreStore
                index
                (scoreValue + payloadDelta + 1)
            ]
          else []
      !events = readEvents <> writeEvents
   in AccessTrace
        { traceVisitedIndicesRev =
            index : traceVisitedIndicesRev trace
        , traceEventsRev =
            List.foldl'
              (flip (:))
              (traceEventsRev trace)
              events
        , traceEventDigest =
            List.foldl' hashTraceEvent (traceEventDigest trace) events
        , traceVisitedNodes = traceVisitedNodes trace + 1
        , traceElementReads = traceElementReads trace + 6
        , traceElementWrites =
            traceElementWrites trace + if shouldWrite then 2 else 0
        , traceHeaderReads = traceHeaderReads trace
        , traceReadDigest =
            traceReadDigest trace
              + fromIntegral
                ( nextIndex
                    + weightValue
                    + markValue
                    + payloadTag
                    + payloadDelta
                    + scoreValue
                    + linkValue
                )
        }

hashTraceEvent :: Int64 -> TraceEvent -> Int64
hashTraceEvent digest event =
  List.foldl' hashTraceWord digest case event of
    ReadIntEvent store index value ->
      [1, traceStoreCode store, index, value]
    ReadPayloadEvent index tag delta ->
      [2, index, tag, delta]
    WriteIntEvent store index value ->
      [3, traceStoreCode store, index, value]

hashTraceWord :: Int64 -> Int -> Int64
hashTraceWord digest value =
  digest * 1_099_511_628_211 + fromIntegral value

traceStoreCode :: TraceStore -> Int
traceStoreCode = \case
  NextStore -> 1
  WeightStore -> 2
  MarkStore -> 3
  ScoreStore -> 4
  LinkStore -> 5

multiStoreScanDirectBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanDirectBenchmarkRoot #-}
multiStoreScanDirectBenchmarkRoot input =
  validateInput input `seq` runST do
    next <- U.thaw (inputNext input)
    weight <- U.thaw (inputWeight input)
    mark <- U.thaw (inputMark input)
    payloadBuffer <- V.thaw (inputPayload input)
    scoreBuffer <- U.thaw (inputScore input)
    linkBuffer <- U.thaw (inputLink input)

    payloadHeader <- newSTRef (multiStoreScanNodeCount, payloadBuffer)
    scoreHeader <- newSTRef (multiStoreScanNodeCount, scoreBuffer)
    linkHeader <- newSTRef (multiStoreScanNodeCount, linkBuffer)
    (_, payload) <- readSTRef payloadHeader
    (_, score) <- readSTRef scoreHeader
    (_, link) <- readSTRef linkHeader

    readDigest <-
      multiStoreScanDirectWorker
        multiStoreScanNodeCount
        0
        0
        0
        next
        weight
        mark
        payload
        score
        link
    frozenMarks <- U.unsafeFreeze mark
    frozenScores <- U.unsafeFreeze score
    pure
      MultiStoreScanOutput
        { outputDigest =
            digestVectors readDigest frozenMarks frozenScores
        , outputMarks = frozenMarks
        , outputScores = frozenScores
        }

multiStoreScanDirectHeaderMatchedBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanDirectHeaderMatchedBenchmarkRoot #-}
multiStoreScanDirectHeaderMatchedBenchmarkRoot input =
  validateInput input `seq` runST do
    next <- U.thaw (inputNext input)
    weight <- U.thaw (inputWeight input)
    mark <- U.thaw (inputMark input)
    payloadBuffer <- V.thaw (inputPayload input)
    scoreBuffer <- U.thaw (inputScore input)
    linkBuffer <- U.thaw (inputLink input)

    payloadHeader <- newSTRef (multiStoreScanNodeCount, payloadBuffer)
    scoreHeader <- newSTRef (multiStoreScanNodeCount, scoreBuffer)
    linkHeader <- newSTRef (multiStoreScanNodeCount, linkBuffer)
    payload <- readHeaderOpaque payloadHeader
    score <- readHeaderOpaque scoreHeader
    link <- readHeaderOpaque linkHeader

    readDigest <-
      multiStoreScanDirectWorker
        multiStoreScanNodeCount
        0
        0
        0
        next
        weight
        mark
        payload
        score
        link
    frozenMarks <- U.unsafeFreeze mark
    frozenScores <- U.unsafeFreeze score
    pure
      MultiStoreScanOutput
        { outputDigest =
            digestVectors readDigest frozenMarks frozenScores
        , outputMarks = frozenMarks
        , outputScores = frozenScores
        }

readHeaderOpaque :: STRef s (Int, vector) -> ST s vector
{-# NOINLINE readHeaderOpaque #-}
-- Keep the comparator's three header reads observable. If this helper inlines,
-- GHC can cancel each locally allocated STRef against its read and turn the
-- control back into the deliberately retained lower-bound root.
readHeaderOpaque header = do
  (_, vector) <- readSTRef header
  pure vector

multiStoreScanDirectWorker ::
  Int ->
  Int ->
  Int ->
  Int64 ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  MV.MVector s (Int, Int) ->
  UM.MVector s Int ->
  UM.MVector s Int ->
  ST s Int64
{-# NOINLINE multiStoreScanDirectWorker #-}
multiStoreScanDirectWorker !remaining !index !visits !digest next weight mark payload score link
  | remaining <= 0 = pure digest
  | otherwise = do
      nextIndex <- UM.unsafeRead next index
      weightValue <- UM.unsafeRead weight index
      markValue <- UM.unsafeRead mark index
      (payloadTag, payloadDelta) <- MV.unsafeRead payload index
      scoreValue <- UM.unsafeRead score index
      linkValue <- UM.unsafeRead link index
      let !shouldWrite =
            (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
          !nextDigest =
            digest
              + fromIntegral
                ( nextIndex
                    + weightValue
                    + markValue
                    + payloadTag
                    + payloadDelta
                    + scoreValue
                    + linkValue
                )
      if shouldWrite
        then do
          UM.unsafeWrite mark index (markValue + 1)
          UM.unsafeWrite score index (scoreValue + payloadDelta + 1)
        else pure ()
      multiStoreScanDirectWorker
        (remaining - 1)
        ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
        (visits + 1)
        nextDigest
        next
        weight
        mark
        payload
        score
        link

multiStoreScanPureBorrowOwningBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanPureBorrowOwningBenchmarkRoot #-}
multiStoreScanPureBorrowOwningBenchmarkRoot input =
  validateInput input `seq`
    unur
      ( linearly \linear -> DataFlow.do
          (allocationLinear, borrowLinear) <- dup linear
          store <- newOwningMultiStore input allocationLinear
          runBO borrowLinear Control.do
            (storeBorrow, lender) <- borrowM store
            (Ur digest, storeBorrow) <-
              reborrowing storeBorrow \local -> Control.do
                let %1 !(fixedRootBorrows, growableRootBorrows) =
                      local
                        .@ (owningFixedRootsField, owningGrowableRootsField)
                let %1 !(nextBorrow, weightBorrow, markBorrow) =
                      fixedRootBorrows
                        .@ (owningNextField, owningWeightField, owningMarkField)
                let %1 !(payloadBorrow, scoreBorrow, linkBorrow) =
                      growableRootBorrows
                        .@ (owningPayloadField, owningScoreField, owningLinkField)
                let %1 !payloadContent =
                      OwningBoxedGrowable.getContents payloadBorrow
                let %1 !scoreContent =
                      OwningUnboxedGrowable.getContents scoreBorrow
                let %1 !linkContent =
                      OwningUnboxedGrowable.getContents linkBorrow
                ( Ur digest
                  , nextBorrow
                  , weightBorrow
                  , markBorrow
                  , payloadContent
                  , scoreContent
                  , linkContent
                  ) <-
                  multiStoreScanPureBorrowOwningWorker
                    multiStoreScanNodeCount
                    0
                    0
                    0
                    nextBorrow
                    weightBorrow
                    markBorrow
                    payloadContent
                    scoreContent
                    linkContent
                let !(Ur _) = share nextBorrow
                let !(Ur _) = share weightBorrow
                let !(Ur _) = share markBorrow
                let !(Ur _) = share payloadContent
                let !(Ur _) = share scoreContent
                let !(Ur _) = share linkContent
                Control.pure (Ur digest)
            let !(Ur _) = share storeBorrow
            pureAfter
              (finishOwningMultiStoreOutput digest (reclaim lender))
      )

multiStoreScanPureBorrowFixedUnrestrictedBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanPureBorrowFixedUnrestrictedBenchmarkRoot #-}
multiStoreScanPureBorrowFixedUnrestrictedBenchmarkRoot input =
  validateInput input `seq`
    unur
      ( linearly \linear -> DataFlow.do
          (allocationLinear, borrowLinear) <- dup linear
          store <- newFixedUnrestrictedStore input allocationLinear
          runBO borrowLinear Control.do
            (storeBorrow, lender) <- borrowM store
            (Ur digest, storeBorrow) <-
              reborrowing storeBorrow \local -> Control.do
                let %1 !(fixedRootBorrows, growableRootBorrows) =
                      local
                        .@ ( fixedUnrestrictedRootsField
                           , fixedUnrestrictedGrowableRootsField
                           )
                let %1 !(nextBorrow, weightBorrow, markBorrow) =
                      fixedRootBorrows .@ (nextField, weightField, markField)
                let %1 !(payloadBorrow, scoreBorrow, linkBorrow) =
                      growableRootBorrows
                        .@ (owningPayloadField, owningScoreField, owningLinkField)
                let %1 !payloadContent =
                      OwningBoxedGrowable.getContents payloadBorrow
                let %1 !scoreContent =
                      OwningUnboxedGrowable.getContents scoreBorrow
                let %1 !linkContent =
                      OwningUnboxedGrowable.getContents linkBorrow
                ( Ur digest
                  , nextBorrow
                  , weightBorrow
                  , markBorrow
                  , payloadContent
                  , scoreContent
                  , linkContent
                  ) <-
                  multiStoreScanPureBorrowFixedUnrestrictedWorker
                    multiStoreScanNodeCount
                    0
                    0
                    0
                    nextBorrow
                    weightBorrow
                    markBorrow
                    payloadContent
                    scoreContent
                    linkContent
                let !(Ur _) = share nextBorrow
                let !(Ur _) = share weightBorrow
                let !(Ur _) = share markBorrow
                let !(Ur _) = share payloadContent
                let !(Ur _) = share scoreContent
                let !(Ur _) = share linkContent
                Control.pure (Ur digest)
            let !(Ur _) = share storeBorrow
            pureAfter
              ( finishFixedUnrestrictedStoreOutput
                  digest
                  (reclaim lender)
              )
      )

multiStoreScanPureBorrowOwningWorker ::
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  Mut α (OwningBoxedFixed.Vector (Int, Int)) %1 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  BO
    α
    ( Ur Int64
    , Mut α (OwningUnboxedFixed.Vector Int)
    , Mut α (OwningUnboxedFixed.Vector Int)
    , Mut α (OwningUnboxedFixed.Vector Int)
    , Mut α (OwningBoxedFixed.Vector (Int, Int))
    , Mut α (OwningUnboxedFixed.Vector Int)
    , Mut α (OwningUnboxedFixed.Vector Int)
    )
{-# NOINLINE multiStoreScanPureBorrowOwningWorker #-}
multiStoreScanPureBorrowOwningWorker !remaining !index !visits !digest nextBorrow weightBorrow markBorrow payloadContent scoreContent linkContent
  | remaining <= 0 =
      Control.pure
        ( Ur digest
        , nextBorrow
        , weightBorrow
        , markBorrow
        , payloadContent
        , scoreContent
        , linkContent
        )
  | otherwise = Control.do
      (Ur nextIndex, nextBorrow) <-
        OwningUnboxedFixed.copyAtMut index nextBorrow
      (Ur weightValue, weightBorrow) <-
        OwningUnboxedFixed.copyAtMut index weightBorrow
      (Ur markValue, markBorrow) <-
        OwningUnboxedFixed.copyAtMut index markBorrow
      (Ur (payloadTag, payloadDelta), payloadContent) <-
        OwningBoxedFixed.copyAtMut index payloadContent
      (Ur scoreValue, scoreContent) <-
        OwningUnboxedFixed.copyAtMut index scoreContent
      (Ur linkValue, linkContent) <-
        OwningUnboxedFixed.copyAtMut index linkContent
      let !shouldWrite =
            (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
          !nextDigest =
            digest
              + fromIntegral
                ( nextIndex
                    + weightValue
                    + markValue
                    + payloadTag
                    + payloadDelta
                    + scoreValue
                    + linkValue
                )
      if shouldWrite
        then Control.do
          (oldMark, markBorrow) <-
            OwningUnboxedFixed.unsafeSet
              index
              (markValue + 1)
              markBorrow
          (oldScore, scoreContent) <-
            OwningUnboxedFixed.unsafeSet
              index
              (scoreValue + payloadDelta + 1)
              scoreContent
          multiStoreScanPureBorrowOwningWorker
            (remaining - 1)
            ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
            (visits + 1)
            nextDigest
            nextBorrow
            weightBorrow
            (consume oldMark `lseq` markBorrow)
            payloadContent
            (consume oldScore `lseq` scoreContent)
            linkContent
        else
          multiStoreScanPureBorrowOwningWorker
            (remaining - 1)
            ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
            (visits + 1)
            nextDigest
            nextBorrow
            weightBorrow
            markBorrow
            payloadContent
            scoreContent
            linkContent

multiStoreScanPureBorrowFixedUnrestrictedWorker ::
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (OwningBoxedFixed.Vector (Int, Int)) %1 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  Mut α (OwningUnboxedFixed.Vector Int) %1 ->
  BO
    α
    ( Ur Int64
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (OwningBoxedFixed.Vector (Int, Int))
    , Mut α (OwningUnboxedFixed.Vector Int)
    , Mut α (OwningUnboxedFixed.Vector Int)
    )
{-# NOINLINE multiStoreScanPureBorrowFixedUnrestrictedWorker #-}
multiStoreScanPureBorrowFixedUnrestrictedWorker !remaining !index !visits !digest nextBorrow weightBorrow markBorrow payloadContent scoreContent linkContent
  | remaining <= 0 =
      Control.pure
        ( Ur digest
        , nextBorrow
        , weightBorrow
        , markBorrow
        , payloadContent
        , scoreContent
        , linkContent
        )
  | otherwise = Control.do
      (Ur nextIndex, nextBorrow) <-
        Fixed.unsafeGet index nextBorrow
      (Ur weightValue, weightBorrow) <-
        Fixed.unsafeGet index weightBorrow
      (Ur markValue, markBorrow) <-
        Fixed.unsafeGet index markBorrow
      (Ur (payloadTag, payloadDelta), payloadContent) <-
        OwningBoxedFixed.copyAtMut index payloadContent
      (Ur scoreValue, scoreContent) <-
        OwningUnboxedFixed.copyAtMut index scoreContent
      (Ur linkValue, linkContent) <-
        OwningUnboxedFixed.copyAtMut index linkContent
      let !shouldWrite =
            (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
          !nextDigest =
            digest
              + fromIntegral
                ( nextIndex
                    + weightValue
                    + markValue
                    + payloadTag
                    + payloadDelta
                    + scoreValue
                    + linkValue
                )
      if shouldWrite
        then Control.do
          markBorrow <-
            Fixed.unsafeWrite index (markValue + 1) markBorrow
          (oldScore, scoreContent) <-
            OwningUnboxedFixed.unsafeSet
              index
              (scoreValue + payloadDelta + 1)
              scoreContent
          multiStoreScanPureBorrowFixedUnrestrictedWorker
            (remaining - 1)
            ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
            (visits + 1)
            nextDigest
            nextBorrow
            weightBorrow
            markBorrow
            payloadContent
            (consume oldScore `lseq` scoreContent)
            linkContent
        else
          multiStoreScanPureBorrowFixedUnrestrictedWorker
            (remaining - 1)
            ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
            (visits + 1)
            nextDigest
            nextBorrow
            weightBorrow
            markBorrow
            payloadContent
            scoreContent
            linkContent

multiStoreScanPureBorrowDirectBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanPureBorrowDirectBenchmarkRoot #-}
multiStoreScanPureBorrowDirectBenchmarkRoot input =
  validateInput input `seq`
    unur
      ( linearly \linear -> DataFlow.do
          (allocationLinear, borrowLinear) <- dup linear
          store <- newMultiStore input allocationLinear
          runBO borrowLinear Control.do
            (storeBorrow, lender) <- borrowM store
            (Ur digest, storeBorrow) <-
              reborrowing storeBorrow \local -> Control.do
                let %1 !(fixedRootBorrows, growableRootBorrows) =
                      local .@ (fixedRootsField, growableRootsField)
                let %1 !(nextBorrow, weightBorrow, markBorrow) =
                      fixedRootBorrows .@ (nextField, weightField, markField)
                let %1 !(payloadBorrow, scoreBorrow, linkBorrow) =
                      growableRootBorrows .@ (payloadField, scoreField, linkField)
                let %1 !payloadContent = Growable.getContents payloadBorrow
                let %1 !scoreContent = Growable.getContents scoreBorrow
                let %1 !linkContent = Growable.getContents linkBorrow
                ( Ur digest
                  , nextBorrow
                  , weightBorrow
                  , markBorrow
                  , payloadContent
                  , scoreContent
                  , linkContent
                  ) <-
                  multiStoreScanPureBorrowWorker
                    multiStoreScanNodeCount
                    0
                    0
                    0
                    nextBorrow
                    weightBorrow
                    markBorrow
                    payloadContent
                    scoreContent
                    linkContent
                let !() =
                      consumeViews
                        nextBorrow
                        weightBorrow
                        markBorrow
                        payloadContent
                        scoreContent
                        linkContent
                Control.pure (Ur digest)
            let !(Ur _) = share storeBorrow
            pureAfter (finishMultiStoreOutput digest (reclaim lender))
      )

multiStoreScanPureBorrowNestedBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanPureBorrowNestedBenchmarkRoot #-}
multiStoreScanPureBorrowNestedBenchmarkRoot input =
  validateInput input `seq`
    unur
      ( linearly \linear -> DataFlow.do
          (allocationLinear, borrowLinear) <- dup linear
          store <- newMultiStore input allocationLinear
          runBO borrowLinear Control.do
            (storeBorrow, lender) <- borrowM store
            (Ur digest, storeBorrow) <-
              reborrowing storeBorrow \local -> Control.do
                let %1 !(fixedRootBorrows, growableRootBorrows) =
                      local .@ (fixedRootsField, growableRootsField)
                let %1 !(nextBorrow, weightBorrow, markBorrow) =
                      fixedRootBorrows .@ (nextField, weightField, markField)
                let %1 !(payloadBorrow, scoreBorrow, linkBorrow) =
                      growableRootBorrows .@ (payloadField, scoreField, linkField)
                (Ur digest, fields) <-
                  reborrowings
                    ( nextBorrow
                        :- weightBorrow
                        :- markBorrow
                        :- payloadBorrow
                        :- scoreBorrow
                        :- linkBorrow
                        :- BNil
                    )
                    \case
                      nextBorrow
                        :- weightBorrow
                        :- markBorrow
                        :- payloadBorrow
                        :- scoreBorrow
                        :- linkBorrow
                        :- BNil -> Control.do
                          let %1 !payloadContent =
                                Growable.getContents payloadBorrow
                          let %1 !scoreContent =
                                Growable.getContents scoreBorrow
                          let %1 !linkContent =
                                Growable.getContents linkBorrow
                          ( Ur digest
                            , nextBorrow
                            , weightBorrow
                            , markBorrow
                            , payloadContent
                            , scoreContent
                            , linkContent
                            ) <-
                            multiStoreScanPureBorrowWorker
                              multiStoreScanNodeCount
                              0
                              0
                              0
                              nextBorrow
                              weightBorrow
                              markBorrow
                              payloadContent
                              scoreContent
                              linkContent
                          let !() =
                                consumeViews
                                  nextBorrow
                                  weightBorrow
                                  markBorrow
                                  payloadContent
                                  scoreContent
                                  linkContent
                          Control.pure (Ur digest)
                let !() = consume fields
                Control.pure (Ur digest)
            let !(Ur _) = share storeBorrow
            pureAfter (finishMultiStoreOutput digest (reclaim lender))
      )

multiStoreScanPureBorrowDirectRoot ::
  MultiStoreScanInput ->
  MultiStoreScanResult
{-# NOINLINE multiStoreScanPureBorrowDirectRoot #-}
multiStoreScanPureBorrowDirectRoot input =
  let !inputValidationReads = validateInput input
   in unur
        ( linearly \linear -> DataFlow.do
            (allocationLinear, borrowLinear) <- dup linear
            store <- newMultiStore input allocationLinear
            runBO borrowLinear Control.do
              (storeBorrow, lender) <- borrowM store
              (Ur trace, storeBorrow) <-
                reborrowing storeBorrow \local -> Control.do
                  let %1 !(fixedRootBorrows, growableRootBorrows) =
                        local .@ (fixedRootsField, growableRootsField)
                  let %1 !(nextBorrow, weightBorrow, markBorrow) =
                        fixedRootBorrows .@ (nextField, weightField, markField)
                  let %1 !(payloadBorrow, scoreBorrow, linkBorrow) =
                        growableRootBorrows .@ (payloadField, scoreField, linkField)
                  let %1 !payloadContent = Growable.getContents payloadBorrow
                  let %1 !scoreContent = Growable.getContents scoreBorrow
                  let %1 !linkContent = Growable.getContents linkBorrow
                  ( Ur trace
                    , nextBorrow
                    , weightBorrow
                    , markBorrow
                    , payloadContent
                    , scoreContent
                    , linkContent
                    ) <-
                    multiStoreScanPureBorrowTraceWorker
                      multiStoreScanNodeCount
                      0
                      0
                      emptyAccessTrace {traceHeaderReads = 3}
                      nextBorrow
                      weightBorrow
                      markBorrow
                      payloadContent
                      scoreContent
                      linkContent
                  let !() =
                        consumeViews
                          nextBorrow
                          weightBorrow
                          markBorrow
                          payloadContent
                          scoreContent
                          linkContent
                  Control.pure (Ur trace)
              let !(Ur _) = share storeBorrow
              pureAfter
                ( finishMultiStoreResult
                    inputValidationReads
                    trace
                    (reclaim lender)
                )
        )

multiStoreScanPureBorrowNestedRoot ::
  MultiStoreScanInput ->
  MultiStoreScanResult
{-# NOINLINE multiStoreScanPureBorrowNestedRoot #-}
multiStoreScanPureBorrowNestedRoot input =
  let !inputValidationReads = validateInput input
   in unur
        ( linearly \linear -> DataFlow.do
            (allocationLinear, borrowLinear) <- dup linear
            store <- newMultiStore input allocationLinear
            runBO borrowLinear Control.do
              (storeBorrow, lender) <- borrowM store
              (Ur trace, storeBorrow) <-
                reborrowing storeBorrow \local -> Control.do
                  let %1 !(fixedRootBorrows, growableRootBorrows) =
                        local .@ (fixedRootsField, growableRootsField)
                  let %1 !(nextBorrow, weightBorrow, markBorrow) =
                        fixedRootBorrows .@ (nextField, weightField, markField)
                  let %1 !(payloadBorrow, scoreBorrow, linkBorrow) =
                        growableRootBorrows .@ (payloadField, scoreField, linkField)
                  (Ur trace, fields) <-
                    reborrowings
                      ( nextBorrow
                          :- weightBorrow
                          :- markBorrow
                          :- payloadBorrow
                          :- scoreBorrow
                          :- linkBorrow
                          :- BNil
                      )
                      \case
                        nextBorrow
                          :- weightBorrow
                          :- markBorrow
                          :- payloadBorrow
                          :- scoreBorrow
                          :- linkBorrow
                          :- BNil -> Control.do
                            let %1 !payloadContent =
                                  Growable.getContents payloadBorrow
                            let %1 !scoreContent =
                                  Growable.getContents scoreBorrow
                            let %1 !linkContent =
                                  Growable.getContents linkBorrow
                            ( Ur trace
                              , nextBorrow
                              , weightBorrow
                              , markBorrow
                              , payloadContent
                              , scoreContent
                              , linkContent
                              ) <-
                              multiStoreScanPureBorrowTraceWorker
                                multiStoreScanNodeCount
                                0
                                0
                                emptyAccessTrace {traceHeaderReads = 3}
                                nextBorrow
                                weightBorrow
                                markBorrow
                                payloadContent
                                scoreContent
                                linkContent
                            let !() =
                                  consumeViews
                                    nextBorrow
                                    weightBorrow
                                    markBorrow
                                    payloadContent
                                    scoreContent
                                    linkContent
                            Control.pure (Ur trace)
                  let !() = consume fields
                  Control.pure (Ur trace)
              let !(Ur _) = share storeBorrow
              pureAfter
                ( finishMultiStoreResult
                    inputValidationReads
                    trace
                    (reclaim lender)
                )
        )

multiStoreScanPureBorrowWorker ::
  Int ->
  Int ->
  Int ->
  Int64 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur Int64
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# INLINEABLE multiStoreScanPureBorrowWorker #-}
multiStoreScanPureBorrowWorker !remaining !index !visits !digest nextBorrow weightBorrow markBorrow payloadContent scoreContent linkContent =
  -- An ordinary strict Int64 accumulator is still rebuilt as I64# at every
  -- recursive call because the BO result also returns six linear borrows.
  -- Entering an explicitly unboxed local loop keeps the accumulator in
  -- Int64# until the single ownership boundary below.
  case digest of
    I64# digest# ->
      go
        remaining
        index
        visits
        digest#
        nextBorrow
        weightBorrow
        markBorrow
        payloadContent
        scoreContent
        linkContent
  where
    go ::
      Int ->
      Int ->
      Int ->
      GHC.Int64# ->
      Mut α (Fixed.Vector U.Vector Int) %1 ->
      Mut α (Fixed.Vector U.Vector Int) %1 ->
      Mut α (Fixed.Vector U.Vector Int) %1 ->
      Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
      Mut α (Fixed.Vector U.Vector Int) %1 ->
      Mut α (Fixed.Vector U.Vector Int) %1 ->
      BO
        α
        ( Ur Int64
        , Mut α (Fixed.Vector U.Vector Int)
        , Mut α (Fixed.Vector U.Vector Int)
        , Mut α (Fixed.Vector U.Vector Int)
        , Mut α (Fixed.Vector V.Vector (Int, Int))
        , Mut α (Fixed.Vector U.Vector Int)
        , Mut α (Fixed.Vector U.Vector Int)
        )
    go !remaining !index !visits digest# nextBorrow weightBorrow markBorrow payloadContent scoreContent linkContent
      | remaining <= 0 =
          Control.pure
            ( Ur (I64# digest#)
            , nextBorrow
            , weightBorrow
            , markBorrow
            , payloadContent
            , scoreContent
            , linkContent
            )
      | otherwise = Control.do
          (Ur nextIndex, nextBorrow) <-
            Fixed.unsafeGet index nextBorrow
          (Ur weightValue, weightBorrow) <-
            Fixed.unsafeGet index weightBorrow
          (Ur markValue, markBorrow) <-
            Fixed.unsafeGet index markBorrow
          (Ur (payloadTag, payloadDelta), payloadContent) <-
            Fixed.unsafeGet index payloadContent
          (Ur scoreValue, scoreContent) <-
            Fixed.unsafeGet index scoreContent
          (Ur linkValue, linkContent) <-
            Fixed.unsafeGet index linkContent
          let !shouldWrite =
                (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
              !nextDigest =
                I64# digest#
                  + fromIntegral
                    ( nextIndex
                        + weightValue
                        + markValue
                        + payloadTag
                        + payloadDelta
                        + scoreValue
                        + linkValue
                    )
          case nextDigest of
            I64# nextDigest# ->
              if shouldWrite
                then Control.do
                  markBorrow <-
                    Fixed.unsafeWrite index (markValue + 1) markBorrow
                  scoreContent <-
                    Fixed.unsafeWrite
                      index
                      (scoreValue + payloadDelta + 1)
                      scoreContent
                  go
                    (remaining - 1)
                    ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
                    (visits + 1)
                    nextDigest#
                    nextBorrow
                    weightBorrow
                    markBorrow
                    payloadContent
                    scoreContent
                    linkContent
                else
                  go
                    (remaining - 1)
                    ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
                    (visits + 1)
                    nextDigest#
                    nextBorrow
                    weightBorrow
                    markBorrow
                    payloadContent
                    scoreContent
                    linkContent

multiStoreScanPureBorrowTraceWorker ::
  Int ->
  Int ->
  Int ->
  AccessTrace ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  BO
    α
    ( Ur AccessTrace
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector V.Vector (Int, Int))
    , Mut α (Fixed.Vector U.Vector Int)
    , Mut α (Fixed.Vector U.Vector Int)
    )
{-# NOINLINE multiStoreScanPureBorrowTraceWorker #-}
multiStoreScanPureBorrowTraceWorker !remaining !index !visits !trace nextBorrow weightBorrow markBorrow payloadContent scoreContent linkContent
  | remaining <= 0 =
      Control.pure
        ( Ur trace
        , nextBorrow
        , weightBorrow
        , markBorrow
        , payloadContent
        , scoreContent
        , linkContent
        )
  | otherwise = Control.do
      (Ur nextIndex, nextBorrow) <-
        Fixed.unsafeGet index nextBorrow
      (Ur weightValue, weightBorrow) <-
        Fixed.unsafeGet index weightBorrow
      (Ur markValue, markBorrow) <-
        Fixed.unsafeGet index markBorrow
      (Ur (payloadTag, payloadDelta), payloadContent) <-
        Fixed.unsafeGet index payloadContent
      (Ur scoreValue, scoreContent) <-
        Fixed.unsafeGet index scoreContent
      (Ur linkValue, linkContent) <-
        Fixed.unsafeGet index linkContent
      let !shouldWrite =
            (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
          !nextTrace =
            recordTraceVisit
              index
              nextIndex
              weightValue
              markValue
              payloadTag
              payloadDelta
              scoreValue
              linkValue
              shouldWrite
              trace
      if shouldWrite
        then Control.do
          markBorrow <-
            Fixed.unsafeWrite index (markValue + 1) markBorrow
          scoreContent <-
            Fixed.unsafeWrite
              index
              (scoreValue + payloadDelta + 1)
              scoreContent
          multiStoreScanPureBorrowTraceWorker
            (remaining - 1)
            ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
            (visits + 1)
            nextTrace
            nextBorrow
            weightBorrow
            markBorrow
            payloadContent
            scoreContent
            linkContent
        else
          multiStoreScanPureBorrowTraceWorker
            (remaining - 1)
            ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
            (visits + 1)
            nextTrace
            nextBorrow
            weightBorrow
            markBorrow
            payloadContent
            scoreContent
            linkContent

consumeViews ::
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector V.Vector (Int, Int)) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  Mut α (Fixed.Vector U.Vector Int) %1 ->
  ()
consumeViews nextBorrow weightBorrow markBorrow payloadContent scoreContent linkContent =
  let !(Ur _) = share nextBorrow
      !(Ur _) = share weightBorrow
      !(Ur _) = share markBorrow
      !(Ur _) = share payloadContent
      !(Ur _) = share scoreContent
      !(Ur _) = share linkContent
   in ()

newOwningMultiStore ::
  MultiStoreScanInput ->
  Linearly %1 ->
  OwningMultiStore
{-# NOINLINE newOwningMultiStore #-}
newOwningMultiStore =
  GHC.noinline \input linear ->
    dup linear & \(nextLinear, rest1) ->
      dup rest1 & \(weightLinear, rest2) ->
        dup rest2 & \(markLinear, rest3) ->
          dup rest3 & \(payloadLinear, rest4) ->
            dup rest4 & \(scoreLinear, linkLinear) ->
              OwningMultiStore
                { owningFixedRoots =
                    OwningFixedRoots
                      { owningNext =
                          OwningUnboxedFixed.fromVector
                            (inputNext input)
                            nextLinear
                      , owningWeight =
                          OwningUnboxedFixed.fromVector
                            (inputWeight input)
                            weightLinear
                      , owningMark =
                          OwningUnboxedFixed.fromVector
                            (inputMark input)
                            markLinear
                      }
                , owningGrowableRoots =
                    OwningGrowableRoots
                      { owningPayload =
                          OwningBoxedGrowable.fromVector
                            (inputPayload input)
                            payloadLinear
                      , owningScore =
                          OwningUnboxedGrowable.fromVector
                            (inputScore input)
                            scoreLinear
                      , owningLink =
                          OwningUnboxedGrowable.fromVector
                            (inputLink input)
                            linkLinear
                      }
                }

newFixedUnrestrictedStore ::
  MultiStoreScanInput ->
  Linearly %1 ->
  FixedUnrestrictedStore
{-# NOINLINE newFixedUnrestrictedStore #-}
newFixedUnrestrictedStore =
  GHC.noinline \input linear ->
    dup linear & \(nextLinear, rest1) ->
      dup rest1 & \(weightLinear, rest2) ->
        dup rest2 & \(markLinear, rest3) ->
          dup rest3 & \(payloadLinear, rest4) ->
            dup rest4 & \(scoreLinear, linkLinear) ->
              FixedUnrestrictedStore
                { fixedUnrestrictedRoots =
                    FixedRoots
                      { next =
                          Fixed.fromVector
                            (inputNext input)
                            nextLinear
                      , weight =
                          Fixed.fromVector
                            (inputWeight input)
                            weightLinear
                      , mark =
                          Fixed.fromVector
                            (inputMark input)
                            markLinear
                      }
                , fixedUnrestrictedGrowableRoots =
                    OwningGrowableRoots
                      { owningPayload =
                          OwningBoxedGrowable.fromVector
                            (inputPayload input)
                            payloadLinear
                      , owningScore =
                          OwningUnboxedGrowable.fromVector
                            (inputScore input)
                            scoreLinear
                      , owningLink =
                          OwningUnboxedGrowable.fromVector
                            (inputLink input)
                            linkLinear
                      }
                }

newMultiStore :: MultiStoreScanInput -> Linearly %1 -> MultiStore
{-# NOINLINE newMultiStore #-}
newMultiStore =
  GHC.noinline \input linear ->
    dup linear & \(nextLinear, rest1) ->
      dup rest1 & \(weightLinear, rest2) ->
        dup rest2 & \(markLinear, rest3) ->
          dup rest3 & \(payloadLinear, rest4) ->
            dup rest4 & \(scoreLinear, linkLinear) ->
              MultiStore
                { fixedRoots =
                    FixedRoots
                      { next =
                          Fixed.fromVector
                            (inputNext input)
                            nextLinear
                      , weight =
                          Fixed.fromVector
                            (inputWeight input)
                            weightLinear
                      , mark =
                          Fixed.fromVector
                            (inputMark input)
                            markLinear
                      }
                , growableRoots =
                    GrowableRoots
                      { payload =
                          Growable.fromVector
                            (inputPayload input)
                            payloadLinear
                      , score =
                          Growable.fromVector
                            (inputScore input)
                            scoreLinear
                      , link =
                          Growable.fromVector
                            (inputLink input)
                            linkLinear
                      }
                }

finishOwningMultiStoreOutput ::
  Int64 ->
  OwningMultiStore %1 ->
  Ur MultiStoreScanOutput
{-# NOINLINE finishOwningMultiStoreOutput #-}
finishOwningMultiStoreOutput
  digest
  ( OwningMultiStore
      (OwningFixedRoots nextOwner weightOwner markOwner)
      (OwningGrowableRoots payloadOwner scoreOwner linkOwner)
    ) =
    case OwningUnboxedFixed.toVector nextOwner of
      Ur nextVector ->
        case OwningUnboxedFixed.toVector weightOwner of
          Ur weightVector ->
            case OwningUnboxedFixed.toVector markOwner of
              Ur markVector ->
                case OwningBoxedGrowable.toVector payloadOwner of
                  Ur payloadVector ->
                    case OwningUnboxedGrowable.toVector scoreOwner of
                      Ur scoreVector ->
                        case OwningUnboxedGrowable.toVector linkOwner of
                          Ur linkVector ->
                            U.length nextVector `lseq`
                              U.length weightVector `lseq`
                                V.length payloadVector `lseq`
                                  U.length linkVector `lseq`
                                    Ur
                                      MultiStoreScanOutput
                                        { outputDigest =
                                            digestVectors
                                              digest
                                              markVector
                                              scoreVector
                                        , outputMarks = markVector
                                        , outputScores = scoreVector
                                        }

finishFixedUnrestrictedStoreOutput ::
  Int64 ->
  FixedUnrestrictedStore %1 ->
  Ur MultiStoreScanOutput
{-# NOINLINE finishFixedUnrestrictedStoreOutput #-}
finishFixedUnrestrictedStoreOutput
  digest
  ( FixedUnrestrictedStore
      (FixedRoots nextOwner weightOwner markOwner)
      (OwningGrowableRoots payloadOwner scoreOwner linkOwner)
    ) =
    case Fixed.toVector nextOwner of
      Ur nextVector ->
        case Fixed.toVector weightOwner of
          Ur weightVector ->
            case Fixed.toVector markOwner of
              Ur markVector ->
                case OwningBoxedGrowable.toVector payloadOwner of
                  Ur payloadVector ->
                    case OwningUnboxedGrowable.toVector scoreOwner of
                      Ur scoreVector ->
                        case OwningUnboxedGrowable.toVector linkOwner of
                          Ur linkVector ->
                            U.length nextVector `lseq`
                              U.length weightVector `lseq`
                                V.length payloadVector `lseq`
                                  U.length linkVector `lseq`
                                    Ur
                                      MultiStoreScanOutput
                                        { outputDigest =
                                            digestVectors
                                              digest
                                              markVector
                                              scoreVector
                                        , outputMarks = markVector
                                        , outputScores = scoreVector
                                        }

finishMultiStoreOutput ::
  Int64 ->
  MultiStore %1 ->
  Ur MultiStoreScanOutput
{-# NOINLINE finishMultiStoreOutput #-}
finishMultiStoreOutput
  digest
  ( MultiStore
      (FixedRoots nextOwner weightOwner markOwner)
      (GrowableRoots payloadOwner scoreOwner linkOwner)
    ) =
    case Fixed.toVector nextOwner of
      Ur nextVector ->
        case Fixed.toVector weightOwner of
          Ur weightVector ->
            case Fixed.toVector markOwner of
              Ur markVector ->
                case Growable.toVector payloadOwner of
                  Ur payloadVector ->
                    case Growable.toVector scoreOwner of
                      Ur scoreVector ->
                        case Growable.toVector linkOwner of
                          Ur linkVector ->
                            U.length nextVector `lseq`
                              U.length weightVector `lseq`
                                V.length payloadVector `lseq`
                                  U.length linkVector `lseq`
                                    Ur
                                      MultiStoreScanOutput
                                        { outputDigest =
                                            digestVectors
                                              digest
                                              markVector
                                              scoreVector
                                        , outputMarks = markVector
                                        , outputScores = scoreVector
                                        }

finishMultiStoreResult ::
  Int ->
  AccessTrace ->
  MultiStore %1 ->
  Ur MultiStoreScanResult
finishMultiStoreResult inputValidationReads trace store =
  case finishMultiStoreOutput (traceReadDigest trace) store of
    Ur output ->
      Ur
        MultiStoreScanResult
          { resultSummary =
              MultiStoreScanSummary
                { visitedNodes = traceVisitedNodes trace
                , elementReads = traceElementReads trace
                , elementWrites = traceElementWrites trace
                , headerReads = traceHeaderReads trace
                , validationReads = inputValidationReads
                , finalDigest = outputDigest output
                }
          , resultVisitedIndices =
              U.fromListN
                multiStoreScanNodeCount
                (reverse (traceVisitedIndicesRev trace))
          , resultEvents =
              V.fromListN
                (traceElementReads trace + traceElementWrites trace)
                (reverse (traceEventsRev trace))
          , resultEventDigest = traceEventDigest trace
          , resultMarks = outputMarks output
          , resultScores = outputScores output
          }

owningFixedRootsField ::
  RecordLabel
    OwningMultiStore
    "owningFixedRoots"
    OwningFixedRoots
owningFixedRootsField = #owningFixedRoots

owningGrowableRootsField ::
  RecordLabel
    OwningMultiStore
    "owningGrowableRoots"
    OwningGrowableRoots
owningGrowableRootsField = #owningGrowableRoots

owningNextField ::
  RecordLabel
    OwningFixedRoots
    "owningNext"
    (OwningUnboxedFixed.Vector Int)
owningNextField = #owningNext

owningWeightField ::
  RecordLabel
    OwningFixedRoots
    "owningWeight"
    (OwningUnboxedFixed.Vector Int)
owningWeightField = #owningWeight

owningMarkField ::
  RecordLabel
    OwningFixedRoots
    "owningMark"
    (OwningUnboxedFixed.Vector Int)
owningMarkField = #owningMark

owningPayloadField ::
  RecordLabel
    OwningGrowableRoots
    "owningPayload"
    (OwningBoxedGrowable.GrowableVector (Int, Int))
owningPayloadField = #owningPayload

owningScoreField ::
  RecordLabel
    OwningGrowableRoots
    "owningScore"
    (OwningUnboxedGrowable.GrowableVector Int)
owningScoreField = #owningScore

owningLinkField ::
  RecordLabel
    OwningGrowableRoots
    "owningLink"
    (OwningUnboxedGrowable.GrowableVector Int)
owningLinkField = #owningLink

fixedUnrestrictedRootsField ::
  RecordLabel
    FixedUnrestrictedStore
    "fixedUnrestrictedRoots"
    FixedRoots
fixedUnrestrictedRootsField = #fixedUnrestrictedRoots

fixedUnrestrictedGrowableRootsField ::
  RecordLabel
    FixedUnrestrictedStore
    "fixedUnrestrictedGrowableRoots"
    OwningGrowableRoots
fixedUnrestrictedGrowableRootsField = #fixedUnrestrictedGrowableRoots

fixedRootsField ::
  RecordLabel MultiStore "fixedRoots" FixedRoots
fixedRootsField = #fixedRoots

growableRootsField ::
  RecordLabel MultiStore "growableRoots" GrowableRoots
growableRootsField = #growableRoots

nextField ::
  RecordLabel FixedRoots "next" (Fixed.Vector U.Vector Int)
nextField = #next

weightField ::
  RecordLabel FixedRoots "weight" (Fixed.Vector U.Vector Int)
weightField = #weight

markField ::
  RecordLabel FixedRoots "mark" (Fixed.Vector U.Vector Int)
markField = #mark

payloadField ::
  RecordLabel
    GrowableRoots
    "payload"
    (Growable.GrowableVector V.Vector (Int, Int))
payloadField = #payload

scoreField ::
  RecordLabel
    GrowableRoots
    "score"
    (Growable.GrowableVector U.Vector Int)
scoreField = #score

linkField ::
  RecordLabel
    GrowableRoots
    "link"
    (Growable.GrowableVector U.Vector Int)
linkField = #link

digestVectors :: Int64 -> U.Vector Int -> U.Vector Int -> Int64
digestVectors initial marks scores =
  let !marksDigest =
        U.ifoldl'
          (\digest index value -> mixDigest digest (index * 17 + value))
          initial
          marks
   in U.ifoldl'
        (\digest index value -> mixDigest digest (index * 31 + value))
        marksDigest
        scores

mixDigest :: Int64 -> Int -> Int64
mixDigest digest value =
  digest * 1_099_511_628_211 + fromIntegral value

benches :: [Benchmark]
benches =
  [ env (pure multiStoreScanDirectInput) \input ->
      bgroup
        "multi-store-scan"
        [ bench "direct" $ nf multiStoreScanDirectBenchmarkRoot input
        , bench "direct/header-matched" $
            nf multiStoreScanDirectHeaderMatchedBenchmarkRoot input
        , bench "pure-borrow/all-owning" $
            nf multiStoreScanPureBorrowOwningBenchmarkRoot input
        , bench "pure-borrow/fixed-unrestricted" $
            nf
              multiStoreScanPureBorrowFixedUnrestrictedBenchmarkRoot
              input
        , bench "pure-borrow/all-unrestricted/direct-shape" $
            nf multiStoreScanPureBorrowDirectBenchmarkRoot input
        , bench "pure-borrow/all-unrestricted/nested-shape" $
            nf multiStoreScanPureBorrowNestedBenchmarkRoot input
        ]
  ]

defaultMain :: IO ()
defaultMain = Bench.defaultMain benches
