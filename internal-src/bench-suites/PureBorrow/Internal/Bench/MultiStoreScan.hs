{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module PureBorrow.Internal.Bench.MultiStoreScan (
  MultiStoreScanInput,
  MultiStoreScanOutput (..),
  MultiStoreScanResult (..),
  MultiStoreScanSummary (..),
  multiStoreScanDirectInput,
  multiStoreScanDirectBenchmarkRoot,
  multiStoreScanDirectRoot,
  multiStoreScanNodeCount,
  benches,
  defaultMain,
) where

import Control.DeepSeq (NFData)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
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
  , finalDigest :: !Int64
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data MultiStoreScanResult = MultiStoreScanResult
  { resultSummary :: !MultiStoreScanSummary
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
  deriving stock (Generic)
  deriving anyclass (NFData)

data AccessTrace = AccessTrace
  { traceVisitedNodes :: !Int
  , traceElementReads :: !Int
  , traceElementWrites :: !Int
  , traceHeaderReads :: !Int
  , traceReadDigest :: !Int64
  }

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

multiStoreScanDirectRoot :: MultiStoreScanInput -> MultiStoreScanResult
{-# NOINLINE multiStoreScanDirectRoot #-}
multiStoreScanDirectRoot input =
  unsafePerformIO do
    next <- U.thaw (inputNext input)
    weight <- U.thaw (inputWeight input)
    mark <- U.thaw (inputMark input)
    payloadBuffer <- V.thaw (inputPayload input)
    scoreBuffer <- U.thaw (inputScore input)
    linkBuffer <- U.thaw (inputLink input)

    payloadHeader <- newIORef (multiStoreScanNodeCount, payloadBuffer)
    scoreHeader <- newIORef (multiStoreScanNodeCount, scoreBuffer)
    linkHeader <- newIORef (multiStoreScanNodeCount, linkBuffer)
    (payloadTrace, (_, payload)) <-
      traceHeaderRead emptyAccessTrace payloadHeader
    (scoreTrace, (_, score)) <-
      traceHeaderRead payloadTrace scoreHeader
    (linkTrace, (_, link)) <-
      traceHeaderRead scoreTrace linkHeader

    trace <-
      multiStoreScanTraceWorker
        multiStoreScanNodeCount
        0
        0
        linkTrace
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
              , finalDigest = digest
              }
        , resultMarks = frozenMarks
        , resultScores = frozenScores
        }

emptyAccessTrace :: AccessTrace
emptyAccessTrace = AccessTrace 0 0 0 0 0

traceHeaderRead :: AccessTrace -> IORef a -> IO (AccessTrace, a)
traceHeaderRead trace ref = do
  value <- readIORef ref
  pure
    ( trace
        { traceHeaderReads = traceHeaderReads trace + 1
        }
    , value
    )

traceUnboxedRead ::
  AccessTrace ->
  UM.IOVector Int ->
  Int ->
  IO (AccessTrace, Int)
traceUnboxedRead trace vector index = do
  value <- UM.unsafeRead vector index
  pure
    ( trace
        { traceElementReads = traceElementReads trace + 1
        }
    , value
    )

traceBoxedRead ::
  AccessTrace ->
  MV.IOVector (Int, Int) ->
  Int ->
  IO (AccessTrace, (Int, Int))
traceBoxedRead trace vector index = do
  value <- MV.unsafeRead vector index
  pure
    ( trace
        { traceElementReads = traceElementReads trace + 1
        }
    , value
    )

traceUnboxedWrite ::
  AccessTrace ->
  UM.IOVector Int ->
  Int ->
  Int ->
  IO AccessTrace
traceUnboxedWrite trace vector index value = do
  UM.unsafeWrite vector index value
  pure
    trace
      { traceElementWrites = traceElementWrites trace + 1
      }

multiStoreScanTraceWorker ::
  Int ->
  Int ->
  Int ->
  AccessTrace ->
  UM.IOVector Int ->
  UM.IOVector Int ->
  UM.IOVector Int ->
  MV.IOVector (Int, Int) ->
  UM.IOVector Int ->
  UM.IOVector Int ->
  IO AccessTrace
multiStoreScanTraceWorker !remaining !index !visits trace next weight mark payload score link
  | remaining <= 0 = pure trace
  | otherwise = do
      let !visitedTrace =
            trace
              { traceVisitedNodes = traceVisitedNodes trace + 1
              }
      (nextTrace, nextIndex) <-
        traceUnboxedRead visitedTrace next index
      (weightTrace, weightValue) <-
        traceUnboxedRead nextTrace weight index
      (markTrace, markValue) <-
        traceUnboxedRead weightTrace mark index
      (payloadTrace, (payloadTag, payloadDelta)) <-
        traceBoxedRead markTrace payload index
      (scoreTrace, scoreValue) <-
        traceUnboxedRead payloadTrace score index
      (linkTrace, linkValue) <-
        traceUnboxedRead scoreTrace link index
      let !shouldWrite =
            (weightValue + scoreValue + payloadTag + visits) `rem` 5 == 0
          !digest =
            traceReadDigest linkTrace
              + fromIntegral
                ( nextIndex
                    + weightValue
                    + markValue
                    + payloadTag
                    + payloadDelta
                    + scoreValue
                    + linkValue
                )
          !digestTrace = linkTrace {traceReadDigest = digest}
      writtenTrace <-
        if shouldWrite
          then do
            markWritten <-
              traceUnboxedWrite digestTrace mark index (markValue + 1)
            traceUnboxedWrite
              markWritten
              score
              index
              (scoreValue + payloadDelta + 1)
          else pure digestTrace
      multiStoreScanTraceWorker
        (remaining - 1)
        ((nextIndex + linkValue) `rem` multiStoreScanNodeCount)
        (visits + 1)
        writtenTrace
        next
        weight
        mark
        payload
        score
        link

multiStoreScanDirectBenchmarkRoot ::
  MultiStoreScanInput ->
  MultiStoreScanOutput
{-# NOINLINE multiStoreScanDirectBenchmarkRoot #-}
multiStoreScanDirectBenchmarkRoot input =
  unsafePerformIO do
    next <- U.thaw (inputNext input)
    weight <- U.thaw (inputWeight input)
    mark <- U.thaw (inputMark input)
    payloadBuffer <- V.thaw (inputPayload input)
    scoreBuffer <- U.thaw (inputScore input)
    linkBuffer <- U.thaw (inputLink input)

    payloadHeader <- newIORef (multiStoreScanNodeCount, payloadBuffer)
    scoreHeader <- newIORef (multiStoreScanNodeCount, scoreBuffer)
    linkHeader <- newIORef (multiStoreScanNodeCount, linkBuffer)
    (_, payload) <- readIORef payloadHeader
    (_, score) <- readIORef scoreHeader
    (_, link) <- readIORef linkHeader

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

multiStoreScanDirectWorker ::
  Int ->
  Int ->
  Int ->
  Int64 ->
  UM.IOVector Int ->
  UM.IOVector Int ->
  UM.IOVector Int ->
  MV.IOVector (Int, Int) ->
  UM.IOVector Int ->
  UM.IOVector Int ->
  IO Int64
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
        [bench "direct" $ nf multiStoreScanDirectBenchmarkRoot input]
  ]

defaultMain :: IO ()
defaultMain = Bench.defaultMain benches
