{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Int (Int64)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import GHC.Stats (
  RTSStats (
    allocated_bytes,
    copied_bytes,
    gc_cpu_ns,
    gc_elapsed_ns,
    max_live_bytes,
    mutator_cpu_ns,
    mutator_elapsed_ns
  ),
  getRTSStats,
  getRTSStatsEnabled,
 )
import PureBorrow.Internal.Bench.MultiStoreScan
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
  arguments <- getArgs
  (rootName, repetitions) <-
    case arguments of
      [rootName, repetitionsText] ->
        case reads repetitionsText of
          [(repetitions, "")]
            | repetitions > 0 ->
                pure (rootName, repetitions)
          _ -> usage
      _ -> usage
  enabled <- getRTSStatsEnabled
  when (not enabled) $
    die "RTS statistics are disabled; run with +RTS -T"
  root <- selectRoot rootName
  inputs <- evaluate (force (allocationInputs repetitions))
  performGC
  before <- getRTSStats
  digest <- runRoots root inputs 0 0
  performGC
  after <- getRTSStats
  let allocated = allocated_bytes after - allocated_bytes before
      copied = copied_bytes after - copied_bytes before
      mutatorCpu = mutator_cpu_ns after - mutator_cpu_ns before
      mutatorElapsed = mutator_elapsed_ns after - mutator_elapsed_ns before
      gcCpu = gc_cpu_ns after - gc_cpu_ns before
      gcElapsed = gc_elapsed_ns after - gc_elapsed_ns before
      bytesPerRun = fromIntegral allocated / fromIntegral repetitions :: Double
      bytesPerVisit =
        bytesPerRun / fromIntegral multiStoreScanNodeCount
  printf
    "root,repetitions,allocated_bytes,bytes_per_run,bytes_per_visit,copied_bytes,max_live_bytes,mutator_cpu_ns,mutator_elapsed_ns,gc_cpu_ns,gc_elapsed_ns,digest\n"
  printf
    "%s,%d,%d,%.3f,%.6f,%d,%d,%d,%d,%d,%d,%d\n"
    rootName
    repetitions
    allocated
    bytesPerRun
    bytesPerVisit
    copied
    (max_live_bytes after)
    mutatorCpu
    mutatorElapsed
    gcCpu
    gcElapsed
    digest

usage :: IO a
usage =
  die
    "usage: multi-store-scan-allocation ROOT REPETITIONS\n\
    \ROOT: direct | direct-header-matched | all-owning | fixed-unrestricted | all-unrestricted-direct | all-unrestricted-nested"

selectRoot ::
  String ->
  IO (MultiStoreScanInput -> MultiStoreScanOutput)
selectRoot = \case
  "direct" ->
    pure multiStoreScanDirectBenchmarkRoot
  "direct-header-matched" ->
    pure multiStoreScanDirectHeaderMatchedBenchmarkRoot
  "all-owning" ->
    pure multiStoreScanPureBorrowOwningBenchmarkRoot
  "fixed-unrestricted" ->
    pure multiStoreScanPureBorrowFixedUnrestrictedBenchmarkRoot
  "all-unrestricted-direct" ->
    pure multiStoreScanPureBorrowDirectBenchmarkRoot
  "all-unrestricted-nested" ->
    pure multiStoreScanPureBorrowNestedBenchmarkRoot
  rootName ->
    die ("unknown root: " <> rootName)

allocationInputs :: Int -> V.Vector MultiStoreScanInput
allocationInputs repetitions =
  V.generate repetitions \seed ->
    multiStoreScanDirectInput
      { inputMark =
          inputMark multiStoreScanDirectInput
            U.// [(0, seed)]
      }

runRoots ::
  (MultiStoreScanInput -> MultiStoreScanOutput) ->
  V.Vector MultiStoreScanInput ->
  Int ->
  Int64 ->
  IO Int64
runRoots root inputs !index !digest
  | index >= V.length inputs =
      pure digest
  | otherwise = do
      output <- evaluate (force (root (inputs V.! index)))
      runRoots
        root
        inputs
        (index + 1)
        (digest + outputDigest output)
