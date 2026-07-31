{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Int (Int64)
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
import PureBorrow.Internal.Bench.Worklist.Resume
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
  arguments <- getArgs
  (rootName, growth, target, repetitions) <-
    case arguments of
      [rootName, growthText, targetText, repetitionsText] -> do
        growth <- parseGrowth growthText
        target <- parseTarget targetText
        repetitions <- parseRepetitions repetitionsText
        pure (rootName, growth, target, repetitions)
      _ -> usage
  enabled <- getRTSStatsEnabled
  when (not enabled) $
    die "RTS statistics are disabled; run with +RTS -T"
  root <- selectRoot rootName growth target
  sample <- evaluate (force (root (-1)))
  let sampleSummary = summary sample
      visits = visitedNodes sampleSummary
      resumes = resumeBoundaries sampleSummary
  performGC
  before <- getRTSStats
  digest <- runRoots root repetitions 0 0
  performGC
  after <- getRTSStats
  let allocated = allocated_bytes after - allocated_bytes before
      copied = copied_bytes after - copied_bytes before
      mutatorCpu = mutator_cpu_ns after - mutator_cpu_ns before
      mutatorElapsed = mutator_elapsed_ns after - mutator_elapsed_ns before
      gcCpu = gc_cpu_ns after - gc_cpu_ns before
      gcElapsed = gc_elapsed_ns after - gc_elapsed_ns before
      bytesPerRun =
        fromIntegral allocated / fromIntegral repetitions :: Double
      bytesPerVisit = bytesPerRun / fromIntegral visits
      totalBytesPerResume =
        if resumes == 0
          then 0
          else bytesPerRun / fromIntegral resumes
  printf
    "root,growth,target,repetitions,visits_per_run,resumes_per_run,allocated_bytes,bytes_per_run,bytes_per_visit,total_bytes_per_resume,copied_bytes,process_max_live_bytes,mutator_cpu_ns,mutator_elapsed_ns,gc_cpu_ns,gc_elapsed_ns,digest\n"
  printf
    "%s,%s,%s,%d,%d,%d,%d,%.3f,%.6f,%.6f,%d,%d,%d,%d,%d,%d,%d\n"
    rootName
    (show growth)
    (show target)
    repetitions
    visits
    resumes
    allocated
    bytesPerRun
    bytesPerVisit
    totalBytesPerResume
    copied
    (max_live_bytes after)
    mutatorCpu
    mutatorElapsed
    gcCpu
    gcElapsed
    digest

parseGrowth :: String -> IO WorklistGrowth
parseGrowth = \case
  "no-growth" -> pure NoGrowth
  "sparse-growth" -> pure SparseGrowth
  "dense-growth" -> pure DenseGrowth
  value -> die ("unknown growth mode: " <> value)

parseTarget :: String -> IO WorklistTarget
parseTarget = \case
  "drain" -> pure Drain
  "stop-early" -> pure StopEarly
  value -> die ("unknown target: " <> value)

parseRepetitions :: String -> IO Int
parseRepetitions value =
  case reads value of
    [(repetitions, "")]
      | repetitions > 0 ->
          pure repetitions
    _ -> usage

selectRoot ::
  String ->
  WorklistGrowth ->
  WorklistTarget ->
  IO (Int -> WorklistOutput)
selectRoot rootName growth target =
  case rootName of
    "direct-open-once" ->
      pure \seed ->
        worklistDirectOpenOnceRootWithSeed seed target
    "pure-borrow-open-once" ->
      pure \seed ->
        worklistPureBorrowOpenOnceRootWithSeed seed target
    "direct-flat" ->
      pure \seed ->
        worklistDirectReopenRootWithSeed
          seed
          FlatReopen
          growth
          target
    "pure-borrow-flat" ->
      pure \seed ->
        worklistPureBorrowFlatReopenRootWithSeed
          seed
          growth
          target
    "direct-nested" ->
      pure \seed ->
        worklistDirectReopenRootWithSeed
          seed
          NestedReopen
          growth
          target
    "pure-borrow-nested" ->
      pure \seed ->
        worklistPureBorrowNestedReopenRootWithSeed
          seed
          growth
          target
    _ ->
      die ("unknown root: " <> rootName)

runRoots ::
  (Int -> WorklistOutput) ->
  Int ->
  Int ->
  Int64 ->
  IO Int64
runRoots root repetitions !seed !digest
  | seed >= repetitions =
      pure digest
  | otherwise = do
      output <- evaluate (force (root seed))
      runRoots
        root
        repetitions
        (seed + 1)
        (digest + finalDigest (summary output))

usage :: IO a
usage =
  die
    "usage: worklist-resume-allocation ROOT GROWTH TARGET REPETITIONS\n\
    \ROOT: direct-open-once | pure-borrow-open-once | direct-flat | pure-borrow-flat | direct-nested | pure-borrow-nested\n\
    \GROWTH: no-growth | sparse-growth | dense-growth\n\
    \TARGET: drain | stop-early"
