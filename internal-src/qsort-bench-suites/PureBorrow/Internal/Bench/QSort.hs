{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Internal.Bench.QSort (
  defaultMain,
  defaultMainWith,
  optionsP,
  rawOptsP,
  BenchOpts (..),
  benches,
  kMAX_SIZE,
) where

import Control.Applicative
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.DivideConquer.Linear (qsortDC)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Proxy (Proxy (..))
import Data.Unrestricted.Linear (dup3)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as AI
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Options.Applicative qualified as Opts
import Prelude.Linear (unur)
import Prelude.Linear qualified as PL
import System.Random.Stateful
import Test.Tasty (askOption, defaultMainWithIngredients)
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench qualified as Bench
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options
import Text.Read (readMaybe)
import Prelude as P

data Mode = Parallel Word | Worksteal Int | Sequential | IntroSort
  deriving (Show, Eq, Ord)

data BenchOpts = BenchOpts {numThreads :: !Int, sampleSize :: !Int}
  deriving (Show, Eq, Ord)

optionsP :: Opts.ParserInfo BenchOpts
optionsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc
      <> Opts.progDesc "Options for qsort benchmark"
  where
    p :: Opts.Parser BenchOpts
    p =
      BenchOpts
        <$> Opts.option
          Opts.auto
          ( Opts.long "threads"
              <> Opts.short 'N'
              <> Opts.metavar "NUM_THREADS"
              <> Opts.help "Number of threads to use for parallel benchmarks"
          )
        <*> Opts.option
          Opts.auto
          ( Opts.long "size"
              <> Opts.short 's'
              <> Opts.metavar "SAMPLE_SIZE"
              <> Opts.help "Number of samples to take (must divide 32768)"
          )

rawOptsP :: Opts.Parser BenchOpts
rawOptsP =
  BenchOpts
    <$> Opts.option
      Opts.auto
      ( Opts.long "threads"
          <> Opts.short 'N'
          <> Opts.value 10
          <> Opts.metavar "NUM_THREADS"
          <> Opts.help "Number of threads to use for parallel benchmarks"
      )
    <*> Opts.option
      Opts.auto
      ( Opts.long "size"
          <> Opts.short 's'
          <> Opts.metavar "SAMPLE_SIZE"
          <> Opts.value 32
          <> Opts.help "Number of samples to take (must divide 32768)"
      )

qsortWith :: Mode -> V.Vector Int -> V.Vector Int
qsortWith IntroSort v = V.modify AI.sort v
qsortWith (Parallel budget) v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin Control.do
        (v, lend) <- Control.pure PL.$ borrow (VL.fromVector v l2) l3
        VL.qsort budget v
        Control.pure PL.$ VL.toVector Control.<$> reclaim' lend
qsortWith Sequential v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin \ @α -> Control.do
        (v, lend) <- Control.pure PL.$ borrow @α (VL.fromVector v l2) l3
        VL.qsort 0 v
        pureAfter (VL.toVector PL.$ reclaim lend)
qsortWith (Worksteal p) v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin \ @α -> Control.do
        (v, lend) <- Control.pure PL.$ borrow @α (VL.fromVector v l2) l3
        Control.void PL.$ qsortDC p 128 v
        pureAfter (VL.toVector PL.$ reclaim lend)

data SampleSize = SampleSize Int
  deriving (Show, Eq, Ord)

instance IsOption SampleSize where
  defaultValue = SampleSize 32
  parseValue s =
    case readMaybe s of
      Just n | kMAX_SIZE `rem` n == 0 -> Just (SampleSize n)
      _ -> Nothing
  optionName = return "size"
  optionHelp = return "Step size to take a sample (must divide 32768)"

defaultMain :: IO ()
defaultMain = do
  numThreads <- getNumCapabilities
  let customOpts = [Option (Proxy :: Proxy SampleSize)]
      ingredients = includingOptions customOpts : benchIngredients
  defaultMainWithIngredients ingredients $ askOption \(SampleSize sampleSize) ->
    bgroup "All" $ benches BenchOpts {..}

defaultMainWith :: BenchOpts -> IO ()
defaultMainWith opts = do
  Bench.defaultMain $ benches opts

benches :: BenchOpts -> [Benchmark]
benches BenchOpts {..} =
  [ bgroup
      "qsort"
      [ env
          ( pure $ runStateGen_ (mkStdGen 42) \g -> do
              V.replicateM size (uniformM g)
          )
          \vec ->
            bgroup
              (show size)
              ( [ bench "intro" $ nf (qsortWith IntroSort) vec
                , bench "sequential" $ nf (qsortWith Sequential) vec
                ]
                  ++ [ bench ("parallel (budget = " <> show n <> ")") $
                         nf (qsortWith $ Parallel n) vec
                     | n <- [4, 8, 16, 32]
                     ]
                  ++ [ bench ("worksteal (workers = " <> show n <> ")") $
                         nf (qsortWith $ Worksteal n) vec
                     | n <- [2, 4 .. numThreads]
                     ]
              )
      | i <- [0 .. sampleSize]
      , let size = i * kMAX_SIZE `quot` sampleSize
      ]
  ]

kMAX_SIZE :: Int
kMAX_SIZE = 32 * 1024
