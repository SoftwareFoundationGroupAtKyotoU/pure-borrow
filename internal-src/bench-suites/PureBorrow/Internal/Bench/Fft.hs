{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Internal.Bench.Fft (
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
import Control.Concurrent.DivideConquer.Linear (
  DivideConquer,
  naiveDivideAndConquer,
  sequentialDivideAndConquer,
 )
import Control.Concurrent.DivideConquer.Linear.Unrestricted (
  fftDC,
  fftDC',
 )
import Control.Concurrent.DivideConquer.Linear.Unrestricted.Internal (
  FftCoe,
  Pair,
  combineLoop,
 )
import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Complex (Complex (..))
import Data.Proxy (Proxy (..))
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as VL
import Data.Vector.Unboxed qualified as V
import Options.Applicative qualified as Opts
import Prelude.Linear (dup, unur)
import Prelude.Linear qualified as PL
import System.Random.Stateful
import Test.Tasty (askOption, defaultMainWithIngredients)
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench qualified as Bench
import Test.Tasty.Ingredients.Basic (includingOptions)
import Test.Tasty.Options
import Text.Read (readMaybe)
import Prelude as P

data Mode = NaiveDC | Worksteal Int | Sequential
  deriving (Show, Eq, Ord)

{-# SPECIALIZE fftDC' ::
  Int ->
  DivideConquer
    FftCoe
    α
    Pair
    (VL.Vector V.Vector (Complex Double))
    ()
  #-}

{-# SPECIALIZE combineLoop ::
  Int ->
  Complex Double ->
  Int ->
  Complex Double ->
  Mut α (VL.Vector V.Vector (Complex Double)) %1 ->
  BO α ()
  #-}

data BenchOpts = BenchOpts {numThreads :: !Int, sampleSize :: !Int}
  deriving (Show, Eq, Ord)

optionsP :: Opts.ParserInfo BenchOpts
optionsP =
  Opts.info (p <**> Opts.helper) $
    Opts.fullDesc
      <> Opts.progDesc "Options for fft benchmark"
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
              <> Opts.help
                "Number of logarithmic size steps (positive divisor of 10)"
          )

fun :: Double -> Double
fun x = sin (2 * pi * x) + 2 * cos (pi * x) + 3 * sin (0.5 * pi * x) + 5

sample :: Int -> (Double -> Double) -> V.Vector (Complex Double)
sample n f = V.generate n \i -> f (-4 + 8 * fromIntegral i / fromIntegral n) :+ 0

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
          <> Opts.value 10
          <> Opts.help
            "Number of logarithmic size steps (positive divisor of 10)"
      )

fftWith :: Mode -> V.Vector (Complex Double) -> V.Vector (Complex Double)
fftWith Sequential v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2) <- dup lin
      runBO lin Control.do
        (v, lend) <- borrowM (VL.fromVector v l2)
        Control.void PL.$ sequentialDivideAndConquer (fftDC' 128) v
        pureAfter (VL.toVector PL.$ reclaim lend)
fftWith NaiveDC v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2) <- dup lin
      runBO lin Control.do
        (v, lend) <- borrowM (VL.fromVector v l2)
        Control.void PL.$ naiveDivideAndConquer (fftDC' 128) v
        pureAfter (VL.toVector PL.$ reclaim lend)
fftWith (Worksteal p) v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2) <- dup lin
      runBO lin Control.do
        (v, lend) <- borrowM (VL.fromVector v l2)
        Control.void PL.$ fftDC (mkStdGen 42) p 128 v
        pureAfter (VL.toVector PL.$ reclaim lend)

data SampleSize = SampleSize Int
  deriving (Show, Eq, Ord)

instance IsOption SampleSize where
  defaultValue = SampleSize 10
  parseValue s =
    case readMaybe s of
      Just n | n > 0, kMAX_SIZE `rem` n == 0 -> Just (SampleSize n)
      _ -> Nothing
  optionName = return "size"
  optionHelp = return "Number of logarithmic size steps (positive divisor of 10)"

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
      "fft"
      [ env
          (evaluate $ sample size fun)
          \vec ->
            bgroup
              (show size)
              ( bench "sequential" (nf (fftWith Sequential) vec)
                  : bench
                    ("parallel-dc (thresh = 128)")
                    (nf (fftWith NaiveDC) vec)
                  : [ bench ("worksteal (workers = " <> show n <> ")") $
                        nf (fftWith $ Worksteal n) vec
                    | n <- [2, 4 .. numThreads]
                    ]
              )
      | i <- [0 .. sampleSize]
      , let size = 2 ^ (10 + i * kMAX_SIZE `quot` sampleSize)
      ]
  ]

kMAX_SIZE :: Int
kMAX_SIZE = 10
