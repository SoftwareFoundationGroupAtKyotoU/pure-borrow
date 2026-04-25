{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Demo.QSort (
  defaultMain,
  defaultMainWith,
  CLIOpts (..),
  optionsP,
) where

import Control.Applicative ((<**>), (<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.DivideConquer.Linear (qsortDC)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Control.Functor.Linear as Control
import Control.Monad.Borrow.Pure
import qualified Control.Syntax.DataFlow as DataFlow
import Data.Functor (void)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as AI
import qualified Data.Vector.Mutable.Linear.Borrow as VL
import GHC.Generics (Generic)
import qualified Options.Applicative as Opts
import Prelude.Linear hiding (Eq, Ord, Semigroup (..), ($), ($!))
import qualified Prelude.Linear as PL hiding (($!))
import System.Mem (performGC)
import System.Random
import System.Random.Stateful (runStateGen_, uniformM)

data Mode = Parallel Word | Worksteal Int Int | Sequential | IntroSort
  deriving (Show, Eq, Ord, Generic)

data CLIOpts = CLIOpts {mode :: Mode, size :: Int, seed :: Maybe Int}
  deriving (Show, Eq, Ord, Generic)

optionsP :: Int -> Opts.ParserInfo CLIOpts
optionsP numCap = Opts.info (p <**> Opts.helper) $ Opts.progDesc "Parallel quicksort with linear borrows"
  where
    p = do
      mode <-
        Parallel <$> Opts.option Opts.auto (Opts.long "parallel" <> Opts.short 'p' <> Opts.help "Use parallel quicksort with specified capacity (default: 8)")
          <|> Opts.flag' Sequential (Opts.long "sequential" <> Opts.short 'S' <> Opts.help "Use sequential quicksort")
          <|> Opts.flag' (Worksteal numCap 512) (Opts.long "worksteal" <> Opts.short 'w' <> Opts.help "Use work-stealing quicksort")
          <|> Opts.flag (Parallel 8) IntroSort (Opts.long "intro" <> Opts.short 'i' <> Opts.help "Use intro sort")
      size <-
        Opts.option
          Opts.auto
          ( Opts.long "size"
              <> Opts.short 'n'
              <> Opts.value 256
              <> Opts.showDefault
              <> Opts.help "Size of the vector to sort"
          )
      seed <- Opts.optional $ Opts.option Opts.auto (Opts.long "seed" <> Opts.short 's' <> Opts.help "Random seed for vector generation (default: random)")
      pure CLIOpts {..}

qsortWith :: Mode -> StdGen -> V.Vector Int -> V.Vector Int
qsortWith IntroSort _ v = V.modify AI.sort v
qsortWith (Parallel bud) _ v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin \ @α -> Control.do
        (v, lend) <- Control.pure PL.$ borrow @α (VL.fromVector v l2) l3
        VL.qsort bud v
        pureAfter (VL.toVector PL.$ reclaim lend)
qsortWith Sequential _ v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin \ @α -> Control.do
        (v, lend) <- Control.pure PL.$ borrow @α (VL.fromVector v l2) l3
        VL.qsort 0 v
        pureAfter (VL.toVector PL.$ reclaim lend)
qsortWith (Worksteal workers thresh) _ v =
  unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin \ @α -> Control.do
        (v, lend) <- Control.pure PL.$ borrow @α (VL.fromVector v l2) l3
        Control.void PL.$ qsortDC workers thresh v
        pureAfter (VL.toVector PL.$ reclaim lend)

defaultMainWith :: CLIOpts -> IO ()
defaultMainWith CLIOpts {..} = do
  putStrLn $ "Sorting " <> show size <> " elements with mode: " <> show mode
  gen <- case seed of
    Just s -> return $ mkStdGen s
    Nothing -> newStdGen
  let !vec =
        runStateGen_ gen \g -> do
          V.replicateM size (uniformM g)
  gen <- newStdGen
  performGC
  void $ evaluate $ force $ qsortWith mode gen vec

defaultMain :: IO ()
defaultMain = do
  numCap <- getNumCapabilities
  opts <- Opts.execParser $ optionsP numCap
  defaultMainWith opts
