{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Applicative ((<**>), (<|>))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.DivideConquer.Linear (divideAndConquer, qsortDC)
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
          <|> Opts.flag' (Worksteal numCap 4) (Opts.long "worksteal" <> Opts.short 'w' <> Opts.help "Use work-stealing quicksort")
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

qsortWith :: Mode -> V.Vector Int -> V.Vector Int
qsortWith IntroSort v = V.modify AI.sort v
qsortWith (Parallel bud) v =
  unur PL.$ unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin Control.do
        (v, lend) <- Control.pure PL.$ borrow (VL.fromVector v l2) l3
        VL.qsort bud v
        Control.pure PL.$ \end -> VL.toVector (reclaim end lend)
qsortWith Sequential v =
  unur PL.$ unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin Control.do
        (v, lend) <- Control.pure PL.$ borrow (VL.fromVector v l2) l3
        VL.qsort 0 v
        Control.pure PL.$ \end -> VL.toVector (reclaim end lend)
qsortWith (Worksteal workers thresh) v =
  unur PL.$ unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin Control.do
        (v, lend) <- Control.pure PL.$ borrow (VL.fromVector v l2) l3
        Control.void PL.$ divideAndConquer workers (qsortDC thresh) v
        Control.pure PL.$ \end -> VL.toVector (reclaim end lend)

main :: IO ()
main = do
  numCap <- getNumCapabilities
  CLIOpts {..} <- Opts.execParser $ optionsP numCap
  putStrLn $ "Sorting " <> show size <> " elements with mode: " <> show mode
  gen <- case seed of
    Just s -> return $ mkStdGen s
    Nothing -> newStdGen
  let !vec =
        runStateGen_ gen \g -> do
          V.replicateM size (uniformM g)
  performGC
  void $ evaluate $ force $ qsortWith mode vec
