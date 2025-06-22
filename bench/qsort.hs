{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import qualified Control.Functor.Linear as Control
import Control.Monad.Borrow.Pure
import qualified Control.Syntax.DataFlow as DataFlow
import Data.Unrestricted.Linear (dup3)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as AI
import qualified Data.Vector.Mutable.Linear.Borrow as VL
import Prelude.Linear (unur)
import qualified Prelude.Linear as PL
import System.Random.Stateful
import Test.Tasty.Bench

data Mode = Parallel Word | Sequential | IntroSort
  deriving (Show, Eq, Ord)

qsortWith :: Mode -> V.Vector Int -> V.Vector Int
qsortWith IntroSort v = V.modify AI.sort v
qsortWith (Parallel budget) v =
  unur PL.$ unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin Control.do
        (v, lend) <- Control.pure PL.$ borrow (VL.fromVector v l2) l3
        VL.qsort budget v
        Control.pure PL.$ \end -> VL.toVector (reclaim end lend)
qsortWith Sequential v =
  unur PL.$ unur PL.$ linearly \lin ->
    DataFlow.do
      (lin, l2, l3) <- dup3 lin
      runBO lin Control.do
        (v, lend) <- Control.pure PL.$ borrow (VL.fromVector v l2) l3
        VL.qsort 0 v
        Control.pure PL.$ \end -> VL.toVector (reclaim end lend)

main :: IO ()
main =
  defaultMain
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
                    ++ [ bench ("parallel (budget = " <> show n <> ")") $ nf (qsortWith $ Parallel n) vec
                       | n <- [4, 8 .. 32]
                       ]
                )
        | i <- [0 .. 32]
        , let size = i * 1024
        ]
    ]
