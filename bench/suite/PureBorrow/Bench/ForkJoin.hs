{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | The cost of 'parBO' itself: a balanced tree of forks whose leaves do no work.

A tree of depth @d@ makes @2^d - 1@ calls, so the time and allocation per call are the totals divided by that.
It uses only 'parBO', whose signature is stable, so the same code measures every implementation of it.
-}
module PureBorrow.Bench.ForkJoin (
  test_forkJoin,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Prelude qualified as NonLinear

-- | Sum the leaves of a fork tree of the given depth, each leaf contributing one.
forkTree :: Int -> Int
{-# NOINLINE forkTree #-}
forkTree depth = linearly \lin -> runBO_ lin (go depth)
  where
    go :: Int -> BO α Int
    go 0 = Control.pure 1
    go d = Control.do
      (left, right) <- parBO (go (d - 1)) (go (d - 1))
      Control.pure (left + right)

test_forkJoin :: [Benchmark]
test_forkJoin =
  [ bgroup
      "parBO/fork-join"
      [ bench ("depth " <> NonLinear.show depth) (nf forkTree depth)
      | depth <- [4, 8, 10]
      ]
  ]
