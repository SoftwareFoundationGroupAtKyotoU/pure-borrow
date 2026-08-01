{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PureBorrow.Internal.Bench.Unboxed (
  defaultMain,
  benches,
  directFixedUnboxedUpdateLoop,
  pureBorrowFixedUnboxedUpdateLoop,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.ST.Strict (ST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Vector
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench qualified as Bench
import Prelude qualified as NonLinear

directFixedUnboxedUpdateLoop :: U.Vector Int -> U.Vector Int
{-# NOINLINE directFixedUnboxedUpdateLoop #-}
directFixedUnboxedUpdateLoop input =
  U.modify (\vector -> directWorker (UM.length vector) 0 vector) input

directWorker :: Int -> Int -> UM.MVector s Int -> ST s ()
{-# NOINLINE directWorker #-}
directWorker !length_ !index vector
  | index >= length_ = NonLinear.pure ()
  | otherwise = do
      value <- UM.unsafeRead vector index
      UM.unsafeWrite vector index (value + 1)
      directWorker length_ (index + 1) vector

pureBorrowFixedUnboxedUpdateLoop :: U.Vector Int -> U.Vector Int
{-# NOINLINE pureBorrowFixedUnboxedUpdateLoop #-}
pureBorrowFixedUnboxedUpdateLoop input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Vector.fromVector input ownerLinear)
      pureBorrowWorker (U.length input) 0 vector
      pureAfter (Vector.toVector (reclaim lend))

pureBorrowWorker ::
  forall α.
  Int ->
  Int ->
  Mut α (Vector.Vector Int) %1 ->
  BO α ()
{-# NOINLINE pureBorrowWorker #-}
pureBorrowWorker !length_ !index vector
  | index >= length_ = Control.pure (consume vector)
  | otherwise = Control.do
      ((), vector) <-
        Vector.unsafeUpdate
          index
          (\ !value -> Control.pure ((), value + 1))
          vector
      pureBorrowWorker length_ (index + 1) vector

defaultMain :: IO ()
defaultMain = Bench.defaultMain benches

benches :: [Benchmark]
benches =
  [ env
      (NonLinear.pure $ U.generate length_ (`NonLinear.rem` 1024))
      \input ->
        bgroup
          ("fixed-unboxed/" <> show length_)
          [ bench "direct" $ nf directFixedUnboxedUpdateLoop input
          , bench "pure-borrow" $ nf pureBorrowFixedUnboxedUpdateLoop input
          ]
  | length_ <- [1024, 1024 * 1024]
  ]
