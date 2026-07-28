{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PureBorrow.Internal.Bench.CopyAt (
  defaultMain,
  benches,
  composedPureBorrowCopiedReadLoop,
  directCopiedReadLoop,
  pureBorrowCopiedReadLoop,
  pureBorrowUpdateLoop,
  reborrowingUpdateLoop,
) where

import Control.Exception (evaluate)
import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.ST.Strict (runST)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Test.Tasty.Bench qualified as Bench
import Prelude qualified as NonLinear

directCopiedReadLoop :: V.Vector Int -> (Int, V.Vector Int)
{-# NOINLINE directCopiedReadLoop #-}
directCopiedReadLoop input = runST do
  mutable <- V.thaw input
  total <- go mutable 0 0
  frozen <- V.unsafeFreeze mutable
  NonLinear.pure (total, frozen)
  where
    go mutable !i !total
      | i >= MV.length mutable = NonLinear.pure total
      | otherwise = do
          value <- MV.read mutable i
          go mutable (i + 1) (total + value)

pureBorrowCopiedReadLoop :: V.Vector Int -> (Int, V.Vector Int)
{-# NOINLINE pureBorrowCopiedReadLoop #-}
pureBorrowCopiedReadLoop input = unur $ linearly \linearlyToken -> DataFlow.do
  (runToken, vectorToken) <- dup linearlyToken
  runBO runToken Control.do
    (initialMutable, lend) <- borrowM (VL.fromVector input vectorToken)
    VL.size initialMutable & \(Ur length_, sizedMutable) ->
      go length_ 0 0 sizedMutable lend
  where
    go ::
      forall α.
      Int ->
      Int ->
      Int ->
      Mut α (VL.Vector Int) %1 ->
      Lend α (VL.Vector Int) %1 ->
      BO α (After α (Ur (Int, V.Vector Int)))
    go !length_ !i !total mutable lend
      | i >= length_ =
          consume mutable `lseq`
            Control.pure
              ( ( \vector -> case VL.toVector vector of
                    Ur frozen -> Ur (total, frozen)
                )
                  Control.<$> reclaim' lend
              )
      | otherwise = Control.do
          (Ur value, nextMutable) <- VL.copyAtMut i mutable
          go length_ (i + 1) (total + value) nextMutable lend

composedPureBorrowCopiedReadLoop :: V.Vector Int -> (Int, V.Vector Int)
{-# NOINLINE composedPureBorrowCopiedReadLoop #-}
composedPureBorrowCopiedReadLoop input = unur $ linearly \linearlyToken -> DataFlow.do
  (runToken, vectorToken) <- dup linearlyToken
  runBO runToken Control.do
    (initialMutable, lend) <- borrowM (VL.fromVector input vectorToken)
    VL.size initialMutable & \(Ur length_, sizedMutable) ->
      go length_ 0 0 sizedMutable lend
  where
    go ::
      forall α.
      Int ->
      Int ->
      Int ->
      Mut α (VL.Vector Int) %1 ->
      Lend α (VL.Vector Int) %1 ->
      BO α (After α (Ur (Int, V.Vector Int)))
    go !length_ !i !total mutable lend
      | i >= length_ =
          consume mutable `lseq`
            Control.pure
              ( ( \vector -> case VL.toVector vector of
                    Ur frozen -> Ur (total, frozen)
                )
                  Control.<$> reclaim' lend
              )
      | otherwise = Control.do
          (Ur value, nextMutable) <-
            sharing @α @α mutable \shared -> VL.copyAt i shared
          go length_ (i + 1) (total + value) nextMutable lend

pureBorrowUpdateLoop :: V.Vector Int -> V.Vector Int
{-# NOINLINE pureBorrowUpdateLoop #-}
pureBorrowUpdateLoop input = unur $ linearly \linearlyToken -> DataFlow.do
  (runToken, vectorToken) <- dup linearlyToken
  runBO runToken Control.do
    (initialMutable, lend) <- borrowM (VL.fromVector input vectorToken)
    VL.size initialMutable & \(Ur length_, sizedMutable) ->
      go length_ 0 sizedMutable lend
  where
    go ::
      forall α.
      Int ->
      Int ->
      Mut α (VL.Vector Int) %1 ->
      Lend α (VL.Vector Int) %1 ->
      BO α (After α (Ur (V.Vector Int)))
    go !length_ !i mutable lend
      | i >= length_ =
          consume mutable `lseq`
            Control.pure (VL.toVector Control.<$> reclaim' lend)
      | otherwise = Control.do
          nextMutable <- VL.modify i (+ 1) mutable
          go length_ (i + 1) nextMutable lend

reborrowingUpdateLoop :: V.Vector Int -> V.Vector Int
{-# NOINLINE reborrowingUpdateLoop #-}
reborrowingUpdateLoop input = unur $ linearly \linearlyToken -> DataFlow.do
  (runToken, vectorToken) <- dup linearlyToken
  runBO runToken Control.do
    (initialMutable, lend) <- borrowM (VL.fromVector input vectorToken)
    VL.size initialMutable & \(Ur length_, sizedMutable) ->
      go length_ 0 sizedMutable lend
  where
    go ::
      forall α.
      Int ->
      Int ->
      Mut α (VL.Vector Int) %1 ->
      Lend α (VL.Vector Int) %1 ->
      BO α (After α (Ur (V.Vector Int)))
    go !length_ !i mutable lend
      | i >= length_ =
          consume mutable `lseq`
            Control.pure (VL.toVector Control.<$> reclaim' lend)
      | otherwise = Control.do
          nextMutable <-
            reborrowing_ mutable \scopedMutable -> Control.do
              modifiedScoped <- VL.modify i (+ 1) scopedMutable
              Control.pure (consume modifiedScoped)
          go length_ (i + 1) nextMutable lend

defaultMain :: IO ()
defaultMain = Bench.defaultMain benches

benches :: [Benchmark]
benches =
  [ bgroup
      "copy-at"
      [ env
          (evaluate $ V.generate size (`NonLinear.rem` 1024))
          \input ->
            bgroup
              (NonLinear.show size)
              [ bench "direct-vector" $ nf directCopiedReadLoop input
              , bench "pure-borrow/direct-copy" $ nf pureBorrowCopiedReadLoop input
              , bench "pure-borrow/composed-copy" $ nf composedPureBorrowCopiedReadLoop input
              ]
      | size <- [4 * 1024, 64 * 1024, 1024 * 1024]
      ]
  , env
      (evaluate $ V.generate (1024 * 1024) (`NonLinear.rem` 1024))
      \input ->
        bgroup
          "discarding-scope/update"
          [ bench "pure-borrow/no-scope" $ nf pureBorrowUpdateLoop input
          , bench "reborrowing_" $ nf reborrowingUpdateLoop input
          ]
  ]
