{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Per-element checked access through growable borrows.

Every access in these loops reads the growable header to bounds-check its index, so the loops measure that read as much as the element access itself.
They use only the element accessors, whose signatures do not depend on how the header is read, so the same code measures every header-read implementation.
-}
module PureBorrow.Bench.GrowableAccess (
  test_growableAccess,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as GrowableG
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as GrowableU
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Prelude qualified as NonLinear

boxedShareCopyAt :: V.Vector Int -> Int
{-# NOINLINE boxedShareCopyAt #-}
boxedShareCopyAt input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromVector input ownerLinear)
      let !(Ur shared) = share vector
      Ur total <- go (V.length input) 0 0 shared
      pureAfter $ consume (reclaim lend) `lseq` Ur total
  where
    go ::
      forall α.
      Int ->
      Int ->
      Int ->
      Share α (Growable.GrowableVector Int) ->
      BO α (Ur Int)
    go !len !index !acc shared
      | index >= len = Control.pure (Ur acc)
      | otherwise = Control.do
          Ur value <- Growable.copyAt index shared
          go len (index + 1) (acc + value) shared

boxedMutIncrement :: V.Vector Int -> V.Vector Int
{-# NOINLINE boxedMutIncrement #-}
boxedMutIncrement input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromVector input ownerLinear)
      finished <- go (V.length input) 0 vector
      let !() = consume finished
      pureAfter $ Growable.toVector (reclaim lend)
  where
    go ::
      forall α.
      Int ->
      Int ->
      Mut α (Growable.GrowableVector Int) %1 ->
      BO α (Mut α (Growable.GrowableVector Int))
    go !len !index vector0
      | index >= len = Control.pure vector0
      | otherwise = Control.do
          (Ur value, vector1) <- Growable.copyAtMut index vector0
          (old, vector2) <- Growable.set index (value + 1) vector1
          let !() = consume old
          go len (index + 1) vector2

unboxedShareCopyAt :: U.Vector Int -> Int
{-# NOINLINE unboxedShareCopyAt #-}
unboxedShareCopyAt input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (GrowableU.fromVector input ownerLinear)
      let !(Ur shared) = share vector
      Ur total <- go (U.length input) 0 0 shared
      pureAfter $ consume (reclaim lend) `lseq` Ur total
  where
    go ::
      forall α.
      Int ->
      Int ->
      Int ->
      Share α (GrowableU.GrowableVector Int) ->
      BO α (Ur Int)
    go !len !index !acc shared
      | index >= len = Control.pure (Ur acc)
      | otherwise = Control.do
          Ur value <- GrowableU.copyAt index shared
          go len (index + 1) (acc + value) shared

unboxedMutIncrement :: U.Vector Int -> U.Vector Int
{-# NOINLINE unboxedMutIncrement #-}
unboxedMutIncrement input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (GrowableU.fromVector input ownerLinear)
      finished <- go (U.length input) 0 vector
      let !() = consume finished
      pureAfter $ GrowableU.toVector (reclaim lend)
  where
    go ::
      forall α.
      Int ->
      Int ->
      Mut α (GrowableU.GrowableVector Int) %1 ->
      BO α (Mut α (GrowableU.GrowableVector Int))
    go !len !index vector0
      | index >= len = Control.pure vector0
      | otherwise = Control.do
          (Ur value, vector1) <- GrowableU.copyAtMut index vector0
          (old, vector2) <- GrowableU.set index (value + 1) vector1
          let !() = consume old
          go len (index + 1) vector2

genericShareCopyAt :: U.Vector Int -> Int
{-# NOINLINE genericShareCopyAt #-}
genericShareCopyAt input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (GrowableG.fromVector input ownerLinear)
      let !(Ur shared) = share vector
      Ur total <- go (U.length input) 0 0 shared
      pureAfter $ consume (reclaim lend) `lseq` Ur total
  where
    go ::
      forall α.
      Int ->
      Int ->
      Int ->
      Share α (GrowableG.GrowableVector U.Vector Int) ->
      BO α (Ur Int)
    go !len !index !acc shared
      | index >= len = Control.pure (Ur acc)
      | otherwise = Control.do
          Ur value <- GrowableG.copyAt index shared
          go len (index + 1) (acc + value) shared

genericMutIncrement :: U.Vector Int -> U.Vector Int
{-# NOINLINE genericMutIncrement #-}
genericMutIncrement input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (GrowableG.fromVector input ownerLinear)
      finished <- go (U.length input) 0 vector
      let !() = consume finished
      pureAfter $ GrowableG.toVector (reclaim lend)
  where
    go ::
      forall α.
      Int ->
      Int ->
      Mut α (GrowableG.GrowableVector U.Vector Int) %1 ->
      BO α (Mut α (GrowableG.GrowableVector U.Vector Int))
    go !len !index vector0
      | index >= len = Control.pure vector0
      | otherwise = Control.do
          (Ur value, vector1) <- GrowableG.get index vector0
          (Ur _, vector2) <- GrowableG.set index (value + 1) vector1
          go len (index + 1) vector2

test_growableAccess :: [Benchmark]
test_growableAccess =
  [ bgroup
      "growable/checked-access"
      (NonLinear.map sized [1024, 1024 * 1024])
  ]
  where
    sized :: Int -> Benchmark
    sized len =
      env
        ( NonLinear.pure
            ( V.generate len (`NonLinear.rem` 1024)
            , U.generate len (`NonLinear.rem` 1024)
            )
        )
        \ ~(boxed, unboxed) ->
          bgroup
            (NonLinear.show len)
            [ bench "boxed/share-copyAt" $ nf boxedShareCopyAt boxed
            , bench "boxed/mut-copyAtMut-set" $ nf boxedMutIncrement boxed
            , bench "unboxed/share-copyAt" $ nf unboxedShareCopyAt unboxed
            , bench "unboxed/mut-copyAtMut-set" $ nf unboxedMutIncrement unboxed
            , bench "generic/share-copyAt" $ nf genericShareCopyAt unboxed
            , bench "generic/mut-get-set" $ nf genericMutIncrement unboxed
            ]
