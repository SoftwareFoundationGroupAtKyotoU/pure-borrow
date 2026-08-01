{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PureBorrow.Bench.Growable (
  test_growable,
  directContentUpdateLoop,
  fixedContentUpdateLoop,
  growableContentUpdateLoop,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.STRef (newSTRef, readSTRef)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Mutable.Linear.Borrow qualified as Fixed
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Prelude qualified as NonLinear

directContentUpdateLoop :: V.Vector Int -> V.Vector Int
{-# NOINLINE directContentUpdateLoop #-}
directContentUpdateLoop input =
  V.modify
    ( \mutable -> do
        -- The header ref mirrors the growable vector's indirection, so the
        -- baseline pays for the same extra load the measured variants do.
        header <- newSTRef (V.length input, mutable)
        (logicalSize, contents) <- readSTRef header
        go logicalSize 0 contents
    )
    input
  where
    go !logicalSize !index contents
      | index >= logicalSize = NonLinear.pure ()
      | otherwise = do
          value <- MV.unsafeRead contents index
          MV.unsafeWrite contents index (value + 1)
          go logicalSize (index + 1) contents

fixedContentUpdateLoop :: V.Vector Int -> V.Vector Int
{-# NOINLINE fixedContentUpdateLoop #-}
fixedContentUpdateLoop input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (initialVector, lend) <- borrowM (Fixed.fromVector input ownerLinear)
      updatedVector <- updateContents (V.length input) 0 initialVector
      let !() = consume updatedVector
      pureAfter $ Fixed.toVector (reclaim lend)

growableContentUpdateLoop :: V.Vector Int -> V.Vector Int
{-# NOINLINE growableContentUpdateLoop #-}
growableContentUpdateLoop input =
  unur $ linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (initialVector, lend) <- borrowM (Growable.fromVector input ownerLinear)
      updatedVector <- Growable.withContent_ initialVector \contents ->
        updateContents (V.length input) 0 contents
      let !() = consume updatedVector
      pureAfter $ Growable.toVector (reclaim lend)

updateContents ::
  forall α.
  Int ->
  Int ->
  Mut α (Fixed.Vector Int) %1 ->
  BO α ()
{-# INLINE updateContents #-}
updateContents !logicalSize !index contents
  | index >= logicalSize = Control.pure (consume contents)
  | otherwise = Control.do
      ((), nextContents) <-
        Fixed.unsafeUpdate
          index
          (\ !value -> Control.pure ((), value + 1))
          contents
      updateContents logicalSize (index + 1) nextContents

test_growable :: [Benchmark]
test_growable =
  [ env
      (NonLinear.pure $ V.generate (1024 * 1024) (`NonLinear.rem` 1024))
      \input ->
        bgroup
          "growable/content-update"
          [ bench "direct/header-open-once" $ nf directContentUpdateLoop input
          , bench "pure-borrow/fixed" $ nf fixedContentUpdateLoop input
          , bench "pure-borrow/withContent_" $ nf growableContentUpdateLoop input
          ]
  ]
