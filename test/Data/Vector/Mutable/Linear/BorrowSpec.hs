{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Vector.Mutable.Linear.BorrowSpec where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude qualified as NonLinear

qsort ::
  forall a α.
  (Ord a, Derefable a, Movable a) =>
  Mut α (VL.Vector a) %1 -> BO α ()
qsort = go
  where
    go :: forall β. Mut β (VL.Vector a) %1 -> BO β ()
    go v = case VL.size v of
      (Ur 0, v) -> Control.pure $ consume v
      (Ur 1, v) -> Control.pure $ consume v
      (Ur n, v) ->
        let i = n `quot` 2
         in Control.do
              (pivot, v) <- sharing v \v ->
                move . derefShare Control.<$> VL.unsafeGet i v
              pivot & \(Ur pivot) -> Control.do
                (lo, hi) <- divide pivot v 0 (n - 1)
                Control.void $ parIf (n > threshold) (go lo) (go hi)
    threshold = 8

divide ::
  forall α a.
  (Ord a, Derefable a) =>
  a ->
  Mut α (VL.Vector a) %1 ->
  Int ->
  Int ->
  BO α (Mut α (VL.Vector a), Mut α (VL.Vector a))
divide pivot = partUp
  where
    partUp
      , partDown ::
        Mut α (VL.Vector a) %1 ->
        Int ->
        Int ->
        BO α (Mut α (VL.Vector a), Mut α (VL.Vector a))
    partUp v l u
      | l < u = Control.do
          (e, v) <- sharing v $ Control.fmap derefShare . VL.unsafeGet l
          if e < pivot
            then partUp v (l + 1) u
            else partDown v l (u - 1)
      | otherwise = Control.pure $ VL.splitAtMut l v
    partDown v l u
      | l < u = Control.do
          (e, v) <- sharing v $ Control.fmap derefShare . VL.unsafeGet l
          if pivot < e
            then partDown v l (u - 1)
            else Control.do
              v <- VL.unsafeSwap v l u
              partUp v (l + 1) u
      | otherwise = Control.pure $ VL.splitAtMut l v

divideList :: [Int] -> (Int, [Int])
divideList [] = (0, [])
divideList (x : xs) =
  unur $ linearly \lin ->
    move x & \(Ur x) -> DataFlow.do
      (l1, l2, l3) <- dup3 lin
      flip runBO l1
        $ borrow (VL.fromList xs l2) l3
        & \(v, lend) ->
          VL.size v & \(Ur len, v) -> Control.do
            (lo, hi) <- divide x v 0 (len - 1)
            VL.size lo & \(Ur n, lo) -> DataFlow.do
              consume lo
              consume hi
              Control.pure \end ->
                (n, VL.toList $ reclaim end lend)

-- >>> divideList [5,3,4,1,2]

parIf :: Bool %1 -> BO α a %1 -> BO α b %1 -> BO α (a, b)
{-# INLINE parIf #-}
parIf p =
  if p
    then parBO
    else \l r -> Control.do
      !l <- l
      !r <- r
      Control.pure (l, r)

test_divideList :: TestTree
test_divideList =
  testGroup
    "divideList"
    [ testCase "empty" do
        divideList [] @?= (0, [])
    ]
