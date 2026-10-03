{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Kernels whose results depend on effects being ordered by the 'BO' state token.

Each one fails as wrong data, not as a crash, when a read or a write through a borrow escapes that ordering.
@PureBorrow.Unoptimised.Ordering@, in the component @pure-borrow-unoptimised@, compiles the same kernels at @-O0@, where a write deferred into a lazily forced result shows up deterministically; this module is built at the suite's @-O2@, where common-subexpression elimination serves a stale read.
Keep the two copies identical.
-}
module Control.Monad.Borrow.Pure.OrderingSpec.Kernels (
  refUpdateWrites,
  hashMapTakeEmpties,
  growableSizeAfterReclaim,
  hashMapSizeAfterReclaim,
  refDupReclaimFresh,
  refDupReclaimOwners,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.HashMap.RobinHood.Mutable.Linear.Borrow qualified as HM
import Data.List qualified as List
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as GV
import Prelude.Linear
import Prelude qualified as NonLinear

-- | Expected @(0, 1)@: the update reports the old value and leaves the new one in the owner.
refUpdateWrites :: (Int, Int)
{-# NOINLINE refUpdateWrites #-}
refUpdateWrites = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l1
  case modifyBO r0 l2 bump of
    (old, r1) -> case Ref.free r1 of
      !now -> (old, now)
  where
    bump :: Mut α (Ref.Ref Int) %1 -> BO α Int
    bump m = Control.do
      x <- RefB.update (\a -> case move a of Ur a' -> Control.pure (a', a' + 1)) m
      Control.pure (case x of (old, m') -> m' `lseq` old)

{- | Expected @([(1, 10), (2, 20)], [(3, 30)])@.

'HM.take_' empties the table, so the later insert lands in the emptied table while the taken one keeps its entries.
-}
hashMapTakeEmpties :: ([(Int, Int)], [(Int, Int)])
{-# NOINLINE hashMapTakeEmpties #-}
hashMapTakeEmpties = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, lin) <- dup lin
  (l2, lin) <- dup lin
  (l3, lin) <- dup lin
  (l4, l5) <- dup lin
  hm <- HM.empty 64 l0
  hm <- modifyBO_ hm l1 insert12
  case modifyBO hm l2 HM.take_ of
    (taken, hm1) -> case modifyBO_ hm1 l3 insert3 of
      hm2 -> case readEntries hm2 l4 of
        !left -> case readEntries taken l5 of
          !tk -> (tk, left)
  where
    insert12 :: Mut α (HM.HashMap Int Int) %1 -> BO α ()
    insert12 m = Control.do
      (Ur _, m) <- HM.insert 1 10 m
      (Ur _, m) <- HM.insert 2 20 m
      Control.pure (consume m)
    insert3 :: Mut α (HM.HashMap Int Int) %1 -> BO α ()
    insert3 m = Control.do
      (Ur _, m) <- HM.insert 3 30 m
      Control.pure (consume m)
    readEntries :: HM.HashMap Int Int %1 -> Linearly %1 -> [(Int, Int)]
    readEntries hm l = runBO l Control.do
      (m, lend) <- borrowM hm
      (Ur xs, m) <- HM.toList m
      Control.pure (consume m)
      pureAfter (consume (reclaim lend) `lseq` deep (List.sort xs))
    deep :: [(Int, Int)] -> [(Int, Int)]
    deep xs = NonLinear.foldr (\(a, b) r -> a `NonLinear.seq` b `NonLinear.seq` r) () xs `NonLinear.seq` xs

-- | Expected @(3, 4)@: a size read through a fresh borrow of the reclaimed owner sees the push.
growableSizeAfterReclaim :: (Int, Int)
{-# NOINLINE growableSizeAfterReclaim #-}
growableSizeAfterReclaim = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  gv <- GV.fromList [1, 2, 3] l1
  runBO l2 Control.do
    (m, lend) <- borrowM gv
    (Ur before, m) <- GV.size m
    m <- GV.push 4 m
    Control.pure (consume m)
    pureAfter (sizeOwner (reclaim lend) l3 before)
  where
    sizeOwner :: GV.GrowableVector Int %1 -> Linearly %1 -> Int -> (Int, Int)
    sizeOwner g l before = runBO l Control.do
      (m, lend) <- borrowM g
      (Ur after, m) <- GV.size m
      Control.pure (consume m)
      pureAfter (consume (GV.toList (reclaim lend)) `lseq` (before, after))

-- | Expected @(2, 3)@: the same shape for the hash map.
hashMapSizeAfterReclaim :: (Int, Int)
{-# NOINLINE hashMapSizeAfterReclaim #-}
hashMapSizeAfterReclaim = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  hm <- HM.fromList [(1 :: Int, 10 :: Int), (2, 20)] l1
  runBO l2 Control.do
    (m, lend) <- borrowM hm
    (Ur before, m) <- HM.size m
    (Ur _, m) <- HM.insert 3 30 m
    Control.pure (consume m)
    pureAfter (sizeOwner (reclaim lend) l3 before)
  where
    sizeOwner :: HM.HashMap Int Int %1 -> Linearly %1 -> Int -> (Int, Int)
    sizeOwner h l before = runBO l Control.do
      (m, lend) <- borrowM h
      (Ur after, m) <- HM.size m
      Control.pure (consume m)
      pureAfter (consume (reclaim lend) `lseq` (before, after))

-- | Expected @(1, 0)@: the owner reclaimed after the scope is not served from the read that 'dup2' made before it.
refDupReclaimFresh :: (Int, Int)
{-# NOINLINE refDupReclaimFresh #-}
refDupReclaimFresh = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l1
  (r1, r2) <- dup2 r0
  runBO l2 Control.do
    (m, lend) <- borrowM r1
    m <- RefB.modify (+ 1) m
    Control.pure (consume m)
    pureAfter (Ref.free (reclaim lend), Ref.free r2)

{- | Expected @(100, 1)@: the owner reclaimed after the scope holds the reference the scope put in, not the one it took out.

A stale read of the reclaimed owner would hand back the taken reference a second time, so that two owners bump and read one cell: @(1, 1)@.
-}
refDupReclaimOwners :: (Int, Int)
{-# NOINLINE refDupReclaimOwners #-}
refDupReclaimOwners = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, lin) <- dup lin
  (l3, l4) <- dup lin
  inner <- Ref.new (0 :: Int) l1
  outer <- Ref.new inner l2
  (o1, o2) <- dup2 outer
  runBO l3 Control.do
    (m, lend) <- borrowM o1
    fresh <- asksLinearly (Ref.new (100 :: Int))
    (taken, m) <- RefB.update (\old -> Control.pure (old, fresh)) m
    Control.pure (consume m)
    pureAfter (observe taken (Ref.free (reclaim lend)) o2 l4)
  where
    observe :: Ref.Ref Int %1 -> Ref.Ref Int %1 -> Ref.Ref (Ref.Ref Int) %1 -> Linearly %1 -> (Int, Int)
    observe taken current o2 l = runBO l Control.do
      (mt, lendT) <- borrowM taken
      mt <- RefB.modify (+ 1) mt
      Control.pure (consume mt)
      (mc, lendC) <- borrowM current
      c <- RefB.copyRef mc
      pureAfter (consume o2 `lseq` consume (reclaim lendC) `lseq` (c, Ref.free (reclaim lendT)))
