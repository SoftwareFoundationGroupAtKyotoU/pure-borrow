{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Rebinding a threaded linear borrow under its own name is the idiom these
-- loops are written in, as it is in the library itself.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
R4 --- the scope-density regression.

Every other benchmark in this package is dominated by what happens /inside/ a
borrow scope. This one is dominated by the scopes themselves: one owner, one
@'Ref.Ref' 'Int'@, and a loop whose body is a single read-modify-write, crossed
by exactly one scope per iteration. The arms differ only in which delimiter
that crossing goes through, so the difference between an arm and the
scope-free control is the cost of one crossing, and the sweep over iteration
counts turns that difference into a slope rather than a single number.

The fixture exists because nothing here could see the two things that matter
most about the delimiters:

  * @c35dace@ removed the runtime construction of the sublifetime and a
    downstream e-graph engine measured 6.9--10.3% less allocation across
    12 cases, while this package measured nothing at all; and
  * @ebba572@ then put one out-of-line @reviveAlias@ on every scope exit, and
    recorded that no benchmark shipped here resolves it --- @copy-at@ is
    byte-identical with a timing difference inside machine noise,
    @qsort-bench@ takes one @reborrowing'@ per @divideAndConquer'@ call rather
    than per node, and the FFT recursion amortises one exit over
    @O(log n)@ butterfly work.

So the loop below is deliberately the artificial worst case: an L1-resident
counter whose body is small enough that a per-exit call is a visible fraction
of it. That is the only shape in which a per-scope cost is observable, and it
is not a claim about any real workload. Read it as a lower bound on how much a
scope can cost and an early-warning gate on that cost changing, never as a
prediction of what a scope costs an application.

Two conventions keep the arms comparable.

Every arm performs exactly the same mutation the same number of times: one
@'Ref.modify' (+1)@ per iteration for the single-owner arms, and one per owner
for the two-owner arms. The plural arms therefore have their own matched
control rather than being compared against the single-owner one.

The plural arms build their @'Muts'@ bundle once, outside the loop, and thread
it. A bundle rebuilt per iteration would measure @(':-')@ allocation, which is
a real cost of some call sites but not a cost of the delimiter.
-}
module PureBorrow.Bench.ScopeDensity (
  test_scopeDensity,

  -- * Single-owner mutable arms
  scopeFreeLoop,
  reborrowingLoop,
  reborrowingDiscardingLoop,
  reborrowingFinalizingLoop,
  locallyDiscardingLoop,

  -- * Single-owner shared arms
  sharingHoistedLoop,
  sharingPerIterationLoop,

  -- * Two-owner arms
  scopeFreePairLoop,
  bundleThreadedPairLoop,
  reborrowingsDiscardingLoop,
  reborrowingsRespineLoop,
  reborrowingsLoop,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Control.Monad.Borrow.Pure.Experimental.Reborrowable (locally_)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefBorrow
import Prelude.Linear
import Test.Tasty.Bench hiding (defaultMain)
import Prelude qualified as NonLinear

{- $setup
Each arm is 'NOINLINE' and takes its iteration count as an argument, so that a
count chosen by the benchmark driver cannot be constant-folded into the body.
-}

-- | Allocate one owner, borrow it, run @k@, and return the final counter.
withCounter ::
  (forall α. Mut α (Ref.Ref Int) %1 -> Lend α (Ref.Ref Int) %1 -> BO α (After α (Ur Int))) %1 ->
  Int
{-# INLINE withCounter #-}
withCounter k = unur $ linearly \linear -> DataFlow.do
  (runToken, refToken) <- dup linear
  runBO runToken Control.do
    (mut, lend) <- borrowM (Ref.new (0 :: Int) refToken)
    k mut lend

-- | The same with two independent owners.
withCounterPair ::
  ( forall α.
    Mut α (Ref.Ref Int) %1 ->
    Mut α (Ref.Ref Int) %1 ->
    Lend α (Ref.Ref Int) %1 ->
    Lend α (Ref.Ref Int) %1 ->
    BO α (After α (Ur Int))
  ) %1 ->
  Int
{-# INLINE withCounterPair #-}
withCounterPair k = unur $ linearly \linear -> DataFlow.do
  (runToken, ownerToken) <- dup linear
  (leftToken, rightToken) <- dup ownerToken
  runBO runToken Control.do
    (leftMut, leftLend) <- borrowM (Ref.new (0 :: Int) leftToken)
    (rightMut, rightLend) <- borrowM (Ref.new (0 :: Int) rightToken)
    k leftMut rightMut leftLend rightLend

-- | Reclaim one owner and move its final counter out.
finishCounter ::
  Lend α (Ref.Ref Int) %1 ->
  After α (Ur Int)
{-# INLINE finishCounter #-}
finishCounter lend = (move . Ref.free) Control.<$> reclaim' lend

{- | Reclaim both owners and move the sum of their counters out.

The sum, rather than one of the two, so that neither owner's updates can be
dropped as dead.
-}
finishCounterPair ::
  Lend α (Ref.Ref Int) %1 ->
  Lend α (Ref.Ref Int) %1 ->
  After α (Ur Int)
{-# INLINE finishCounterPair #-}
finishCounterPair leftLend rightLend =
  ( \left right -> case (move (Ref.free left), move (Ref.free right)) of
      (Ur left, Ur right) -> Ur (left + right)
  )
    Control.<$> reclaim' leftLend
    Control.<*> reclaim' rightLend

-- | The control: the mutation without a scope, with the borrow threaded by hand.
scopeFreeLoop :: Int -> Int
{-# NOINLINE scopeFreeLoop #-}
scopeFreeLoop iterations = withCounter (go iterations)
  where
    go ::
      forall α.
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i mut lend
      | i <= 0 = consume mut `lseq` Control.pure (finishCounter lend)
      | otherwise = Control.do
          mut <- RefBorrow.modify (+ 1) mut
          go (i - 1) mut lend

-- | One result-discarding mutable scope per iteration.
reborrowingDiscardingLoop :: Int -> Int
{-# NOINLINE reborrowingDiscardingLoop #-}
reborrowingDiscardingLoop iterations = withCounter (go iterations)
  where
    go ::
      forall α.
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i mut lend
      | i <= 0 = consume mut `lseq` Control.pure (finishCounter lend)
      | otherwise = Control.do
          mut <- reborrowing_ mut \scoped ->
            consume Control.<$> RefBorrow.modify (+ 1) scoped
          go (i - 1) mut lend

-- | One result-returning mutable scope per iteration.
reborrowingLoop :: Int -> Int
{-# NOINLINE reborrowingLoop #-}
reborrowingLoop iterations = withCounter (go iterations)
  where
    go ::
      forall α.
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i mut lend
      | i <= 0 = consume mut `lseq` Control.pure (finishCounter lend)
      | otherwise = Control.do
          ((), mut) <- reborrowing mut \scoped ->
            consume Control.<$> RefBorrow.modify (+ 1) scoped
          go (i - 1) mut lend

-- | One finalizing mutable scope per iteration, whose continuation returns an 'After'.
reborrowingFinalizingLoop :: Int -> Int
{-# NOINLINE reborrowingFinalizingLoop #-}
reborrowingFinalizingLoop iterations = withCounter (go iterations)
  where
    go ::
      forall α.
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i mut lend
      | i <= 0 = consume mut `lseq` Control.pure (finishCounter lend)
      | otherwise = Control.do
          ((), mut) <- reborrowing' mut \scoped -> Control.do
            scoped <- RefBorrow.modify (+ 1) scoped
            -- The inner 'Control.pure' is the @'After' β@ applicative's.
            Control.pure (Control.pure (consume scoped))
          go (i - 1) mut lend

{- | The shared control: @n@ reads under a single hoisted 'sharing' scope.

A shared borrow cannot mutate, so the shared arms read rather than write, and
their control cannot be scope-free: 'share' consumes the mutable occurrence, so
the only way to read through a 'Share' and still get the owner back is a scope.
The honest comparison is therefore against the share-once idiom the library
documents — one scope hoisted out of the loop, the 'Share' moved to an
unrestricted occurrence, and @n@ reads inside — which is what this arm is.
-}
sharingHoistedLoop :: Int -> Int
{-# NOINLINE sharingHoistedLoop #-}
sharingHoistedLoop iterations = withCounter \mut lend -> Control.do
  (Ur seen, mut) <- sharing mut \shared ->
    move shared & \(Ur shared) -> go iterations 0 shared
  consume mut `lseq`
    Control.pure ((\(Ur final) -> Ur (final + seen)) Control.<$> finishCounter lend)
  where
    go :: Int -> Int -> Share β (Ref.Ref Int) -> BO β (Ur Int)
    go !i !seen shared
      | i <= 0 = Control.pure (Ur seen)
      | otherwise = Control.do
          Ur observed <- move Control.<$> RefBorrow.copyRef shared
          go (i - 1) (seen + observed) shared

-- | The same @n@ reads, each under its own 'sharing' scope.
sharingPerIterationLoop :: Int -> Int
{-# NOINLINE sharingPerIterationLoop #-}
sharingPerIterationLoop iterations = withCounter (go iterations 0)
  where
    go ::
      forall α.
      Int ->
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i !seen mut lend
      | i <= 0 =
          consume mut `lseq`
            Control.pure ((\(Ur final) -> Ur (final + seen)) Control.<$> finishCounter lend)
      | otherwise = Control.do
          (Ur observed, mut) <- sharing mut \shared ->
            move Control.<$> RefBorrow.copyRef shared
          go (i - 1) (seen + observed) mut lend

-- | One result-discarding scope per iteration, through the generic 'locally_'.
locallyDiscardingLoop :: Int -> Int
{-# NOINLINE locallyDiscardingLoop #-}
locallyDiscardingLoop iterations = withCounter (go iterations)
  where
    go ::
      forall α.
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i mut lend
      | i <= 0 = consume mut `lseq` Control.pure (finishCounter lend)
      | otherwise = Control.do
          mut <- locally_ mut \scoped ->
            consume Control.<$> RefBorrow.modify (+ 1) scoped
          go (i - 1) mut lend

-- | The two-owner control: two mutations per iteration, no scope, no bundle.
scopeFreePairLoop :: Int -> Int
{-# NOINLINE scopeFreePairLoop #-}
scopeFreePairLoop iterations = withCounterPair (go iterations)
  where
    go ::
      forall α.
      Int ->
      Mut α (Ref.Ref Int) %1 ->
      Mut α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i leftMut rightMut leftLend rightLend
      | i <= 0 =
          consume leftMut `lseq`
            consume rightMut `lseq`
              Control.pure (finishCounterPair leftLend rightLend)
      | otherwise = Control.do
          leftMut <- RefBorrow.modify (+ 1) leftMut
          rightMut <- RefBorrow.modify (+ 1) rightMut
          go (i - 1) leftMut rightMut leftLend rightLend

{- | The bundled control: the same two mutations, threaded through a 'Muts', no scope.

'scopeFreePairLoop' threads two separate borrows, so comparing it directly with
a plural arm would charge the delimiter for the bundle as well. This arm
destructures and rebuilds the bundle each iteration and crosses no scope, so
the spine cost and the delimiter cost come apart:
@spine = bundled − direct@, @delimiter = reborrowings_ − direct@.
-}
bundleThreadedPairLoop :: Int -> Int
{-# NOINLINE bundleThreadedPairLoop #-}
bundleThreadedPairLoop iterations =
  withCounterPair \leftMut rightMut leftLend rightLend ->
    go iterations (leftMut :- rightMut :- BNil) leftLend rightLend
  where
    go ::
      forall α.
      Int ->
      Muts α '[Ref.Ref Int, Ref.Ref Int] %1 ->
      Lend α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i bundle leftLend rightLend
      | i <= 0 =
          consume bundle `lseq`
            Control.pure (finishCounterPair leftLend rightLend)
      | otherwise = case bundle of
          leftMut :- rightMut :- BNil -> Control.do
            leftMut <- RefBorrow.modify (+ 1) leftMut
            rightMut <- RefBorrow.modify (+ 1) rightMut
            go (i - 1) (leftMut :- rightMut :- BNil) leftLend rightLend

{- | One result-discarding plural scope per iteration, over a two-member bundle.

The continuation consumes its two members individually rather than rebuilding a
bundle to consume, so this arm measures the delimiter and not the caller's
@(':-')@ cells. 'reborrowingsRespineLoop' is the same arm with the rebuild put
back, and the difference between the two is the respine.
-}
reborrowingsDiscardingLoop :: Int -> Int
{-# NOINLINE reborrowingsDiscardingLoop #-}
reborrowingsDiscardingLoop iterations =
  withCounterPair \leftMut rightMut leftLend rightLend ->
    go iterations (leftMut :- rightMut :- BNil) leftLend rightLend
  where
    go ::
      forall α.
      Int ->
      Muts α '[Ref.Ref Int, Ref.Ref Int] %1 ->
      Lend α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i bundle leftLend rightLend
      | i <= 0 =
          consume bundle `lseq`
            Control.pure (finishCounterPair leftLend rightLend)
      | otherwise = Control.do
          bundle <- reborrowings_ bundle \case
            leftScoped :- rightScoped :- BNil -> Control.do
              leftScoped <- RefBorrow.modify (+ 1) leftScoped
              rightScoped <- RefBorrow.modify (+ 1) rightScoped
              Control.pure (consume leftScoped `lseq` consume rightScoped)
          go (i - 1) bundle leftLend rightLend

-- | 'reborrowingsDiscardingLoop' with the shortened bundle rebuilt before it is consumed.
reborrowingsRespineLoop :: Int -> Int
{-# NOINLINE reborrowingsRespineLoop #-}
reborrowingsRespineLoop iterations =
  withCounterPair \leftMut rightMut leftLend rightLend ->
    go iterations (leftMut :- rightMut :- BNil) leftLend rightLend
  where
    go ::
      forall α.
      Int ->
      Muts α '[Ref.Ref Int, Ref.Ref Int] %1 ->
      Lend α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i bundle leftLend rightLend
      | i <= 0 =
          consume bundle `lseq`
            Control.pure (finishCounterPair leftLend rightLend)
      | otherwise = Control.do
          bundle <- reborrowings_ bundle \case
            leftScoped :- rightScoped :- BNil -> Control.do
              leftScoped <- RefBorrow.modify (+ 1) leftScoped
              rightScoped <- RefBorrow.modify (+ 1) rightScoped
              Control.pure (consume (leftScoped :- rightScoped :- BNil))
          go (i - 1) bundle leftLend rightLend

-- | One result-returning plural scope per iteration, over a two-member bundle.
reborrowingsLoop :: Int -> Int
{-# NOINLINE reborrowingsLoop #-}
reborrowingsLoop iterations =
  withCounterPair \leftMut rightMut leftLend rightLend ->
    go iterations (leftMut :- rightMut :- BNil) leftLend rightLend
  where
    go ::
      forall α.
      Int ->
      Muts α '[Ref.Ref Int, Ref.Ref Int] %1 ->
      Lend α (Ref.Ref Int) %1 ->
      Lend α (Ref.Ref Int) %1 ->
      BO α (After α (Ur Int))
    go !i bundle leftLend rightLend
      | i <= 0 =
          consume bundle `lseq`
            Control.pure (finishCounterPair leftLend rightLend)
      | otherwise = Control.do
          ((), bundle) <- reborrowings bundle \case
            leftScoped :- rightScoped :- BNil -> Control.do
              leftScoped <- RefBorrow.modify (+ 1) leftScoped
              rightScoped <- RefBorrow.modify (+ 1) rightScoped
              Control.pure (consume leftScoped `lseq` consume rightScoped)
          go (i - 1) bundle leftLend rightLend

{- | The iteration counts swept.

Three points, an order of magnitude apart, so that the per-crossing cost comes
out as a slope. One point could not tell a per-crossing cost from the fixture's
own setup.
-}
iterationCounts :: [Int]
iterationCounts = [1024, 16384, 262144]

test_scopeDensity :: [Benchmark]
test_scopeDensity =
  [ bgroup
      "scope-density"
      [ bgroup
          (NonLinear.show iterations)
          [ bgroup
              "mutable"
              [ bench "direct" $ nf scopeFreeLoop iterations
              , bench "reborrowing_" $ nf reborrowingDiscardingLoop iterations
              , bench "reborrowing" $ nf reborrowingLoop iterations
              , bench "reborrowing'" $ nf reborrowingFinalizingLoop iterations
              , bench "locally_" $ nf locallyDiscardingLoop iterations
              ]
          , bgroup
              "shared"
              [ bench "share-once" $ nf sharingHoistedLoop iterations
              , bench "sharing" $ nf sharingPerIterationLoop iterations
              ]
          , bgroup
              "plural"
              [ bench "direct" $ nf scopeFreePairLoop iterations
              , bench "bundled" $ nf bundleThreadedPairLoop iterations
              , bench "reborrowings_" $ nf reborrowingsDiscardingLoop iterations
              , bench "reborrowings_/respine" $ nf reborrowingsRespineLoop iterations
              , bench "reborrowings" $ nf reborrowingsLoop iterations
              ]
          ]
      | iterations <- iterationCounts
      ]
  ]
