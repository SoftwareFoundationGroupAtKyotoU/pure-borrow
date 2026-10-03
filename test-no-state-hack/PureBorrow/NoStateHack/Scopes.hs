{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -O2 -fno-state-hack -ffull-laziness #-}

{- | Owners reclaimed in a scope's 'After', built with @-fno-state-hack@ (the round-3 ownership review's probe).

With 'Control.Monad.Borrow.Pure.Lifetime.Token.withEnd' strict in a nullary 'Control.Monad.Borrow.Pure.Lifetime.Token.EndToken', the 'srunBO' kernels here returned 0 and 'ownerGivenAway' handed back a freed reference.

Every kernel bumps a reference through a borrow and reads the reclaimed owner in the scope's 'After'; the scope's result is then forced before more effects run, so that the 'After' is not applied in tail position.
Each is expected to return 1, and 'ownerTwice' @(100, 1)@.

With the state hack on, GHC never floats out of the state-threaded lambdas, so what keeps the read after the bump is its lexical position.
With @-fno-state-hack@ the lambdas are ordinary value lambdas, and only a data dependency on the scope's end keeps the read after the bump.
Only safe modules are imported.
-}
module PureBorrow.NoStateHack.Scopes (kernels, ownerTwice, ownerGivenAway) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO (evaluateBO)
import Control.Monad.Borrow.Pure.Experimental.Borrows qualified as Borrows
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import Prelude.Linear
import Prelude qualified as NonLinear

bump :: Mut α (Ref.Ref Int) %1 -> BO α ()
bump m = consume Control.<$> RefB.modify (+ 1) m

-- | The After is applied in tail position (a join point): expected to pass either way.
viaRunBO :: Int -> Int
{-# NOINLINE viaRunBO #-}
viaRunBO seed = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r <- Ref.new seed l1
  runBO l2 Control.do
    (m, lend) <- borrowM r
    bump m
    pureAfter (Ref.free (reclaim lend))

-- | srunBO forces its After at scope exit ('$!'), and execBO's 'reviveNow' follows it.
viaSrunBO :: Int -> Int
{-# NOINLINE viaSrunBO #-}
viaSrunBO seed = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r <- Ref.new seed l1
  runBO l2 Control.do
    x <- srunBO Control.do
      (m, lend) <- borrowM r
      bump m
      Control.pure (upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    pureAfter x

-- | reborrowing', result forced right after the scope, then another effect.
viaReborrowing' :: Int -> Int
{-# NOINLINE viaReborrowing' #-}
viaReborrowing' seed = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r <- Ref.new seed l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    (x, md) <- reborrowing' md \md' -> Control.do
      (m, lend) <- borrowM r
      bump m
      Control.pure (consume md' `lseq` upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    !x <- evaluateBO x
    bump md
    pureAfter (consume (reclaim lendD) `lseq` x)

-- | sharing', the same.
viaSharing' :: Int -> Int
{-# NOINLINE viaSharing' #-}
viaSharing' seed = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r <- Ref.new seed l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    (x, md) <- sharing' md \_ -> Control.do
      (m, lend) <- borrowM r
      bump m
      Control.pure (upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    !x <- evaluateBO x
    bump md
    pureAfter (consume (reclaim lendD) `lseq` x)

-- | reborrowings', the same.
viaReborrowings' :: Int -> Int
{-# NOINLINE viaReborrowings' #-}
viaReborrowings' seed = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r <- Ref.new seed l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    (x, bundle) <- Borrows.reborrowings' (md Borrows.:- Borrows.BNil) \(md' Borrows.:- Borrows.BNil) -> Control.do
      (m, lend) <- borrowM r
      bump m
      Control.pure (consume md' `lseq` upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    case bundle of
      md Borrows.:- Borrows.BNil -> Control.do
        !x <- evaluateBO x
        bump md
        pureAfter (consume (reclaim lendD) `lseq` x)

-- | srunBO followed by more effects in the same run.
viaSrunBOThenEffect :: Int -> Int
{-# NOINLINE viaSrunBOThenEffect #-}
viaSrunBOThenEffect seed = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r <- Ref.new seed l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    x <- srunBO Control.do
      (m, lend) <- borrowM r
      bump m
      Control.pure (upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    bump md
    pureAfter (consume (reclaim lendD) `lseq` x)

viaModifyBO :: Int -> Int
{-# NOINLINE viaModifyBO #-}
viaModifyBO seed = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r <- Ref.new seed l1
  Ref.free (modifyBO_ r l2 bump)

kernels :: [(NonLinear.String, Int -> Int)]
kernels =
  [ ("runBO", viaRunBO)
  , ("srunBO", viaSrunBO)
  , ("srunBO then effect", viaSrunBOThenEffect)
  , ("reborrowing' forced", viaReborrowing')
  , ("sharing' forced", viaSharing')
  , ("reborrowings' forced", viaReborrowings')
  , ("modifyBO_", viaModifyBO)
  ]

{- | Take the inner reference out of the outer one inside a 'srunBO' scope and put a fresh one in, and read the reclaimed outer reference in the scope's 'After'.

Expected @(100, 1)@: the reclaimed outer reference holds the fresh inner one, and the taken one is bumped separately.
A stale read hands the taken reference back a second time, @(1, 1)@: two owners of one reference.
-}
ownerTwice :: Int -> (Int, Int)
{-# NOINLINE ownerTwice #-}
ownerTwice seed = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, lin) <- dup lin
  (l3, l4) <- dup lin
  inner <- Ref.new seed l1
  outer <- Ref.new inner l2
  runBO l3 Control.do
    (taken, current) <- srunBO Control.do
      (m, lend) <- borrowM outer
      fresh <- asksLinearly (Ref.new (100 :: Int))
      (taken, m) <- RefB.update (\old -> Control.pure (old, fresh)) m
      Control.pure (consume m)
      Control.pure (upcast @_ @(After _ (Ref.Ref Int, Ref.Ref Int)) (keep taken Control.<$> reclaim' lend))
    pureAfter (observe taken current l4)
  where
    -- Read the reclaimed outer reference strictly, so that srunBO's '$!' performs the read.
    keep :: Ref.Ref Int %1 -> Ref.Ref (Ref.Ref Int) %1 -> (Ref.Ref Int, Ref.Ref Int)
    keep taken outer' = case Ref.free outer' of !cur -> (taken, cur)

{- | The same, with the taken reference bumped and freed inside the scope, so that the 'After' depends on nothing the scope produced.

Expected 100: the reclaimed outer reference holds the fresh inner one.
A stale read hands back the inner reference the scope took out and freed, 1.
-}
ownerGivenAway :: Int -> Int
{-# NOINLINE ownerGivenAway #-}
ownerGivenAway seed = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  inner <- Ref.new seed l1
  outer <- Ref.new inner l2
  runBO l3 Control.do
    current <- srunBO Control.do
      (m, lend) <- borrowM outer
      fresh <- asksLinearly (Ref.new (100 :: Int))
      (taken, m) <- RefB.update (\old -> Control.pure (old, fresh)) m
      Control.pure (consume m)
      -- Bump the taken reference in a run of its own and free it, inside the scope.
      taken <- asksLinearly \l -> modifyBO_ taken l bump
      Control.pure (consume taken)
      Control.pure (upcast @_ @(After _ (Ref.Ref Int)) (readOuter Control.<$> reclaim' lend))
    pureAfter (Ref.free current)
  where
    -- Read the reclaimed outer reference strictly, so that srunBO's '$!' performs the read.
    readOuter :: Ref.Ref (Ref.Ref Int) %1 -> Ref.Ref Int
    readOuter outer' = case Ref.free outer' of !cur -> cur

observe :: Ref.Ref Int %1 -> Ref.Ref Int %1 -> Linearly %1 -> (Int, Int)
{-# INLINE observe #-}
observe taken current l = runBO l Control.do
  (mt, lendT) <- borrowM taken
  mt <- RefB.modify (+ 1) mt
  Control.pure (consume mt)
  (mc, lendC) <- borrowM current
  c <- RefB.copyRef mc
  pureAfter (consume (reclaim lendC) `lseq` (c, Ref.free (reclaim lendT)))
