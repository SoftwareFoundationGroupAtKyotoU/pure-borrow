{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Kernels that force a 'Linearly' token before allocating with it, written with the safe modules alone.

Were 'Linearly' a nullary constructor, forcing a token would tell GHC which value it is.
Two allocations with equal arguments would then be merged into one, and an allocation whose arguments are all constants would be floated out and shared by every call.
With a nullary token, every kernel here but 'twoRefsUnforced' failed at @-O2@, and 'twoVectors' failed only under @-fno-state-hack@.
See Note [Tokens carry a field] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal".

"PureBorrow.NoStateHack.ForcedLinearly" in the @pure-borrow-no-state-hack@ suite is a copy of this module.
-}
module Control.Monad.Borrow.Pure.Lifetime.TokenSpec.Forced (
  twoRefs,
  twoRefsUnforced,
  callTwiceRef,
  callTwiceVector,
  twoVectors,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear qualified as Ref
import Data.Vector.Mutable.Linear.Borrow qualified as V
import Prelude.Linear

{- | Allocate two references holding @seed@ from two forced tokens, bump the first, and read both.

Expected @(seed + 1, seed)@; one shared reference gives @(seed + 1, seed + 1)@.
-}
twoRefs :: Int -> (Int, Int)
{-# NOINLINE twoRefs #-}
twoRefs seed = linearly \lin -> case dup2 lin of
  (!l1, !l2) -> readBoth (Ref.atomicModify_ (+ 1) (Ref.new seed l1)) (Ref.new seed l2)

-- | 'twoRefs' without forcing the tokens.
twoRefsUnforced :: Int -> (Int, Int)
{-# NOINLINE twoRefsUnforced #-}
twoRefsUnforced seed = linearly \lin -> case dup2 lin of
  (l1, l2) -> readBoth (Ref.atomicModify_ (+ 1) (Ref.new seed l1)) (Ref.new seed l2)

readBoth :: Ref.Ref Int %1 -> Ref.Ref Int %1 -> (Int, Int)
readBoth r1 r2 = case Ref.free r1 of
  !a -> case Ref.free r2 of
    !b -> (a, b)

{- | Allocate a reference holding 0 from a forced token, add @k@ to it, and return what it held before.

Every call should see 0; one reference shared by every call makes the second call see the first call's total.
-}
freshRef :: Int -> Int
{-# NOINLINE freshRef #-}
freshRef k = linearly \lin -> DataFlow.do
  r <- Ref.new (0 :: Int) $! lin
  (old, r) <- Ref.atomicModify (\x -> case move x of Ur y -> (y, y + k)) r
  Ref.free r `lseq` old

-- | Call 'freshRef' twice; expected @(0, 0)@.
callTwiceRef :: (Int, Int)
{-# NOINLINE callTwiceRef #-}
callTwiceRef = case freshRef 7 of
  !a -> case freshRef 8 of
    !b -> (a, b)

-- | 'freshRef' for a vector of one element, allocated from a forced token.
freshVector :: Int -> Int
{-# NOINLINE freshVector #-}
freshVector k = linearly \lin -> case dup2 lin of
  (!l1, l2) -> DataFlow.do
    v <- V.constant 1 (0 :: Int) l1
    runBO l2 Control.do
      (m, lend) <- borrowM v
      (old, m) <- V.set 0 k m
      Control.pure (consume m)
      pureAfter (consume (reclaim lend) `lseq` old)

-- | Call 'freshVector' twice; expected @(0, 0)@.
callTwiceVector :: (Int, Int)
{-# NOINLINE callTwiceVector #-}
callTwiceVector = case freshVector 7 of
  !a -> case freshVector 8 of
    !b -> (a, b)

{- | Allocate two vectors holding @[seed]@ from two forced tokens, set the first one's element to @seed + 1@, and read both.

Expected @(seed + 1, seed)@.
-}
twoVectors :: Int -> (Int, Int)
{-# NOINLINE twoVectors #-}
twoVectors seed = linearly \lin -> case dup2 lin of
  (!l1, lin) -> case dup2 lin of
    (!l2, l3) -> DataFlow.do
      v1 <- V.constant 1 seed l1
      v2 <- V.constant 1 seed l2
      runBO l3 Control.do
        (m1, lend1) <- borrowM v1
        (old, m1) <- V.set 0 (seed + 1) m1
        Control.pure (consume old)
        Ur s1 <- Control.pure (share m1)
        (m2, lend2) <- borrowM v2
        Ur s2 <- Control.pure (share m2)
        Ur a <- V.copyAt 0 s1
        Ur b <- V.copyAt 0 s2
        pureAfter (consume (reclaim lend1) `lseq` consume (reclaim lend2) `lseq` (a, b))
