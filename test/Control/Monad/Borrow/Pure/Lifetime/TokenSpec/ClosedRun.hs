{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Runs of 'BO' whose action has no free variables, written with the safe modules alone.

None of these kernels forces a token.
While 'newLifetime' built its 'Now' from constants, such a run depended on nothing but constants once inlined, and GHC made it one top-level value: every kernel here but 'twoMkRefInline' handed out one resource twice, or the same resource on every call.
See Note [Tokens carry a field] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal".

"PureBorrow.NoStateHack.ClosedRun" in the @pure-borrow-no-state-hack@ suite is a copy of this module.
-}
module Control.Monad.Borrow.Pure.Lifetime.TokenSpec.ClosedRun (
  twoMkRef,
  twoMkRefAfter,
  twoMkRefInline,
  twoMkVector,
  callTwiceMkRef,
  callTwiceNewLifetime,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO (execBO)
import Control.Monad.Borrow.Pure.Lifetime.Token (SomeNow (..), endLifetime, newLifetime)
import Data.Ref.Linear qualified as Ref
import Data.Vector.Mutable.Linear.Borrow qualified as V
import Prelude.Linear

readBoth :: Ref.Ref Int %1 -> Ref.Ref Int %1 -> (Int, Int)
readBoth r1 r2 = case Ref.free r1 of
  !a -> case Ref.free r2 of
    !b -> (a, b)

-- | A fresh reference holding 0, allocated by a run of 'BO' that does nothing else.
mkRef :: Linearly %1 -> Ref.Ref Int
{-# NOINLINE mkRef #-}
mkRef lin = runBO_ lin (asksLinearly (Ref.new (0 :: Int)))

-- | Two references from 'mkRef' and two tokens; add @k@ to the first, read both; expected @(k, 0)@.
twoMkRef :: Int -> (Int, Int)
{-# NOINLINE twoMkRef #-}
twoMkRef k = linearly \lin -> case dup2 lin of
  (l1, l2) -> readBoth (Ref.atomicModify_ (+ k) (mkRef l1)) (mkRef l2)

-- | 'mkRef' through 'runBO' and 'pureAfter'.
mkRefAfter :: Linearly %1 -> Ref.Ref Int
{-# NOINLINE mkRefAfter #-}
mkRefAfter lin = runBO lin Control.do
  r <- asksLinearly (Ref.new (0 :: Int))
  pureAfter r

-- | 'twoMkRef' with 'mkRefAfter'; expected @(k, 0)@.
twoMkRefAfter :: Int -> (Int, Int)
{-# NOINLINE twoMkRefAfter #-}
twoMkRefAfter k = linearly \lin -> case dup2 lin of
  (l1, l2) -> readBoth (Ref.atomicModify_ (+ k) (mkRefAfter l1)) (mkRefAfter l2)

-- | 'mkRef', inlined into its callers.
mkRefInline :: Linearly %1 -> Ref.Ref Int
{-# INLINE mkRefInline #-}
mkRefInline lin = runBO_ lin (asksLinearly (Ref.new (0 :: Int)))

-- | 'twoMkRef' with both runs inlined into one body; expected @(k, 0)@.
twoMkRefInline :: Int -> (Int, Int)
{-# NOINLINE twoMkRefInline #-}
twoMkRefInline k = linearly \lin -> case dup2 lin of
  (l1, l2) -> readBoth (Ref.atomicModify_ (+ k) (mkRefInline l1)) (mkRefInline l2)

-- | A fresh vector holding @[0]@, allocated by a run of 'BO' that does nothing else.
mkVector :: Linearly %1 -> V.Vector Int
{-# NOINLINE mkVector #-}
mkVector lin = runBO_ lin (asksLinearly (V.constant 1 (0 :: Int)))

-- | Two vectors from 'mkVector'; set the first one's element to @k@, read both; expected @(k, 0)@.
twoMkVector :: Int -> (Int, Int)
{-# NOINLINE twoMkVector #-}
twoMkVector k = linearly \lin -> case dup2 lin of
  (l1, lin) -> case dup2 lin of
    (l2, l3) -> case mkVector l1 of
      v1 -> case mkVector l2 of
        v2 -> runBO l3 Control.do
          (m1, lend1) <- borrowM v1
          (old, m1) <- V.set 0 k m1
          Control.pure (consume old)
          Ur s1 <- Control.pure (share m1)
          (m2, lend2) <- borrowM v2
          Ur s2 <- Control.pure (share m2)
          Ur a <- V.copyAt 0 s1
          Ur b <- V.copyAt 0 s2
          pureAfter (consume (reclaim lend1) `lseq` consume (reclaim lend2) `lseq` (a, b))

-- | Allocate with 'mkRef', add @k@, and return what it held before.
bumpFresh :: Int -> Int
{-# NOINLINE bumpFresh #-}
bumpFresh k = linearly \lin -> case Ref.atomicModify (\x -> case move x of Ur y -> (y, y + k)) (mkRef lin) of
  (old, r) -> Ref.free r `lseq` old

-- | Call 'bumpFresh' twice; every call should see 0, so @(0, 0)@.
callTwiceMkRef :: (Int, Int)
{-# NOINLINE callTwiceMkRef #-}
callTwiceMkRef = case bumpFresh 7 of
  !a -> case bumpFresh 8 of
    !b -> (a, b)

-- | 'bumpFresh' with the lifetime begun and ended by hand, through 'newLifetime', 'execBO' and 'endLifetime'.
bumpFreshNewLifetime :: Int -> Int
{-# NOINLINE bumpFreshNewLifetime #-}
bumpFreshNewLifetime k = linearly \lin -> case newLifetime lin of
  MkSomeNow now -> case execBO (asksLinearly (Ref.new (0 :: Int))) now of
    (now, r) -> case endLifetime now of
      Ur _ -> case Ref.atomicModify (\x -> case move x of Ur y -> (y, y + k)) r of
        (old, r) -> Ref.free r `lseq` old

-- | Call 'bumpFreshNewLifetime' twice; expected @(0, 0)@.
callTwiceNewLifetime :: (Int, Int)
{-# NOINLINE callTwiceNewLifetime #-}
callTwiceNewLifetime = case bumpFreshNewLifetime 7 of
  !a -> case bumpFreshNewLifetime 8 of
    !b -> (a, b)
