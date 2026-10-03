{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Kernels that force their 'Linearly' tokens without a bang: the module is compiled with @Strict@.

@Strict@ makes every binding strict and every field of a data type declared here strict, so the tokens bound by the 'DataFlow.do' patterns, and those stored in 'Env', are forced.
With a nullary 'Linearly', 'twoRefsEnv' made one reference out of two.
See Note [Tokens carry a field] in "Control.Monad.Borrow.Pure.Lifetime.Token.Internal".
-}
module Control.Monad.Borrow.Pure.Lifetime.TokenSpec.Strict (twoRefsStrict, twoRefsEnv) where

import Control.Monad.Borrow.Pure (Linearly, linearly)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear qualified as Ref
import Prelude.Linear

-- | Allocate two references holding @seed@, bump the first, and read both; expected @(seed + 1, seed)@.
twoRefsStrict :: Int -> (Int, Int)
{-# NOINLINE twoRefsStrict #-}
twoRefsStrict seed = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r1 <- Ref.new seed l1
  r2 <- Ref.new seed l2
  r1 <- Ref.atomicModify_ (+ 1) r1
  a <- Ref.free r1
  b <- Ref.free r2
  (a, b)

-- | A record of a user's that carries a token, in a strict field.
data Env = Env Linearly Int

mkRef :: Env %1 -> Ref.Ref Int
{-# INLINE mkRef #-}
mkRef (Env l seed) = Ref.new seed l

-- | 'twoRefsStrict' with each reference allocated from a record; expected @(seed + 1, seed)@.
twoRefsEnv :: Int -> (Int, Int)
{-# NOINLINE twoRefsEnv #-}
twoRefsEnv seed = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r1 <- mkRef (Env l1 seed)
  r2 <- mkRef (Env l2 seed)
  r1 <- Ref.atomicModify_ (+ 1) r1
  a <- Ref.free r1
  b <- Ref.free r2
  (a, b)
