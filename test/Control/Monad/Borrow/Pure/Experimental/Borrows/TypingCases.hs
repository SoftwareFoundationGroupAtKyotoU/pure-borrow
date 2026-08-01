{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{- |
Cases that must /not/ typecheck: a lender may never be discarded in safe code.

A 'Lend' is the sole capability to recover the borrowed owner, so dropping one strands that owner forever.
The scalar 'Affine' instance excludes the @\'Lend@ alias kind by construction, and 'Lends' — a bundle of lenders — must behave identically.
Both forms are exercised here so the pair stays symmetric.
-}
module Control.Monad.Borrow.Pure.Experimental.Borrows.TypingCases (
  module Control.Monad.Borrow.Pure.Experimental.Borrows.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Experimental.Borrows
import Prelude.Linear

-- | A bundle of lenders is not 'Affine', so it cannot be popped away.
badLendsAff :: Lends α xs %1 -> ()
badLendsAff bundle = pop (aff bundle)

-- | A bundle of lenders is not 'Consumable' either.
badLendsConsume :: Lends α xs %1 -> ()
badLendsConsume = consume

-- | The scalar reference: a single lender is not 'Affine'.
badLendAff :: Lend α a %1 -> ()
badLendAff lend = pop (aff lend)

-- | The scalar reference: a single lender is not 'Consumable'.
badLendConsume :: Lend α a %1 -> ()
badLendConsume = consume

badLendsAffCase :: Int
badLendsAffCase = discardingLenders badLendsAff

badLendsConsumeCase :: Int
badLendsConsumeCase = discardingLenders badLendsConsume

badLendAffCase :: Int
badLendAffCase = discardingLender badLendAff

badLendConsumeCase :: Int
badLendConsumeCase = discardingLender badLendConsume

{- |
Borrow an owner, throw away the mutable borrow, and hand the resulting one-element
'Lends' bundle to @abandon@ instead of reclaiming through it.
-}
discardingLenders :: (forall α. Lends α '[Int] %1 -> ()) -> Int
discardingLenders abandon =
  linearly \linear ->
    runBO_ linear Control.do
      (mut, lend) <- borrowM (42 :: Int)
      let
        !() = consume mut
        !() = abandon (lend :- BNil)
      Control.pure 0

-- | The scalar counterpart of 'discardingLenders'.
discardingLender :: (forall α. Lend α Int %1 -> ()) -> Int
discardingLender abandon =
  linearly \linear ->
    runBO_ linear Control.do
      (mut, lend) <- borrowM (42 :: Int)
      let
        !() = consume mut
        !() = abandon lend
      Control.pure 0
