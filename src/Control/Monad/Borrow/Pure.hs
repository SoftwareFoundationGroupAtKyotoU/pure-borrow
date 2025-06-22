{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure (
  BO (),
  execBO,
  runBO,
  sexecBO,
  scope_,
  srunBO,
  scope,

  -- * Parallel computation
  parBO,

  -- * Borrows and refernces
  Mut (),
  Share (),
  Lend (),
  borrow,
  share,
  reclaim,
  reborrow,
  Derefable (),
  derefShare,
  genericDerefShare,
  GenericDerefable,

  -- ** Collapsing borrows
  unMutMut,
  unShrMut,
  unMutShr,
  unShrShr,

  -- ** Case-splitting for borrows
  SplittableRef,
  SplittableRefAt,
  AccessibleRef,
  AccessibleRefAt,
  DistributesRef (),
  split,
  GenericDistributesRef,
  genericSplit,

  -- *** Specialized case-splitting for variables
  splitList,
  splitPair,
  splitEither,

  -- * References
  module Control.Monad.Borrow.Pure.Var,

  -- * Re-exports
  module Control.Monad.Borrow.Pure.Lifetime,
  module Control.Monad.Borrow.Pure.Lifetime.Token,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Var
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Proxy (Proxy (..))
import Prelude.Linear

runBO :: (forall α. BO α (End α -> a)) %1 -> Linearly %1 -> a
{-# INLINE runBO #-}
runBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> DataFlow.do
      (now, f) <- execBO bo now
      case endLifetime now of
        Ur end -> f end

-- | Flipped version of 'sexecBO'.
scope_ :: Now α %1 -> BO (α /\ β) a %1 -> BO β (Now α, a)
{-# INLINE scope_ #-}
scope_ = flip sexecBO

srunBO :: (forall α. Proxy α -> BO (α /\ β) (End α -> a)) %1 -> Linearly %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> Control.do
      (now, f) <- sexecBO (bo Proxy) now
      Ur end <- Control.pure (endLifetime now)
      Control.pure (f end)

scope :: Linearly %1 -> (forall α. Proxy α -> BO (α /\ β) (End α -> a)) %1 -> BO β a
scope = flip srunBO
