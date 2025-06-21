{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure (
  BO (),
  execBO,
  runBO,
  sexecBO,
  srunBO,

  -- * Parallel computation
  parBO,

  -- * Re-exports
  module Control.Monad.Borrow.Pure.Lifetime,
  module Control.Monad.Borrow.Pure.Lifetime.Token,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Unrestricted.Linear (Ur (..))

runBO :: (forall α. BO α (End α -> a)) %1 -> Linearly %1 -> a
{-# INLINE runBO #-}
runBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> DataFlow.do
      (now, f) <- execBO bo now
      case endLifetime now of
        Ur end -> f end

srunBO :: (forall α. BO (α /\ β) (End α -> a)) %1 -> Linearly %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> Control.do
      (now, f) <- sexecBO bo now
      Ur end <- Control.pure (endLifetime now)
      Control.pure (f end)
