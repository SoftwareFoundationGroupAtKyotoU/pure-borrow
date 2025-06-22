{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Borrow.Pure.Var (
  Var (),
  updateMutVar,
  swapMutVar,
  readSharedVar,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Var.Linear (Var)
import Data.Var.Linear qualified as MutVar
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

updateMutVar :: (β <= α) => Mut α (Var a) %1 -> (a %1 -> BO β (a, b)) %1 -> BO β (Mut α (Var a), b)
{-# INLINE updateMutVar #-}
updateMutVar (UnsafeMut mv) f = DataFlow.do
  -- NOTE: as there is only one reference to @'Var' a@, we can just use read/write
  -- instead of 'MutVar.atomicModify' (which requires pure function) while retaining atomicity.
  (a, mv) <- MutVar.readVar mv
  f a Control.<&> \(!a, !b) -> DataFlow.do
    mv <- MutVar.writeVar mv a
    (UnsafeMut mv, b)

swapMutVar :: (β <= α) => Mut α (Var a) %1 -> Mut α (Var a) %1 -> BO β (Mut α (Var a), Mut α (Var a))
{-# INLINE swapMutVar #-}
swapMutVar ma ma' = updateMutVar ma \a -> Control.do
  (ma', a') <- updateMutVar ma' \a' -> Control.pure (a, a')
  Control.pure (a', ma')

readSharedVar :: (β <= α) => Share α (Var a) %1 -> BO β (Share α a)
{-# INLINE readSharedVar #-}
readSharedVar = Unsafe.toLinear \(UnsafeShare mv) ->
  Control.pure $ UnsafeShare NonLinear.$ NonLinear.fst $ MutVar.readVar mv
