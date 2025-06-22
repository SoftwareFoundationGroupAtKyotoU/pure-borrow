{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Borrow.Pure.Var (Var (), updateMutVar) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Data.Var.Linear (Var)
import Data.Var.Linear qualified as MutVar

updateMutVar :: (β <= α) => Mut α (Var a) %1 -> (a %1 -> BO β (a, b)) %1 -> BO β (Mut α (Var a), b)
{-# NOINLINE updateMutVar #-}
updateMutVar (UnsafeMut mv) f = Control.do
  -- NOTE: as there is only one reference to @'Var' a@, we can just use read/write
  -- instead of 'MutVar.atomicModify' (which requires pure function) while retaining atomicity.
  (!a, mv) <- Control.pure (MutVar.readVar mv)
  (!a, !b) <- f a
  !mv <- Control.pure (MutVar.writeVar mv a)
  Control.pure (UnsafeMut mv, b)
