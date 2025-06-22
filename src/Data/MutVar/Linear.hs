{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.MutVar.Linear (Var, new, atomicModify, unVar) where

import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Data.MutVar.Linear.Unlifted
import Unsafe.Linear qualified as Unsafe

data Var a = Var (Var# a)

type role Var nominal

new :: a %1 -> Linearly %1 -> Var a
{-# INLINE new #-}
new a lin = Var (newVar# a lin)

instance LinearOnly (Var a) where
  unsafeWithLinear = unsafeLinearOnly

instance Affable (Var a) where
  aff = Unsafe.toLinear UnsafeAff

atomicModify :: Var a %1 -> (a %1 -> a) %1 -> Var a
{-# INLINE atomicModify #-}
atomicModify (Var v) f = Var (atomicModify# v f)

unVar :: Var a %1 -> a
{-# INLINE unVar #-}
unVar (Var v) = unVar# v
