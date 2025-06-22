{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Var.Linear (
  Var,
  new,
  unVar,
  readVar,
  writeVar,
  atomicModify,
  atomicModify_,
) where

import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Data.Var.Linear.Unlifted
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

atomicModify_ :: Var a %1 -> (a %1 -> a) %1 -> Var a
{-# INLINE atomicModify_ #-}
atomicModify_ (Var v) f = Var (atomicModify_# v f)

atomicModify :: Var a %1 -> (a %1 -> (a, b)) %1 -> (Var a, b)
{-# INLINE atomicModify #-}
atomicModify (Var v) f = case atomicModify# v f of
  (# v', b #) -> (Var v', b)

unVar :: Var a %1 -> a
{-# INLINE unVar #-}
unVar (Var v) = unVar# v

readVar :: Var a %1 -> (a, Var a)
{-# INLINE readVar #-}
readVar (Var v) = case readVar# v of
  (# a, v' #) -> (a, Var v')

writeVar :: Var a %1 -> a %1 -> Var a
{-# INLINE writeVar #-}
writeVar (Var v) a = Var (writeVar# v a)
