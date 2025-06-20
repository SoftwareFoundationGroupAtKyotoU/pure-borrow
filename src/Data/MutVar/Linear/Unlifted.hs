{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.MutVar.Linear.Unlifted (Var#, newVar#, unVar#, atomicModify#) where

import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Utils (lseq#)
import GHC.Exts
import GHC.Exts qualified as GHC
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

newtype Var# a = Var# (MutVar# RealWorld a)

type role Var# nominal

newVar# :: a %1 -> Linearly %1 -> Var# a
{-# NOINLINE newVar# #-}
newVar# = GHC.noinline $ Unsafe.toLinear $ \a lin ->
  lin
    `lseq#` GHC.runRW# \s ->
      case GHC.newMutVar# a s of
        (# _, !v #) -> Var# v

unVar# :: Var# a %1 -> a
{-# NOINLINE unVar# #-}
unVar# = Unsafe.toLinear \(Var# a) ->
  runRW# \s ->
    case GHC.readMutVar# a s of
      (# _, !a #) -> a

instance LinearOnly (Var# a) where
  unsafeWithLinear = unsafeLinearOnly

atomicModify# :: Var# a %1 -> (a %1 -> a) %1 -> Var# a
{-# NOINLINE atomicModify# #-}
atomicModify# = GHC.noinline $ Unsafe.toLinear2 \(Var# mv) f ->
  runRW# \s ->
    case GHC.atomicModifyMutVar_# mv (Unsafe.toLinear f) s of
      (# _, !_, !_ #) -> Var# mv
