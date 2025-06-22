{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Var.Linear.Unlifted (
  Var#,
  newVar#,
  unVar#,
  readVar#,
  writeVar#,
  atomicModify_#,
  atomicModify#,
) where

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

readVar# :: Var# a %1 -> (# a, Var# a #)
{-# INLINE readVar# #-}
readVar# = GHC.noinline $ Unsafe.toLinear \(Var# mv) ->
  runRW# \s ->
    case GHC.readMutVar# mv s of
      (# _, !a #) -> (# a, Var# mv #)

writeVar# :: Var# a %1 -> a %1 -> Var# a
{-# NOINLINE writeVar# #-}
writeVar# = GHC.noinline $ Unsafe.toLinear2 \(Var# mv) !a ->
  runRW# \s ->
    case GHC.writeMutVar# mv a s of
      _ -> Var# mv

unVar# :: Var# a %1 -> a
{-# NOINLINE unVar# #-}
unVar# = Unsafe.toLinear \(Var# a) ->
  runRW# \s ->
    case GHC.readMutVar# a s of
      (# _, !a #) -> a

instance LinearOnly (Var# a) where
  unsafeWithLinear = unsafeLinearOnly

atomicModify_# :: Var# a %1 -> (a %1 -> a) %1 -> Var# a
{-# NOINLINE atomicModify_# #-}
atomicModify_# = GHC.noinline $ Unsafe.toLinear2 \(Var# mv) f ->
  runRW# \s ->
    case GHC.atomicModifyMutVar2# mv (Unsafe.toLinear f) s of
      (# _, !_, !_ #) -> Var# mv

atomicModify# :: Var# a %1 -> (a %1 -> (a, b)) %1 -> (# Var# a, b #)
{-# NOINLINE atomicModify# #-}
atomicModify# = GHC.noinline $ Unsafe.toLinear2 \(Var# mv) f ->
  runRW# \s ->
    case GHC.atomicModifyMutVar2# mv (Unsafe.toLinear f) s of
      (# _, !_, (!_, !b) #) -> (# Var# mv, b #)
