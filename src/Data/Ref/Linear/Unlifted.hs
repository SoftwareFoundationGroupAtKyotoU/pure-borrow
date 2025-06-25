{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Ref.Linear.Unlifted (
  Ref#,
  newRef#,
  unRef#,
  readRef#,
  writeRef#,
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

newtype Ref# a = Ref# (MutVar# RealWorld a)

type role Ref# nominal

newRef# :: a %1 -> Linearly %1 -> Ref# a
{-# NOINLINE newRef# #-}
newRef# = GHC.noinline $ Unsafe.toLinear $ \a lin ->
  lin
    `lseq#` GHC.runRW# \s ->
      case GHC.newMutVar# a s of
        (# _, !v #) -> Ref# v

readRef# :: Ref# a %1 -> (# a, Ref# a #)
{-# INLINE readRef# #-}
readRef# = GHC.noinline $ Unsafe.toLinear \(Ref# mv) ->
  runRW# \s ->
    case GHC.readMutVar# mv s of
      (# _, !a #) -> (# a, Ref# mv #)

writeRef# :: Ref# a %1 -> a %1 -> Ref# a
{-# NOINLINE writeRef# #-}
writeRef# = GHC.noinline $ Unsafe.toLinear2 \(Ref# mv) !a ->
  runRW# \s ->
    case GHC.writeMutVar# mv a s of
      _ -> Ref# mv

unRef# :: Ref# a %1 -> a
{-# NOINLINE unRef# #-}
unRef# = Unsafe.toLinear \(Ref# a) ->
  runRW# \s ->
    case GHC.readMutVar# a s of
      (# _, !a #) -> a

instance LinearOnly (Ref# a) where
  unsafeWithLinear = unsafeLinearOnly

atomicModify_# :: Ref# a %1 -> (a %1 -> a) %1 -> Ref# a
{-# NOINLINE atomicModify_# #-}
atomicModify_# = GHC.noinline $ Unsafe.toLinear2 \(Ref# mv) f ->
  runRW# \s ->
    case GHC.atomicModifyMutVar2# mv (Unsafe.toLinear f) s of
      (# _, !_, !_ #) -> Ref# mv

atomicModify# :: Ref# a %1 -> (a %1 -> (a, b)) %1 -> (# Ref# a, b #)
{-# NOINLINE atomicModify# #-}
atomicModify# = GHC.noinline $ Unsafe.toLinear2 \(Ref# mv) f ->
  runRW# \s ->
    case GHC.atomicModifyMutVar2# mv (Unsafe.toLinear f) s of
      (# _, !_, (!_, !b) #) -> (# Ref# mv, b #)
