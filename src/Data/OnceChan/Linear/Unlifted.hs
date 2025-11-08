{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.OnceChan.Linear.Unlifted (
  Sink#,
  Source#,
  new#,
  put#,
  take#,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import GHC.Exts qualified as GHC
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

newtype Source# a = Source# (GHC.MVar# GHC.RealWorld a)

newtype Sink# a = Sink# (GHC.MVar# GHC.RealWorld a)

type role Source# representational

type role Sink# nominal

new# :: Linearly %1 -> (# Sink# a, Source# a #)
{-# NOINLINE new# #-}
new# = GHC.noinline $ Unsafe.toLinear $ \_ ->
  GHC.runRW# \s ->
    case GHC.newMVar# s of
      (# _, !v #) -> (# Sink# v, Source# v #)

take# :: Source# a %1 -> a
{-# INLINE take# #-}
take# = GHC.noinline $ Unsafe.toLinear \(Source# mv) ->
  GHC.runRW# \s ->
    case GHC.takeMVar# mv s of
      (# _, !a #) -> a

put# :: Sink# a %1 -> a %1 -> ()
{-# NOINLINE put# #-}
put# = GHC.noinline $ Unsafe.toLinear2 \(Sink# mv) !a ->
  GHC.runRW# \s ->
    case GHC.putMVar# mv a s of
      !_ -> ()

instance LinearOnly (Sink# a) where
  linearOnly = UnsafeLinearOnly

instance LinearOnly (Source# a) where
  linearOnly = UnsafeLinearOnly
