{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Unlifted.Linear.Witness (
  allocL,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token
import Data.Array.Mutable.Unlifted.Linear
import GHC.Exts (unsafeCoerce#)
import GHC.Exts qualified as GHC
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

allocL :: Int -> a -> Linearly %1 -> Array# a
{-# ANN allocL "HLint: ignore Avoid lambda" #-}
-- We need 'noinline' here, otherwise GHC will fuse allocL away and
-- unsound allocation can occur when multiple allocation done in serial!
allocL = GHC.noinline \(GHC.I# s) a -> Unsafe.toLinear \_ ->
  GHC.runRW# P.$ \st ->
    case GHC.newArray# s a st of
      (# _, arr #) -> unsafeCoerce# arr
{-# NOINLINE allocL #-} -- prevents runRW# from floating outwards
