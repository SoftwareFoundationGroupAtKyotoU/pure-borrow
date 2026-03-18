{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Concurrent.DivideConquer.Utils.AtomicCounter.Linear (
  Counter,
  withCapacity,
  new,
  increment,
  decrement,
  addToCounter,
  subFromCounter,
) where

import Control.Monad qualified as P
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Data.Word
import Foreign (free)
import Foreign.Marshal.Array
import GHC.Base qualified as GHC
import GHC.Exts
import GHC.IO (noDuplicate, uninterruptibleMask_)
import Prelude.Linear
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

-- | Thread-safe atomic counter.
data Counter
  = Counter
      {- | Counter body.
      Memory layout: | number of duplicated counters | count |
      -}
      {-# UNPACK #-} !(Ptr Word)

instance LinearOnly Counter where
  linearOnly = UnsafeLinearOnly

fetchSubWordOffset :: Ptr Word -> Word -> Int -> IO Word
fetchSubWordOffset ptr (W# val) off =
  let !(Ptr addr#) = ptr `advancePtr` off
   in GHC.IO \s -> do
        case fetchSubWordAddr# addr# val s of
          (# !s, !r #) -> (# s, W# r #)

fetchAddWordOffset :: Ptr Word -> Word -> Int -> IO Word
fetchAddWordOffset ptr (W# val) off =
  let !(Ptr addr#) = ptr `advancePtr` off
   in GHC.IO \s -> do
        case fetchAddWordAddr# addr# val s of
          (# !s, !r #) -> (# s, W# r #)

instance Consumable Counter where
  consume = Unsafe.toLinear \(Counter ptr) ->
    unsafePerformIO $ uninterruptibleMask_ do
      n <- fetchSubWordOffset ptr 1 0
      P.when (n P.== 1) do
        free ptr
  {-# NOINLINE consume #-}

instance Dupable Counter where
  dup2 :: Counter %1 -> (Counter, Counter)
  dup2 = GHC.noinline $ Unsafe.toLinear \c@(Counter ptr) ->
    unsafeStrictPerformIO do
      !_ <- fetchAddWordOffset ptr 1 0
      P.pure (c, c)
  {-# NOINLINE dup2 #-}

unsafeStrictPerformIO :: IO a %1 -> a
{-# INLINE unsafeStrictPerformIO #-}
unsafeStrictPerformIO = Unsafe.toLinear \act -> case runRW# (GHC.unIO act) of
  (# !_, !a #) -> a

new :: Linearly %1 -> Counter
new = withCapacity 0

withCapacity :: Word -> Linearly %1 -> Counter
{-# NOINLINE withCapacity #-}
withCapacity = GHC.noinline \i lin ->
  lin `lseq` unsafeStrictPerformIO do
    !ptr <- newArray [1, i]
    P.pure (Counter ptr)

-- | Atomically increments the counter and returns the previous value.
increment :: Counter %1 -> BO α (Counter, Ur Word)
increment = Unsafe.toLinear \c@(Counter ptr) ->
  unsafeSystemIOToBO do
    i <- fetchAddWordOffset ptr 1 1
    P.pure (c, Ur i)

-- | Atomically decrements the counter and returns the previous value.
decrement :: Counter %1 -> BO α (Counter, Ur Word)
decrement = Unsafe.toLinear \c@(Counter ptr) ->
  unsafeSystemIOToBO do
    i <- fetchSubWordOffset ptr 1 1
    P.pure (c, Ur i)

addToCounter :: Counter %1 -> Word -> BO α (Counter, Ur Word)
addToCounter = Unsafe.toLinear \c@(Counter ptr) val ->
  unsafeSystemIOToBO do
    i <- fetchAddWordOffset ptr val 1
    P.pure (c, Ur $! i + val)

subFromCounter :: Counter %1 -> Word -> BO α (Counter, Ur Word)
subFromCounter = Unsafe.toLinear \c@(Counter ptr) val ->
  unsafeSystemIOToBO do
    i <- fetchSubWordOffset ptr val 1
    P.pure (c, Ur $! i - val)
