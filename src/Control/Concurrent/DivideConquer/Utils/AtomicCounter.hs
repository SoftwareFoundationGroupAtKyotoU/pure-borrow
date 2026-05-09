{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Control.Concurrent.DivideConquer.Utils.AtomicCounter (
  Counter,
  newCounter,
  release,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Unsafe (unsafeSystemIOToBO)
import Control.Monad.Primitive (RealWorld)
import Data.Primitive.PrimVar (PrimVar)
import Data.Primitive.PrimVar qualified as PVar
import GHC.Stack (HasCallStack)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

newtype Counter = Counter (PrimVar RealWorld Int)

newCounter :: (HasCallStack) => Int -> BO α Counter
newCounter capa
  | capa P.<= 0 = error "Counter capacity must be positive"
  | otherwise = Counter Control.<$> unsafeSystemIOToBO (PVar.newPrimVar capa)

-- | Returns 'True' if the counter has reached zero, and 'False' otherwise.
release :: Counter %1 -> BO α Bool
release = Unsafe.toLinear \(Counter var) -> unsafeSystemIOToBO do
  i <- PVar.fetchSubInt var 1
  P.pure $! i P.== 1
