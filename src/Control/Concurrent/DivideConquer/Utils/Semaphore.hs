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

module Control.Concurrent.DivideConquer.Utils.Semaphore (
  Semaphore,
  newSemaphore,
  release,
  retain,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Unsafe (unsafeSystemIOToBO)
import Control.Monad.Primitive (RealWorld)
import Data.Functor qualified as P
import Data.Primitive.PrimVar (PrimVar)
import Data.Primitive.PrimVar qualified as PVar
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

data Semaphore a = Semaphore !a {-# UNPACK #-} !(PrimVar RealWorld Int)

-- | Create a semaphore with initial capacity 1.
newSemaphore :: a %1 -> BO α (Semaphore a)
newSemaphore a = Semaphore a Control.<$> unsafeSystemIOToBO (PVar.newPrimVar 1)

-- | Returns 'Just' and original resource if the capacity becomes @0@, otherwise returns 'Nothing'.
release :: Semaphore a %1 -> BO α (Maybe a)
release = Unsafe.toLinear \(Semaphore a var) -> unsafeSystemIOToBO do
  i <- PVar.fetchSubInt var 1
  if i P.== 1
    then P.pure (Just a)
    else P.pure Nothing

retain :: Semaphore a %1 -> BO α (Semaphore a, Semaphore a)
retain = Unsafe.toLinear \sem@(Semaphore _ var) -> unsafeSystemIOToBO do
  P.void $ PVar.fetchAddInt var 1
  P.pure (sem, sem)
