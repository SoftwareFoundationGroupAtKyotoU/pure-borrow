{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Control.Concurrent.DivideConquer.Linear.Types (ThreadId_ (..), Thread (..), wait, Pair (..)) where

import Control.Concurrent (ThreadId)
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Affine (Affable, GenericallyAffable (..))
import Data.Functor.Linear qualified as Data
import Data.Functor.Linear.Extra
import Data.Functor.Linear.Extra qualified as Data
import Data.OnceChan.Linear (Source)
import Data.OnceChan.Linear qualified as Once
import Data.Unrestricted.Linear (AsMovable (..))
import GHC.Generics qualified as GHC
import Generics.Linear.TH (deriveGeneric, deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically, Generically1)
import Unsafe.Linear qualified as Unsafe

newtype ThreadId_ = ThreadId_ ThreadId
  deriving stock (GHC.Generic)
  deriving (Consumable, Dupable) via AsMovable ThreadId_

instance Movable ThreadId_ where
  move = Unsafe.toLinear Ur

wait :: Thread %1 -> BO α ()
wait (Thread tid source) = tid `lseq` Once.take source

data Thread = Thread !ThreadId_ !(Source ())
  deriving stock (GHC.Generic)

deriveGeneric ''Thread

deriving via Generically Thread instance Consumable Thread

data Pair a where
  Pair :: !a %1 -> !a %1 -> Pair a
  deriving (GHC.Generic, GHC.Generic1)

deriveGenericAnd1 ''Pair

instance Data.Unzip Pair where
  unzip (Pair (a1, b1) (a2, b2)) = (Pair a1 a2, Pair b1 b2)
  {-# INLINE unzip #-}

deriving via Generically1 Pair instance Data.Functor Pair

deriving via
  Generically (Pair a)
  instance
    (Consumable a) => Consumable (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Dupable a) => Dupable (Pair a)

deriving via
  GenericallyAffable (Pair a)
  instance
    (Affable a) => Affable (Pair a)

deriving via
  Generically (Pair a)
  instance
    (Movable a) => Movable (Pair a)

instance Data.Traversable Pair where
  traverse = Data.genericTraverse
  {-# INLINE traverse #-}
