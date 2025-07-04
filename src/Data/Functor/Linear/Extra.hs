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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Data.Functor.Linear.Extra (Unzip (..)) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Utils (coerceLin)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Linear (genericTraverse)
import Data.Functor.Linear qualified as Data
import Data.Functor.Product (Product (..))
import Data.List.Linear qualified as LList
import Data.Tuple (Solo)
import Prelude.Linear hiding (Product)
import Prelude.Linear.Generically

class (Data.Functor t) => Unzip t where
  unzip :: t (a, b) %1 -> (t a, t b)

instance Unzip [] where
  unzip = LList.unzip
  {-# INLINE unzip #-}

instance Unzip Maybe where
  unzip Nothing = (Nothing, Nothing)
  unzip (Just (a, b)) = (Just a, Just b)
  {-# INLINE unzip #-}

instance Unzip Identity where
  unzip = coerceLin
  {-# INLINE unzip #-}

instance
  (Data.Traversable f, Data.Traversable g) =>
  Data.Traversable (Product f g)
  where
  traverse f (Pair x y) =
    Pair
      Control.<$> Data.traverse f x
      Control.<*> Data.traverse f y
  {-# INLINE traverse #-}

instance Data.Traversable Identity where
  traverse = genericTraverse
  {-# INLINE traverse #-}

deriving via Generically1 Solo instance Data.Functor Solo

deriving via Generically (Product f g a) instance (Consumable (f a), Consumable (g a)) => Consumable (Product f g a)

instance Data.Traversable Solo where
  traverse = genericTraverse
  {-# INLINE traverse #-}
