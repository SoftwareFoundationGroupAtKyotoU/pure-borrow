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
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Control.Functor.Linear.Extra (Backwards (..), Reverse (..)) where

import Control.Functor.Linear qualified as Control
import Data.Functor.Linear qualified as Data
import GHC.Generics (Generic)
import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear
import Prelude.Linear.Generically (Generically (..))

newtype Backwards f a = Backwards {getBackwards :: f a}

instance (Data.Functor f) => Data.Functor (Backwards f) where
  fmap f (Backwards x) = Backwards (Data.fmap f x)
  {-# INLINE fmap #-}

instance (Control.Functor f) => Control.Functor (Backwards f) where
  fmap f (Backwards x) = Backwards (Control.fmap f x)
  {-# INLINE Control.fmap #-}

instance (Data.Applicative f) => Data.Applicative (Backwards f) where
  pure x = Backwards (Data.pure x)
  {-# INLINE pure #-}
  Backwards f <*> Backwards x = Backwards ((&) Data.<$> x Data.<*> f)
  {-# INLINE (<*>) #-}

instance (Control.Applicative f) => Control.Applicative (Backwards f) where
  pure x = Backwards (Control.pure x)
  {-# INLINE Control.pure #-}
  Backwards f <*> Backwards x = Backwards ((&) Control.<$> x Control.<*> f)
  {-# INLINE (<*>) #-}

newtype Reverse t a = Reverse {getReverse :: t a}
  deriving (Show, Generic)

deriveGenericAnd1 ''Reverse

deriving via
  Generically (Reverse t a)
  instance
    (Consumable (t a)) => Consumable (Reverse t a)

instance (Data.Functor t) => Data.Functor (Reverse t) where
  fmap f (Reverse x) = Reverse (Data.fmap f x)
  {-# INLINE fmap #-}

instance (Data.Traversable t) => Data.Traversable (Reverse t) where
  traverse f (Reverse x) = getBackwards (Reverse Control.<$> Data.traverse (Backwards . f) x)
  {-# INLINE traverse #-}