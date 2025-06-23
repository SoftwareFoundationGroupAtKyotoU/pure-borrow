{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.OnceChan.Linear (
  Sink,
  Source,
  new,
  put,
  take,
) where

import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Data.OnceChan.Linear.Unlifted
import Data.Unrestricted.Linear
import Prelude.Linear hiding (take)
import Unsafe.Linear qualified as Unsafe

data Sink a = Sink (Sink# a)

data Source a = Source (Source# a)

type role Sink nominal

type role Source representational

new :: Linearly %1 -> (Sink a, Source a)
{-# INLINE new #-}
new lin = case new# lin of
  (# sink, source #) -> (Sink sink, Source source)

instance LinearOnly (Sink a) where
  unsafeWithLinear = unsafeLinearOnly

instance LinearOnly (Source a) where
  unsafeWithLinear = unsafeLinearOnly

instance Affable (Sink a) where
  aff = Unsafe.toLinear UnsafeAff

instance Consumable (Sink a) where
  consume = Unsafe.toLinear \(Sink !_) -> ()

instance Consumable (Source a) where
  consume = Unsafe.toLinear \(Source !_) -> ()

instance Affable (Source a) where
  aff = Unsafe.toLinear UnsafeAff

take :: Source a %1 -> BO α a
{-# INLINE take #-}
take (Source v) = evaluate $ take# v

put :: Sink a %1 -> a %1 -> BO α ()
{-# INLINE put #-}
put (Sink v) !a = evaluate $ put# v a
