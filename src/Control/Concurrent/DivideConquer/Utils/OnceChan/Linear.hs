{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Concurrent.DivideConquer.Utils.OnceChan.Linear (
  Sink,
  Source,
  new,
  put,
  take,
) where

import Control.Concurrent.DivideConquer.Utils.OnceChan.Linear.Unlifted
import Control.Monad.Borrow.Affine
import Control.Monad.Borrow.Affine.Unsafe (unsafeAff)
import Control.Monad.Borrow.BO
import Control.Monad.Borrow.Lifetime.Token.Unsafe (
  LinearOnly (..),
  LinearOnlyWitness (..),
 )
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
  linearOnly = UnsafeLinearOnly

instance LinearOnly (Source a) where
  linearOnly = UnsafeLinearOnly

instance Affine (Sink a) where
  aff = unsafeAff

instance Consumable (Sink a) where
  consume = Unsafe.toLinear \(Sink !_) -> ()

instance Consumable (Source a) where
  consume = Unsafe.toLinear \(Source !_) -> ()

instance Affine (Source a) where
  aff = unsafeAff

take :: forall a α w. Source a %1 -> BO' w α a
{-# INLINE take #-}
take (Source v) = evaluateBO $ take# v

put :: forall a α w. Sink a %1 -> a %1 -> BO' w α ()
{-# INLINE put #-}
put (Sink v) !a = evaluateBO $ put# v a
