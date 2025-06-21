{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  module Control.Monad.Borrow.Pure.Lifetime.Token.Internal,
) where

import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Internal
import Data.Unrestricted.Linear
import GHC.Stack (HasCallStack)

type role Now nominal

data Now (α :: Lifetime) = UnsafeNow

type role End nominal

data End (α :: Lifetime) = UnsafeEnd

data Linearly = UnsafeLinearly

newtype UnsafeLinearOnly# a = MkUnsafeLinearOnly# (# #)

type LinearOnlyWitness a = (# #) -> UnsafeLinearOnly# a

unsafeLinearOnly :: LinearOnlyWitness a
unsafeLinearOnly = \_ -> MkUnsafeLinearOnly# (# #)

class LinearOnly a where
  unsafeWithLinear :: LinearOnlyWitness a

withLinearly :: (LinearOnly a) => a %1 -> (Linearly, a)
withLinearly !a = (UnsafeLinearly, a)

instance LinearOnly Linearly where
  unsafeWithLinear = unsafeLinearOnly
  {-# INLINE unsafeWithLinear #-}

instance Consumable Linearly where
  consume = \UnsafeLinearly -> ()
  {-# INLINE consume #-}

instance Dupable Linearly where
  dup2 = \UnsafeLinearly -> (UnsafeLinearly, UnsafeLinearly)
  {-# INLINE dup2 #-}

instance Affable Linearly where
  aff UnsafeLinearly = UnsafeAff UnsafeLinearly
  {-# INLINE aff #-}

instance Affable (Now α) where
  aff UnsafeNow = UnsafeAff UnsafeNow
  {-# INLINE aff #-}

instance LinearOnly (Now α) where
  unsafeWithLinear = unsafeLinearOnly
  {-# INLINE unsafeWithLinear #-}

instance Affable (End α) where
  aff UnsafeEnd = UnsafeAff UnsafeEnd
  {-# INLINE aff #-}

instance Consumable (End α) where
  consume UnsafeEnd = ()
  {-# INLINE consume #-}

instance Dupable (End α) where
  dup2 UnsafeEnd = (UnsafeEnd, UnsafeEnd)
  {-# INLINE dup2 #-}

instance Movable (End α) where
  move UnsafeEnd = Ur UnsafeEnd
  {-# INLINE move #-}

instance LinearOnly (End α) where
  unsafeWithLinear = unsafeLinearOnly
  {-# INLINE unsafeWithLinear #-}

endLifetime :: Now (Al i) %1 -> (Ur (End (Al i)))
endLifetime UnsafeNow = Ur UnsafeEnd

data SomeNow where
  MkSomeNow :: Now (Al i) %1 -> SomeNow

alreadyEnded :: (β <= α) => End α %1 -> End β
{-# INLINE alreadyEnded #-}
alreadyEnded = \UnsafeEnd -> UnsafeEnd

occurringNow :: (α <= β) => Now α %1 -> Now β
{-# INLINE occurringNow #-}
occurringNow = \UnsafeNow -> UnsafeNow

newLifetime :: Linearly %1 -> SomeNow
newLifetime UnsafeLinearly = MkSomeNow UnsafeNow

-- | Static Lifetime is always available
nowStatic :: Now Static
nowStatic = UnsafeNow

-- | Static lifetime lasts forever
absurdEndStatic :: (HasCallStack) => End Static %1 -> a
absurdEndStatic UnsafeEnd = error "Unreachable: if you see this, you created an End Static in the internal code!"
