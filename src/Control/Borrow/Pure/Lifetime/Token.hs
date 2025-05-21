{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Control.Borrow.Pure.Lifetime.Token where

import Control.Borrow.Pure.Affine.Internal
import Control.Borrow.Pure.Lifetime (Lifetime)
import Data.Unrestricted.Linear
import GHC.Base (ZeroBitType)

newtype Now (α :: Lifetime) = UnsafeNow (# #)

newtype End (α :: Lifetime) = UnsafeEnd (# #)

instance Affine (Now a) where
  affinityWitness = UnsafeAssumeAffinity
  {-# INLINE affinityWitness #-}

instance Affine (End a) where
  affinityWitness = UnsafeAssumeAffinity
  {-# INLINE affinityWitness #-}
