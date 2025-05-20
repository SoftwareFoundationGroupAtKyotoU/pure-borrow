{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Control.Borrow.Pure.Lifetime.Token where

import Control.Borrow.Pure.Lifetime (Lifetime)
import Control.Borrow.Pure.Multiplicity
import Data.Unrestricted.Linear
import GHC.Base (ZeroBitType)

newtype Now (α :: Lifetime) = UnsafeNow (# #)

newtype End (α :: Lifetime) = UnsafeEnd (# #)

instance Affine (Now α) where
  pop (UnsafeNow (# #)) = ()
  {-# INLINE pop #-}

instance Affine (End α) where
  pop (UnsafeEnd (# #)) = ()
  {-# INLINE pop #-}
