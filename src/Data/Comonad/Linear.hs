{-# LANGUAGE DerivingStrategies #-}

module Data.Comonad.Linear (Comonad (..), ComonadApply (..)) where

import Data.Functor.Linear qualified as Data
import Data.Unrestricted.Linear (Ur (..))

class (Data.Functor w) => Comonad w where
  extract :: w a %1 -> a
  duplicate :: w a %1 -> w (w a)

instance Comonad Ur where
  extract (Ur a) = a
  {-# INLINE extract #-}

  duplicate (Ur a) = Ur (Ur a)
  {-# INLINE duplicate #-}

infixl 4 <@>

class (Comonad w) => ComonadApply w where
  (<@>) :: w (a %1 -> b) %1 -> w a %1 -> w b

instance ComonadApply Ur where
  (Ur f) <@> (Ur a) = Ur (f a)
  {-# INLINE (<@>) #-}
