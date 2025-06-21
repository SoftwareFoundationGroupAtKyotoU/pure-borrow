{-# LANGUAGE MagicHash #-}

module Control.Monad.Borrow.Pure.Utils (
  module Control.Monad.Borrow.Pure.Utils,
) where

import Data.Coerce (Coercible)
import Data.Coerce qualified
import Data.Unrestricted.Linear
import GHC.Base (UnliftedType)
import Unsafe.Linear qualified as Unsafe

coerceLin :: (Coercible a b) => a %1 -> b
coerceLin = Unsafe.toLinear Data.Coerce.coerce

lseq# :: forall a (s :: UnliftedType). (Consumable a) => a %1 -> s %1 -> s
lseq# a = case consume a of
  () -> \b -> b
