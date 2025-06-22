{-# LANGUAGE MagicHash #-}

module Control.Monad.Borrow.Pure.Utils (
  module Control.Monad.Borrow.Pure.Utils,
) where

import Data.Coerce (Coercible)
import Data.Coerce qualified
import Data.Type.Coercion (Coercion, coerceWith)
import Data.Unrestricted.Linear
import GHC.Base (UnliftedType)
import Unsafe.Linear qualified as Unsafe

coerceLin :: (Coercible a b) => a %1 -> b
{-# INLINE coerceLin #-}
coerceLin = Unsafe.toLinear Data.Coerce.coerce

lseq# :: forall a (s :: UnliftedType). (Consumable a) => a %1 -> s %1 -> s
{-# INLINE lseq# #-}
lseq# a = case consume a of
  () -> \b -> b

coerceWithLin :: Coercion a b %1 -> a %1 -> b
{-# INLINE coerceWithLin #-}
coerceWithLin = Unsafe.toLinear2 coerceWith

infixr 1 >>>

(>>>) :: (a %1 -> b) -> (b %1 -> c) -> a %1 -> c
{-# INLINE (>>>) #-}
(>>>) f g = \x -> g (f x)
