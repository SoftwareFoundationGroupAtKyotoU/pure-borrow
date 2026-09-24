{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Monad.Borrow.Pure.Utils (
  module Control.Monad.Borrow.Pure.Utils,
) where

import Data.Coerce (Coercible)
import Data.Coerce qualified
import Data.Type.Coercion (Coercion, coerceWith)
import Data.Unrestricted.Linear
import Data.Vector.Fusion.Bundle qualified as Bundle
import GHC.Base (UnliftedType)
import GHC.Exts (lazy)
import GHC.IO (IO (..))
import Unsafe.Linear qualified as Unsafe

coerceLin :: (Coercible a b) => a %1 -> b
{-# INLINE coerceLin #-}
coerceLin = Unsafe.toLinear Data.Coerce.coerce

{- | Drop a linearly bound value without consuming it.

This is for an /alias/ of a resource that some other owner is still
responsible for: consuming it would claim an ownership this scope does not
have, and holding it is impossible where the surrounding function has to
return. Every use is a proof obligation that the value really is an alias,
and that dropping it releases nothing -- otherwise it is exactly a leak.
-}
unsafeLeak :: a %1 -> ()
{-# INLINE unsafeLeak #-}
unsafeLeak = Unsafe.toLinear (\ !_ -> ())

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

{- | Force a stored value to WHNF inside an already guarded BO computation.
This is ordinary strict evaluation, not a precise exception-ordering primitive.
See Note [Demand stays inside a BO run] in "Control.Monad.Borrow.Pure.BO.Internal".
-}
evaluateStored :: a -> IO a
{-# INLINE evaluateStored #-}
evaluateStored a = IO \s -> case lazy a of !evaluated -> (# s, evaluated #)

{- | A single-use construction stream that evaluates each element before yielding it.
Its consumer must run under the constructor's guarded evaluation.
-}
evaluatingBundle :: [a] -> Bundle.Bundle v a
{-# INLINE evaluatingBundle #-}
evaluatingBundle = Bundle.unfoldr step
  where
    step [] = Nothing
    step (element : rest) = element `seq` Just (element, rest)
