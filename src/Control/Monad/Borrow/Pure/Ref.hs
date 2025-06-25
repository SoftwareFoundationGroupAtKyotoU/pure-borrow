{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Monad.Borrow.Pure.Ref (
  Ref (),
  updateRef,
  swapRef,
  readSharedRef,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Internal
import Control.Monad.Borrow.Pure.Lifetime
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

updateRef :: (β <= α) => Mut α (Ref a) %1 -> (a %1 -> BO β (a, b)) %1 -> BO β (Mut α (Ref a), b)
{-# INLINE updateRef #-}
updateRef (UnsafeMut mv) f = DataFlow.do
  -- NOTE: as there is only one reference to @'Ref' a@, we can just use read/write
  -- instead of 'MutVar.atomicModify' (which requires pure function) while retaining atomicity.
  (a, mv) <- Ref.readRef mv
  f a Control.<&> \(!a, !b) -> DataFlow.do
    mv <- Ref.writeRef mv a
    (UnsafeMut mv, b)

swapRef :: (β <= α) => Mut α (Ref a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a), Mut α (Ref a))
{-# INLINE swapRef #-}
swapRef ma ma' = updateRef ma \a -> Control.do
  (ma', a') <- updateRef ma' \a' -> Control.pure (a, a')
  Control.pure (a', ma')

readSharedRef :: (β <= α) => Share α (Ref a) %1 -> BO β (Share α a)
{-# INLINE readSharedRef #-}
readSharedRef = Unsafe.toLinear \(UnsafeShare mv) ->
  Control.pure $ UnsafeShare NonLinear.$ NonLinear.fst $ Ref.readRef mv
