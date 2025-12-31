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
  modifyRef,
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

updateRef :: (β <= α) => (a %1 -> BO β (b, a)) %1 -> Mut α (Ref a) %1 -> BO β (b, Mut α (Ref a))
{-# INLINE updateRef #-}
updateRef f (UnsafeAlias mv) = DataFlow.do
  -- NOTE: as there is only one reference to @'Ref' a@, we can just use read/write
  -- instead of 'MutVar.atomicModify' (which requires pure function) while retaining atomicity.
  (!a, !mv) <- Ref.unsafeReadRef mv
  f a Control.<&> \(!b, !a) -> DataFlow.do
    !mv <- Ref.unsafeWriteRef mv a
    (b, UnsafeAlias mv)

modifyRef :: (β <= α) => (a %1 -> a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a))
modifyRef f ma = Control.do
  ((), ma) <- updateRef (Control.pure . ((),) . f) ma
  Control.pure ma

swapRef :: (β <= α) => Mut α (Ref a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a), Mut α (Ref a))
{-# INLINE swapRef #-}
swapRef ma ma' =
  flip updateRef ma' \ !a' -> Control.do
    (a, ma) <- updateRef (\ !a -> Control.pure (a, a')) ma
    Control.pure (ma, a)

readSharedRef :: (β <= α) => Share α (Ref a) %1 -> BO β (Ur (Share α a))
{-# INLINE readSharedRef #-}
readSharedRef = Unsafe.toLinear \(UnsafeAlias mv) ->
  Control.pure $ Ur $! UnsafeAlias NonLinear.$! NonLinear.fst $! Ref.unsafeReadRef mv
