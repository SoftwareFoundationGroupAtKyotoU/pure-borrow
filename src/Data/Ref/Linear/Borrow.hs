{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
A reference cell. To mutate, use as @'Mut' α ('Ref' a)@.
This module is inteted to be imported qualified.
-}
module Data.Ref.Linear.Borrow (
  Ref (),
  update,
  modify,
  swap,
  readShare,
  copyRef,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO
import Control.Monad.Borrow.Copyable
import Control.Monad.Borrow.Unsafe
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

{- | Perform one read-modify-write traversal and return an auxiliary result.

This is the canonical operation when a mutation must also report what it
observed: for example, an insert can return the displaced old value and use it
to detect a collision without a separate lookup. Prefer this shape over
lookup-then-update in hot paths, since the latter silently traverses the
underlying structure twice.
-}
update :: forall α β a b w. (α >= β) => (a %1 -> BO' w β (b, a)) %1 -> Mut α (Ref a) %1 -> BO' w β (b, Mut α (Ref a))
{-# INLINE update #-}
update f (UnsafeAlias mv) = DataFlow.do
  -- NOTE: as there is only one reference to @'Ref' a@, we can just use read/write
  -- instead of 'MutVar.atomicModify' (which requires pure function) while retaining atomicity.
  (!a, !mv) <- Ref.unsafeReadRef mv
  f a Control.<&> \(!b, !a) -> DataFlow.do
    !mv <- Ref.unsafeWriteRef mv a
    (b, UnsafeAlias mv)

modify :: forall α β a w. (α >= β) => (a %1 -> a) %1 -> Mut α (Ref a) %1 -> BO' w β (Mut α (Ref a))
modify f ma = Control.do
  ((), ma) <- update (Control.pure . ((),) . f) ma
  Control.pure ma

swap :: forall α β a w. (α >= β) => Mut α (Ref a) %1 -> Mut α (Ref a) %1 -> BO' w β (Mut α (Ref a), Mut α (Ref a))
{-# INLINE swap #-}
swap ma ma' =
  flip update ma' \ !a' -> Control.do
    (a, ma) <- update (\ !a -> Control.pure (a, a')) ma
    Control.pure (ma, a)

readShare :: forall α β a w. (α >= β) => Share α (Ref a) %1 -> BO' w β (Ur (Share α a))
{-# INLINE readShare #-}
readShare = Unsafe.toLinear \(UnsafeAlias mv) ->
  Control.pure $ Ur $! UnsafeAlias NonLinear.$! NonLinear.fst $! Ref.unsafeReadRef mv

copyRef :: forall a α β k w. (Copyable a, α >= β) => Borrow k α (Ref a) %1 -> BO' w β a
{-# INLINE copyRef #-}
copyRef bor =
  share bor & \(Ur bor) -> Control.do
    Ur !shr <- readShare bor
    Control.pure $! copy shr
