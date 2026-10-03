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
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe
import Control.Monad.Borrow.Pure.Copyable
import Data.Ref.Linear (Ref)
import Data.Ref.Linear.Internal qualified as Ref
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

{- | Perform one read-modify-write traversal and return an auxiliary result.

This is the canonical operation when a mutation must also report what it
observed: for example, an insert can return the displaced old value and use it
to detect a collision without a separate lookup. Prefer this shape over
lookup-then-update in hot paths, since the latter silently traverses the
underlying structure twice.
-}
update :: (α >= β) => (a %1 -> BO β (b, a)) %1 -> Mut α (Ref a) %1 -> BO β (b, Mut α (Ref a))
{-# INLINE update #-}
update = Unsafe.toLinear2 \f borrow@(UnsafeAlias ref) -> Control.do
  -- NOTE: as there is only one reference to @'Ref' a@, we can just use read/write
  -- instead of 'MutVar.atomicModify' (which requires pure function) while retaining atomicity.
  -- Both happen in the state thread: a write left in a lazy result would run
  -- whenever the caller forced it, possibly after the lifetime had ended.
  a <- Ref.unsafeReadRefBO ref
  (!b, !a) <- f a
  () <- Ref.unsafeWriteRefBO ref a
  Control.pure (b, borrow)

modify :: (α >= β) => (a %1 -> a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a))
modify f ma = Control.do
  ((), ma) <- update (Control.pure . ((),) . f) ma
  Control.pure ma

swap :: (α >= β) => Mut α (Ref a) %1 -> Mut α (Ref a) %1 -> BO β (Mut α (Ref a), Mut α (Ref a))
{-# INLINE swap #-}
swap ma ma' =
  flip update ma' \ !a' -> Control.do
    (a, ma) <- update (\ !a -> Control.pure (a, a')) ma
    Control.pure (ma, a)

readShare :: (α >= β) => Share α (Ref a) %1 -> BO β (Ur (Share α a))
{-# INLINE readShare #-}
readShare = Unsafe.toLinear \(UnsafeAlias ref) -> Control.do
  -- Read and force inside the state thread: a read left to a lazy 'Control.pure'
  -- would run whenever the result is demanded, possibly after later writes.
  value <- Ref.unsafeReadRefBO ref
  value <- evaluateBO value
  Control.pure (Unsafe.toLinear (\value -> Ur (UnsafeAlias value)) value)

copyRef :: (Copyable a, α >= β) => Borrow k α (Ref a) %1 -> BO β a
{-# INLINE copyRef #-}
copyRef bor =
  share bor & \(Ur bor) -> Control.do
    Ur !shr <- readShare bor
    Control.pure $! copy shr
