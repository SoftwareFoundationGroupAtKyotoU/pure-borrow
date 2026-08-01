{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Vector.Unboxed.Mutable.Linear.TypingCases (
  module Data.Vector.Unboxed.Mutable.Linear.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce (coerce)
import Data.Ref.Linear qualified as Ref
import Data.Vector.Mutable.Linear.Borrow qualified as Boxed
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Unboxed
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

newtype WrappedInt = WrappedInt Int

badElementCoercion ::
  Unboxed.Vector WrappedInt %1 ->
  Unboxed.Vector Int
badElementCoercion = Unsafe.toLinear coerce

badUnboxedToBoxed ::
  Unboxed.Vector Int %1 ->
  Boxed.Vector Int
badUnboxedToBoxed = Unsafe.toLinear coerce

badBoxedToUnboxed ::
  Boxed.Vector Int %1 ->
  Unboxed.Vector Int
badBoxedToUnboxed = Unsafe.toLinear coerce

badLifetimeSwap ::
  Mut α (Unboxed.Vector Int) %1 ->
  Mut β (Unboxed.Vector Int)
badLifetimeSwap = Unsafe.toLinear coerce

badSplit ::
  Mut α (Unboxed.Vector Int) %1 ->
  Unboxed.Vector (Mut α Int)
badSplit = split

badDuplicate ::
  Borrow borrowKind α (Unboxed.Vector Int) %1 ->
  Unboxed.Vector Int
badDuplicate = copy

newtype LinearElement = LinearElement (Ref.Ref Int)

type BoxedLinearElement = U.DoNotUnboxLazy LinearElement

instance Consumable (U.DoNotUnboxLazy LinearElement) where
  consume =
    Unsafe.toLinear \(U.DoNotUnboxLazy (LinearElement ref)) ->
      consume ref

nonCopyableGet ::
  Mut α (Unboxed.Vector BoxedLinearElement) %1 ->
  BO α (Mut α BoxedLinearElement)
nonCopyableGet = Unboxed.get 0

badNonCopyableCopyAtMut ::
  Mut α (Unboxed.Vector BoxedLinearElement) %1 ->
  BO
    α
    ( Ur BoxedLinearElement
    , Mut α (Unboxed.Vector BoxedLinearElement)
    )
badNonCopyableCopyAtMut = Unboxed.copyAtMut 0

badNonCopyableCopyAtMutCase :: Int
badNonCopyableCopyAtMutCase =
  linearly \linear -> DataFlow.do
    (refLinear, remainingLinear) <- dup linear
    (ownerLinear, runLinear) <- dup remainingLinear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Unboxed.fromList
              [U.DoNotUnboxLazy (LinearElement (Ref.new 1 refLinear))]
              ownerLinear
          )
      (Ur _, vector) <- Unboxed.copyAtMut 0 vector
      let !() = consume vector
      pureAfter (consume (reclaim lend) `lseq` 0)

badNonCopyableCopyAtCase :: Int
badNonCopyableCopyAtCase =
  linearly \linear -> DataFlow.do
    (refLinear, remainingLinear) <- dup linear
    (ownerLinear, runLinear) <- dup remainingLinear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Unboxed.fromList
              [U.DoNotUnboxLazy (LinearElement (Ref.new 1 refLinear))]
              ownerLinear
          )
      share vector & \(Ur shared) -> Control.do
        Ur _ <- Unboxed.copyAt 0 shared
        let !() = consume shared
        pureAfter (consume (reclaim lend) `lseq` 0)
