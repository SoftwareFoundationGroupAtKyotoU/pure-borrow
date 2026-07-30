{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Vector.Unboxed.Mutable.Growable.Linear.TypingCases (
  module Data.Vector.Unboxed.Mutable.Growable.Linear.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce (coerce)
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as BoxedGrowable
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as Growable
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as Fixed
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe

newtype WrappedInt = WrappedInt Int

newtype CopyOnly = CopyOnly Int

type UnboxedCopyOnly = U.DoNotUnboxLazy CopyOnly

instance Copyable UnboxedCopyOnly where
  copy = Unsafe.coerce

newtype NonCopyable = NonCopyable Int

type UnboxedNonCopyable = U.DoNotUnboxLazy NonCopyable

instance Consumable UnboxedNonCopyable where
  consume = Unsafe.toLinear \_ -> ()

instance Dupable UnboxedNonCopyable where
  dup2 = Unsafe.toLinear \value -> (value, value)

instance Movable UnboxedNonCopyable where
  move = Unsafe.toLinear \value -> Ur value

badElementCoercion ::
  Growable.GrowableVector WrappedInt %1 ->
  Growable.GrowableVector Int
badElementCoercion = Unsafe.toLinear coerce

badGrowableToFixed ::
  Growable.GrowableVector Int %1 ->
  Fixed.Vector Int
badGrowableToFixed = Unsafe.toLinear coerce

badFixedToGrowable ::
  Fixed.Vector Int %1 ->
  Growable.GrowableVector Int
badFixedToGrowable = Unsafe.toLinear coerce

badGrowableToFixedUpcast ::
  Growable.GrowableVector Int %1 ->
  Fixed.Vector Int
badGrowableToFixedUpcast = upcast

badFixedToGrowableUpcast ::
  Fixed.Vector Int %1 ->
  Growable.GrowableVector Int
badFixedToGrowableUpcast = upcast

badUnboxedGrowableToBoxedGrowable ::
  Growable.GrowableVector Int %1 ->
  BoxedGrowable.GrowableVector Int
badUnboxedGrowableToBoxedGrowable = Unsafe.toLinear coerce

badBoxedGrowableToUnboxedGrowable ::
  BoxedGrowable.GrowableVector Int %1 ->
  Growable.GrowableVector Int
badBoxedGrowableToUnboxedGrowable = Unsafe.toLinear coerce

badUnboxedGrowableToBoxedGrowableUpcast ::
  Growable.GrowableVector Int %1 ->
  BoxedGrowable.GrowableVector Int
badUnboxedGrowableToBoxedGrowableUpcast = upcast

badBoxedGrowableToUnboxedGrowableUpcast ::
  BoxedGrowable.GrowableVector Int %1 ->
  Growable.GrowableVector Int
badBoxedGrowableToUnboxedGrowableUpcast = upcast

badLifetimeSwap ::
  forall α β.
  Mut α (Growable.GrowableVector Int) %1 ->
  Mut β (Growable.GrowableVector Int)
{-# NOINLINE badLifetimeSwap #-}
badLifetimeSwap =
  Unsafe.toLinear
    ( coerce ::
        Mut α (Growable.GrowableVector Int) ->
        Mut β (Growable.GrowableVector Int)
    )

badLifetimeSwapCase :: Int
badLifetimeSwapCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      let !() = consume (badLifetimeSwap vector)
      pureAfter (consume (reclaim lend) `lseq` 0)

badSplit ::
  Mut α (Growable.GrowableVector Int) %1 ->
  Growable.GrowableVector (Mut α Int)
badSplit = split

badDuplicate ::
  Borrow borrowKind α (Growable.GrowableVector Int) %1 ->
  Growable.GrowableVector Int
badDuplicate = copy

badContentEscapeCase :: Int
badContentEscapeCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      (escaped, vector) <- Growable.withContent vector Control.pure
      let
        !() = consume escaped
        !() = consume vector
      pureAfter $
        case Growable.toVector (reclaim lend) of
          Ur frozen -> U.length frozen

badSharedContentEscapeCase :: Int
badSharedContentEscapeCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.empty ownerLinear)
      share vector & \(Ur sharedVector) -> Control.do
        (escaped, sharedVector) <-
          Growable.withContent sharedVector Control.pure
        let
          !() = consume escaped
          !() = consume sharedVector
        pureAfter $
          case Growable.toVector (reclaim lend) of
            Ur frozen -> U.length frozen

badGrowableCopyableOnlyToVectorCase :: Int
badGrowableCopyableOnlyToVectorCase =
  linearly \linear ->
    case Growable.toVector
      ( Growable.fromVector
          (U.singleton (U.DoNotUnboxLazy (CopyOnly 1)))
          linear
      ) of
      Ur frozen -> U.length frozen

badFixedCopyableOnlyToVectorCase :: Int
badFixedCopyableOnlyToVectorCase =
  linearly \linear ->
    case Fixed.toVector
      ( Fixed.fromVector
          (U.singleton (U.DoNotUnboxLazy (CopyOnly 1)))
          linear
      ) of
      Ur frozen -> U.length frozen

badNonCopyableCopyAtCase :: Int
badNonCopyableCopyAtCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              (U.singleton (U.DoNotUnboxLazy (NonCopyable 1)))
              ownerLinear
          )
      share vector & \(Ur shared) -> Control.do
        Ur _ <- Growable.copyAt 0 shared
        let !() = consume shared
        pureAfter (consume (reclaim lend) `lseq` 0)

badNonCopyableCopyAtMutCase :: Int
badNonCopyableCopyAtMutCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Growable.fromVector
              (U.singleton (U.DoNotUnboxLazy (NonCopyable 1)))
              ownerLinear
          )
      (Ur (U.DoNotUnboxLazy (NonCopyable copied)), vector) <-
        Growable.copyAtMut 0 vector
      let !() = consume vector
      pureAfter (consume (reclaim lend) `lseq` copied)
