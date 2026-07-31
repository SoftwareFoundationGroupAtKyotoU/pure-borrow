{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.TypingCases (
  module Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce (coerce)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Owning
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

newtype WrappedInt = WrappedInt Int

newtype WrappedBackend a = WrappedBackend (V.Vector a)

badBackendCoercion ::
  Growable.GrowableVector V.Vector Int %1 ->
  Growable.GrowableVector WrappedBackend Int
badBackendCoercion = Unsafe.toLinear coerce

badElementCoercion ::
  Growable.GrowableVector V.Vector WrappedInt %1 ->
  Growable.GrowableVector V.Vector Int
badElementCoercion = Unsafe.toLinear coerce

badOwnershipCoercion ::
  Growable.GrowableVector V.Vector Int %1 ->
  Owning.GrowableVector Int
badOwnershipCoercion = Unsafe.toLinear coerce

badElementBorrow ::
  Mut α (Growable.GrowableVector V.Vector Int) %1 ->
  BO α (Mut α Int)
badElementBorrow = Growable.get 0

badDuplicate ::
  Borrow bk α (Growable.GrowableVector V.Vector Int) %1 ->
  Growable.GrowableVector V.Vector Int
badDuplicate = copy

badMutateShared ::
  Share α (Growable.GrowableVector V.Vector Int) ->
  BO α (Share α (Growable.GrowableVector V.Vector Int))
badMutateShared =
  Growable.modify 0 (\value -> value NonLinear.+ 1)

badGrowShared ::
  Share α (Growable.GrowableVector V.Vector Int) ->
  BO α (Share α (Growable.GrowableVector V.Vector Int))
badGrowShared = Growable.reserve 10

badElementCoercionCase :: Int
badElementCoercionCase =
  linearly \linear ->
    case Growable.toVector
      ( badElementCoercion
          (Growable.fromList @V.Vector [WrappedInt 1] linear)
      ) of
      Ur vector -> V.length vector

badOwnershipCoercionCase :: Int
badOwnershipCoercionCase =
  linearly \linear ->
    case Owning.toVector
      (badOwnershipCoercion (Growable.fromList @V.Vector [1] linear)) of
      Ur vector -> V.length vector

badElementBorrowCase :: Int
badElementBorrowCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromList @V.Vector [1] ownerLinear)
      element <- badElementBorrow vector
      let !() = consume element
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen -> V.length frozen
        )

badMutateSharedCase :: Int
badMutateSharedCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromList @V.Vector [1] ownerLinear)
      vector <-
        sharing_ vector \shared -> Control.do
          shared <- badMutateShared shared
          Control.pure (consume shared)
      let !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen -> V.length frozen
        )

badGrowSharedCase :: Int
badGrowSharedCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.fromList @V.Vector [1] ownerLinear)
      vector <-
        sharing_ vector \shared -> Control.do
          shared <- badGrowShared shared
          Control.pure (consume shared)
      let !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen -> V.length frozen
        )

badContentEscapeCase :: Int
badContentEscapeCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Growable.empty @V.Vector @Int ownerLinear)
      (escaped, vector) <- Growable.withContent vector Control.pure
      let
        !() = consume escaped
        !() = consume vector
      pureAfter
        ( case Growable.toVector (reclaim lend) of
            Ur frozen -> V.length frozen
        )
