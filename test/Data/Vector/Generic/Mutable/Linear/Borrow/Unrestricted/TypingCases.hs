{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.TypingCases (
  module Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Copyable (Copyable (copy))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Coerce (coerce)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Linear.Borrow.Unrestricted qualified as Vector
import Data.Vector.Mutable.Linear.Borrow qualified as Owning
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

newtype WrappedInt = WrappedInt Int

newtype WrappedBackend a = WrappedBackend (V.Vector a)

badBackendCoercion ::
  Vector.Vector V.Vector Int %1 ->
  Vector.Vector WrappedBackend Int
badBackendCoercion = Unsafe.toLinear coerce

badElementCoercion ::
  Vector.Vector V.Vector WrappedInt %1 ->
  Vector.Vector V.Vector Int
badElementCoercion = Unsafe.toLinear coerce

badOwnershipCoercion ::
  Vector.Vector V.Vector Int %1 ->
  Owning.Vector Int
badOwnershipCoercion = Unsafe.toLinear coerce

badElementBorrow ::
  Mut α (Vector.Vector V.Vector Int) %1 ->
  BO α (Mut α Int)
badElementBorrow = Vector.get 0

badDuplicate ::
  Borrow bk α (Vector.Vector V.Vector Int) %1 ->
  Vector.Vector V.Vector Int
badDuplicate = copy

badMutateShared ::
  Share α (Vector.Vector V.Vector Int) ->
  BO α (Share α (Vector.Vector V.Vector Int))
badMutateShared =
  Vector.modify 0 (\value -> value NonLinear.+ 1)

badElementCoercionCase :: Int
badElementCoercionCase =
  linearly \linear ->
    case Vector.toVector
      ( badElementCoercion
          (Vector.fromList @V.Vector [WrappedInt 1] linear)
      ) of
      Ur vector -> V.length vector

badOwnershipCoercionCase :: Int
badOwnershipCoercionCase =
  linearly \linear ->
    case Owning.toVector
      (badOwnershipCoercion (Vector.fromList @V.Vector [1] linear)) of
      Ur vector -> V.length vector

badElementBorrowCase :: Int
badElementBorrowCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1] ownerLinear)
      element <- badElementBorrow vector
      let !() = consume element
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur frozen -> V.length frozen
        )

badMutateSharedCase :: Int
badMutateSharedCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM (Vector.fromList @V.Vector [1] ownerLinear)
      vector <-
        sharing_ vector \shared -> Control.do
          shared <- badMutateShared shared
          Control.pure (consume shared)
      let !() = consume vector
      pureAfter
        ( case Vector.toVector (reclaim lend) of
            Ur frozen -> V.length frozen
        )
