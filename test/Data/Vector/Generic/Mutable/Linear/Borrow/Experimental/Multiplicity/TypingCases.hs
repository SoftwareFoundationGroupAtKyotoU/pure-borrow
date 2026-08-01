{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity.TypingCases (
  module Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Control.Monad.Borrow.Pure.Clone (Clone (clone))
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Linear.Borrow.Experimental.Multiplicity qualified as Vector
import GHC.Exts (Multiplicity (One))
import Prelude.Linear

data NoCapabilities = NoCapabilities Int

data ConsumableOnly = ConsumableOnly Int

instance Consumable ConsumableOnly where
  consume (ConsumableOnly value) = consume value

badOwningGet ::
  Mut α (Vector.Vector One V.Vector Int) %1 ->
  BO
    α
    ( Mut α Int
    , Mut α (Vector.Vector One V.Vector Int)
    )
badOwningGet = Vector.get 0

badOwningGetCase :: ()
badOwningGetCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO_ runLinear Control.do
      (element, vector) <-
        badOwningGet
          ( UnsafeAlias
              ( Vector.fromList @V.Vector
                  [1]
                  ownerLinear ::
                  Vector.Vector One V.Vector Int
              )
          )
      Control.pure (consume (element, vector))

badConsume ::
  Vector.Vector One V.Vector NoCapabilities %1 ->
  ()
badConsume = consume

badConsumeCase :: ()
badConsumeCase =
  linearly \linear ->
    badConsume
      ( Vector.fromList @V.Vector
          [NoCapabilities 1]
          linear
      )

badClone ::
  Share α (Vector.Vector One V.Vector ConsumableOnly) %1 ->
  BO α (Vector.Vector One V.Vector ConsumableOnly)
badClone = clone

badCloneCase :: ()
badCloneCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Vector.fromList @V.Vector
              [ConsumableOnly 1]
              ownerLinear
          )
      (cloned, vector) <- sharing vector (\shared -> badClone shared)
      let !() = consume vector
      pureAfter
        (consume cloned `lseq` consume (reclaim lend))

badCopyCase :: ()
badCopyCase =
  linearly \linear -> DataFlow.do
    (ownerLinear, runLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <-
        borrowM
          ( Vector.fromList @V.Vector
              [NoCapabilities 1]
              ownerLinear
          )
      (Ur _, vector) <- Vector.copyToVector vector
      let !() = consume vector
      pureAfter (badConsume (reclaim lend))
