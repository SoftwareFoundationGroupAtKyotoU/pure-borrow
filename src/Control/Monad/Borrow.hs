{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Borrowing operations shared by pure, IO, and downstream worlds.

'BO' is the pure specialization of 'BO\'', and 'BIO' is its IO specialization.
The world index is nominal: a computation cannot change worlds through 'Data.Coerce.coerce'.
Use "Control.Monad.Borrow.Pure" for the existing pure prelude or "Control.Monad.Borrow.IO" for effectful runners.
-}
module Control.Monad.Borrow (
  module Control.Monad.Borrow.BO,
  module Control.Monad.Borrow.Clone,
  module Control.Monad.Borrow.Copyable,
  mapConcurrentlyOf,
  forConcurrentlyOf,
  Consumable (..),
  Dupable (..),
  dup,
  dup3,
  Movable (..),
  Ur (..),
) where

import Control.Monad.Borrow.BO
import Control.Monad.Borrow.Clone
import Control.Monad.Borrow.Copyable
import Control.Optics.Linear (Traversal, traverseOf)
import Data.Unrestricted.Linear (Consumable (..), Dupable (..), Movable (..), Ur (..), dup, dup3)
import Prelude.Linear ((.))
import Prelude.Linear qualified as PL

mapConcurrentlyOf ::
  forall s t a b α w.
  (Forkable w) =>
  Traversal s t a b ->
  (a %1 -> BO' w α b) ->
  s %1 ->
  BO' w α t
mapConcurrentlyOf l f = runPar . traverseOf l (Par . f)

forConcurrentlyOf ::
  forall s t a b α w.
  (Forkable w) =>
  Traversal s t a b ->
  s %1 ->
  (a %1 -> BO' w α b) ->
  BO' w α t
forConcurrentlyOf l = PL.flip (mapConcurrentlyOf l)
