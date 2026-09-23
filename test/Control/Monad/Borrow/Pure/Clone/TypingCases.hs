{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{- | Clones that must not typecheck: contents that are 'Dupable' but not 'Clone' cannot be cloned through a shared borrow of their container.

'dup2' consumes the piece it duplicates, and linear-base's laws do not say which copy, if either, is the original, so a clone through 'dup2' either consumes the original twice or can hand it to the clone.
See Note [Cloning the contents of a shared borrow] in "Data.Ref.Linear.Internal".

Each case runs the clone, because the missing instance is only needed, and its deferred error only raised, when the container's 'clone' clones a piece of the contents.
-}
module Control.Monad.Borrow.Pure.Clone.TypingCases (
  module Control.Monad.Borrow.Pure.Clone.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Data.Ref.Linear qualified as Ref
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Prelude.Linear

-- | Contents that can be duplicated, but have no 'Clone' instance.
newtype DupableOnly = DupableOnly Int

instance Consumable DupableOnly where
  consume (DupableOnly n) = consume n

instance Dupable DupableOnly where
  dup2 (DupableOnly n) = dup2 n & \(a, b) -> (DupableOnly a, DupableOnly b)

-- | Clone a container of 'DupableOnly' through a shared borrow, then consume the clone and the original.
cloneDupableOnly ::
  (Consumable (f DupableOnly)) =>
  (forall α. Share α (f DupableOnly) %1 -> BO α (f DupableOnly)) ->
  (Linearly %1 -> f DupableOnly) %1 ->
  ()
cloneDupableOnly cloneIt build = linearly \lin -> runBO lin Control.do
  container <- asksLinearly build
  (borrowed, lend) <- borrowM container
  let !(Ur shared) = share borrowed
  cloned <- cloneIt shared
  pureAfter (consume cloned `lseq` consume (reclaim lend))

refOfDupableOnly :: ()
refOfDupableOnly = cloneDupableOnly clone (Ref.new (DupableOnly 1))

vectorOfDupableOnly :: ()
vectorOfDupableOnly = cloneDupableOnly clone (VL.fromList [DupableOnly 1])
