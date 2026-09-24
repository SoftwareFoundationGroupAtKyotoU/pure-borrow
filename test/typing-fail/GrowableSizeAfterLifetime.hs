{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GrowableSizeAfterLifetime where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Prelude.Linear

-- This fixture must not typecheck: a shared borrow of a growable vector is
-- kept past the end of its lifetime, and its size is read in a later run.
-- 'Growable.size' runs in @BO β@ only where @α >= β@, which no lifetime of
-- the later run satisfies.
--
-- It cannot live in @TypingCases@: 'Growable.size' never forces its outlives
-- evidence, so the deferred error would never be raised.
-- EXPECT: Could not deduce
-- EXPECT: <=!!
sizeAfterLifetime :: Int
sizeAfterLifetime =
  linearly \linear -> DataFlow.do
    (ownerLinear, linear) <- dup linear
    (runLinear, lateLinear) <- dup linear
    runBO runLinear Control.do
      (vector, lend) <- borrowM (Growable.fromList [1, 2, 3 :: Int] ownerLinear)
      share vector & \(Ur shared) ->
        pureAfter
          ( consume (reclaim lend)
              `lseq` runBO_ lateLinear Control.do
                (Ur n, shared) <- Growable.size shared
                Control.pure (consume shared `lseq` n)
          )
