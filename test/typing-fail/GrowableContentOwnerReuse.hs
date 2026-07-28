{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GrowableContentOwnerReuse where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Prelude.Linear

-- This fixture must not typecheck: the outer mutable owner is captured by the
-- content callback after the same linear capability has entered withContent_.
badGrowthWhileContentLive ::
  Mut α (Growable.GrowableVector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Int))
badGrowthWhileContentLive vector =
  Growable.withContent_ vector \contents -> Control.do
    grown <- upcast (Growable.push 1 vector)
    let
      !() = consume grown
      !() = consume contents
    Control.pure ()
