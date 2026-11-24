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
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as GenericGrowable
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as Growable
import Prelude.Linear

shortenBO :: (α >= β) => BO α a %1 -> BO β a
shortenBO = upcast

-- This fixture must not typecheck: in the growable vector and in the generic one of unrestricted elements, the outer mutable owner is captured by the content callback after the same linear capability has entered withContent_.
-- GHC rejects the multiplicity mismatch even with -fdefer-type-errors, so it cannot be a case of TypingCases.
-- EXPECT: arising from multiplicity of
-- EXPECT: badOwnerReuse vector =
-- EXPECT: badGenericOwnerReuse vector =
badOwnerReuse ::
  Mut α (Growable.GrowableVector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector Int))
badOwnerReuse vector =
  Growable.withContent_ vector \contents -> Control.do
    grown <- upcast (Growable.push 1 vector)
    let
      !() = consume grown
      !() = consume contents
    Control.pure ()

badGenericOwnerReuse ::
  Mut α (GenericGrowable.GrowableVector V.Vector Int) %1 ->
  BO α (Mut α (GenericGrowable.GrowableVector V.Vector Int))
badGenericOwnerReuse vector =
  GenericGrowable.withContent_ vector \contents -> Control.do
    grown <- shortenBO (GenericGrowable.push 1 vector)
    let
      !() = consume grown
      !() = consume contents
    Control.pure ()
