{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GenericGrowableUnrestricted.ContentOwnerReuse where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable.Growable.Linear.Borrow.Unrestricted qualified as Growable
import Prelude.Linear

shortenBO :: (α >= β) => BO α a %1 -> BO β a
shortenBO = upcast

-- This fixture must not typecheck: the outer mutable owner is captured by the
-- content callback after the same linear capability has entered withContent_.
--
-- Verified with GHC 9.12.4: the resulting `Many`/`One` multiplicity mismatch is
-- rejected while compiling this module even with
-- `-fdefer-type-errors -Wno-deferred-type-errors`. It therefore cannot live in
-- `TypingCases` as a runtime-observed deferred type error.
badGrowthWhileContentLive ::
  Mut α (Growable.GrowableVector V.Vector Int) %1 ->
  BO α (Mut α (Growable.GrowableVector V.Vector Int))
badGrowthWhileContentLive vector =
  Growable.withContent_ vector \contents -> Control.do
    grown <- shortenBO (Growable.push 1 vector)
    let
      !() = consume grown
      !() = consume contents
    Control.pure ()
