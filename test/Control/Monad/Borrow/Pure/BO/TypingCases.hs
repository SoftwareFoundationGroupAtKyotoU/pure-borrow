{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Control.Monad.Borrow.Pure.BO.TypingCases (
  module Control.Monad.Borrow.Pure.BO.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine (aff)
import Control.Monad.Borrow.Pure.BO (nowStatic, runBO_)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Token (Linearly, Now, linearly, withLinearly)
import Prelude.Linear (consume, lseq)

-- This must not typecheck: a 'Now' token for 'Static' exists only inside
-- 'Control.Monad.Borrow.Pure.BO', where it is bound linearly.
-- A top-level token let 'Control.Monad.Borrow.Pure.withLinearly' mint an
-- unrestricted 'Control.Monad.Borrow.Pure.Linearly'.
badTopLevelStaticNow :: Now Static
badTopLevelStaticNow = nowStatic

-- This must not typecheck: 'withLinearly' mints a 'Linearly' from the token
-- that 'nowStatic' produces, but only linearly, because 'nowStatic' runs only
-- where a linear token already exists, and 'linearly' will not hand it out
-- unrestricted, because 'Linearly' has no 'Movable' instance.
-- 'linearly' moves its result, so forcing this raises the deferred error.
badEscapeStaticLinearly :: Linearly
badEscapeStaticLinearly =
  linearly \lin -> runBO_ lin Control.do
    now <- nowStatic
    case withLinearly now of
      (minted, now) -> Control.pure (consume (aff now) `lseq` minted)
