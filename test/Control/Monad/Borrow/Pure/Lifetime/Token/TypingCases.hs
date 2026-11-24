{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- Conversions between tokens that must not typecheck, each raised where it is forced.
module Control.Monad.Borrow.Pure.Lifetime.Token.TypingCases (
  module Control.Monad.Borrow.Pure.Lifetime.Token.TypingCases,
) where

-- The token modules are imported whole, so that a constructor any of them exported would be in scope.
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe
import Data.Coerce (coerce)
import Data.Coerce.Directed (upcast)
import GHC.Exts (Any)

-- A Linearly must not be made out of any value at hand.
badCoerceAny :: Any -> Linearly
badCoerceAny = coerce

-- An EndToken is unrestricted, so a Linearly made from one could be used twice.
badCoerceEndToLinearly :: EndToken α -> Linearly
badCoerceEndToLinearly = coerce

-- A Now turned into the EndToken of its own lifetime would reclaim while borrows are live.
badCoerceNowToEnd :: Now α -> EndToken α
badCoerceNowToEnd = coerce

badUpcastNowToEnd :: Now α %1 -> EndToken α
badUpcastNowToEnd = upcast

-- Relabelling a Now would move a computation to another lifetime.
badCoerceNowRelabel :: Now α -> Now β
badCoerceNowRelabel = coerce

badUpcastNowRelabel :: Now α %1 -> Now β
badUpcastNowRelabel = upcast

-- An EndToken relabelled to another lifetime would reclaim while that lifetime runs.
badCoerceEndRelabel :: EndToken α -> EndToken β
badCoerceEndRelabel = coerce
