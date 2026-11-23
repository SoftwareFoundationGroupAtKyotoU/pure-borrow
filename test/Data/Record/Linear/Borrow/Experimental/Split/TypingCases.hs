{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Record.Linear.Borrow.Experimental.Split.TypingCases (
  module Data.Record.Linear.Borrow.Experimental.Split.TypingCases,
) where

import Control.Monad.Borrow.Pure.BO (upcast)
import Data.Record.Linear.Borrow.Experimental.Split (SplitRecord, SplittableRecord)
import Data.Ref.Linear (Ref)
import GHC.Exts (Multiplicity (..))
import Generics.Linear.TH (deriveGeneric)

data R = R {x :: Ref Int, y :: Ref Int}

deriveGeneric ''R

deriving anyclass instance SplittableRecord R

-- This must not typecheck: putting back the field y after it was split out would give a second live borrow of it.
badResetFields ::
  SplitRecord R bk α '[ '("x", '( 'One, Ref Int))] %1 ->
  SplitRecord R bk α '[ '("x", '( 'One, Ref Int)), '("y", '( 'One, Ref Int))]
badResetFields = upcast

data Node = Node {next :: Node, val :: Ref Int}

newtype Cursor = Cursor {next :: Node}

deriveGeneric ''Node

deriveGeneric ''Cursor

deriving anyclass instance SplittableRecord Node

deriving anyclass instance SplittableRecord Cursor

-- This must not typecheck: the field next of a Cursor is the whole Node, whose field val was already split out.
badViewRecord ::
  SplitRecord Node bk α '[ '("next", '( 'One, Node))] %1 ->
  SplitRecord Cursor bk α '[ '("next", '( 'One, Node))]
badViewRecord = upcast
