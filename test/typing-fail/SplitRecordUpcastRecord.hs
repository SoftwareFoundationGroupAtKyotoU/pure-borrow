{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SplitRecordUpcastRecord where

import Control.Monad.Borrow.Pure.BO (upcast)
import Data.Record.Linear.Borrow.Experimental.Split (SplitRecord, SplittableRecord)
import Data.Ref.Linear (Ref)
import GHC.Exts (Multiplicity (..))
import Generics.Linear.TH (deriveGeneric)

data Node = Node {next :: Node, val :: Ref Int}

newtype Cursor = Cursor {next :: Node}

deriveGeneric ''Node

deriveGeneric ''Cursor

deriving anyclass instance SplittableRecord Node

deriving anyclass instance SplittableRecord Cursor

-- This fixture must not typecheck: the field next of a Cursor is the whole Node, whose field val was already split out.
-- EXPECT: Couldn't match type
-- EXPECT: arising from a use of
view ::
  SplitRecord Node bk α '[ '("next", '( 'One, Node))] %1 ->
  SplitRecord Cursor bk α '[ '("next", '( 'One, Node))]
view = upcast
