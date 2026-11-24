{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SubtypeGenericallyTwoInstances where

import Control.Monad.Borrow.Pure.BO (Mut)
import Data.Coerce.Directed (upcast)
import Data.Ref.Linear (Ref)
import Generics.Linear (Generic (..), Generically (..))

data TwoInst tag a b = TwoInst a b

instance Generic (TwoInst Int a b) where
  type Rep (TwoInst Int a b) = Rep (a, b)
  from (TwoInst x y) = from (x, y)
  to r = case to r of (x, y) -> TwoInst x y

instance Generic (TwoInst Bool a b) where
  type Rep (TwoInst Bool a b) = Rep (b, a)
  from (TwoInst x y) = from (y, x)
  to r = case to r of (y, x) -> TwoInst x y

-- This fixture must not typecheck: two instances that list the fields in other orders would swap the lifetimes of two Muts.
-- EXPECT: arising from the head of a quantified constraint
swap ::
  TwoInst Int (Mut α (Ref Int)) (Mut β (Ref Int)) %1 ->
  Generically (TwoInst Bool (Mut β (Ref Int)) (Mut α (Ref Int)))
swap = upcast
