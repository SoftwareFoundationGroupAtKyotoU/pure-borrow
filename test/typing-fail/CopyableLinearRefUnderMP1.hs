{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module CopyableLinearRefUnderMP1 where

import Control.Monad.Borrow.Pure (Copyable)
import Data.Ref.Linear (Ref)
import GHC.Exts (Any, Multiplicity (..))
import Generics.Linear

data Holder where
  Holder :: Ref Int %1 -> Holder

instance Generic Holder where
  type Rep Holder = D1 Any (C1 Any (S1 Any (MP1 'One (K1 R (Ref Int)))))
  from holder = fromHolder holder
    where
      fromHolder :: Holder %1 -> Rep Holder x
      fromHolder (Holder ref) = M1 (M1 (M1 (MP1 (K1 ref))))
  to rep = toHolder rep
    where
      toHolder :: Rep Holder x %1 -> Holder
      toHolder (M1 (M1 (M1 (MP1 (K1 ref))))) = Holder ref

-- This fixture must not typecheck: a linear field is copied, also under a hand-written MP1 'One, and a reference cannot be.
-- EXPECT: cannot be copied!
deriving via Generically Holder instance Copyable Holder
