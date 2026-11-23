{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SplitNonlinearParameterField where

import Control.Monad.Borrow.Pure (DistributesAlias)
import GHC.Exts (Any, Multiplicity (..))
import Generics.Linear

data Both a where
  Both :: a -> a %1 -> Both a

instance Generic1 Both where
  type Rep1 Both = D1 Any (C1 Any (S1 Any (MP1 'Many Par1) :*: S1 Any Par1))
  from1 both = fromBoth both
    where
      fromBoth :: Both a %1 -> Rep1 Both a
      fromBoth (Both unrestricted linear) = M1 (M1 (M1 (MP1 (Par1 unrestricted)) :*: M1 (Par1 linear)))
  to1 rep = toBoth rep
    where
      toBoth :: Rep1 Both a %1 -> Both a
      toBoth (M1 (M1 (M1 (MP1 (Par1 unrestricted)) :*: M1 (Par1 linear)))) = Both unrestricted linear

-- This fixture must not typecheck: 'Control.Monad.Borrow.Pure.split' would put
-- a 'Control.Monad.Borrow.Pure.Mut' in the nonlinear field, and an
-- unrestricted 'Control.Monad.Borrow.Pure.Mut' gives
-- 'Control.Monad.Borrow.Pure.withLinearly' a 'Control.Monad.Borrow.Pure.Linearly'
-- outside any linear context.
-- EXPECT: A borrow cannot be distributed over the nonlinear field represented as
deriving anyclass instance DistributesAlias Both
