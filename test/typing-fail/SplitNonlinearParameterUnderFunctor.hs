{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SplitNonlinearParameterUnderFunctor where

import Control.Monad.Borrow.Pure (DistributesAlias)
import GHC.Exts (Any, Multiplicity (..))
import Generics.Linear

data Spare a where
  Spare :: [a] -> a %1 -> Spare a

instance Generic1 Spare where
  type Rep1 Spare = D1 Any (C1 Any (S1 Any (MP1 'Many (Par1 :.: [])) :*: S1 Any Par1))
  from1 spare = fromSpare spare
    where
      fromSpare :: Spare a %1 -> Rep1 Spare a
      fromSpare (Spare spares linear) = M1 (M1 (M1 (MP1 (Comp1 (Par1 spares))) :*: M1 (Par1 linear)))
  to1 rep = toSpare rep
    where
      toSpare :: Rep1 Spare a %1 -> Spare a
      toSpare (M1 (M1 (M1 (MP1 (Comp1 (Par1 spares))) :*: M1 (Par1 linear)))) = Spare spares linear

-- This fixture must not typecheck: the nonlinear field mentions the parameter
-- under a functor, so 'Control.Monad.Borrow.Pure.split' would fill it with
-- unrestricted borrows just the same.
-- EXPECT: A borrow cannot be distributed over the nonlinear field represented as
deriving anyclass instance DistributesAlias Spare
