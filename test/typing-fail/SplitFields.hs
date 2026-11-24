{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SplitFields where

import Control.Monad.Borrow.Pure (DistributesAlias, Ur)
import Data.Ref.Linear (Ref)
import GHC.Exts (Any, Multiplicity (..))
import Generics.Linear
import Generics.Linear.TH (deriveGenericAnd1)

-- A linear field of another type.
data Counted a = Counted (Ref Int) a

-- A nonlinear field that holds the parameter.
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

-- A nonlinear field that holds the parameter under a functor.
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

-- The parameter under 'Ur', directly and inside a 'Maybe'.
data UrWrapped a = UrWrapped !(Ur a) a

data UrMaybeWrapped a = UrMaybeWrapped !(Ur (Maybe a)) a

deriveGenericAnd1 ''Counted

deriveGenericAnd1 ''UrWrapped

deriveGenericAnd1 ''UrMaybeWrapped

-- This fixture must not typecheck: 'Control.Monad.Borrow.Pure.split' would hand out the linearly owned 'Data.Ref.Linear.Ref' of a borrowed structure as an owned value, while the lender still gets it back.
-- Or it would put a 'Control.Monad.Borrow.Pure.Mut' in a nonlinear field or under 'Ur', where it would be unrestricted, and an unrestricted 'Control.Monad.Borrow.Pure.Mut' gives 'Control.Monad.Borrow.Pure.withLinearly' a 'Control.Monad.Borrow.Pure.Linearly' outside any linear context.
-- EXPECT: A borrow cannot be distributed over a linear field of type
-- EXPECT: If the field holds a GC-owned value, declare it as
-- EXPECT: A borrow cannot be distributed over the nonlinear field represented as
-- EXPECT: A borrow cannot be distributed over Ur
-- EXPECT: :.: Ur)
-- EXPECT: instance DistributesAlias Counted
-- EXPECT: instance DistributesAlias Both
-- EXPECT: instance DistributesAlias Spare
-- EXPECT: instance DistributesAlias UrWrapped
-- EXPECT: instance DistributesAlias UrMaybeWrapped
deriving anyclass instance DistributesAlias Counted

deriving anyclass instance DistributesAlias Both

deriving anyclass instance DistributesAlias Spare

deriving anyclass instance DistributesAlias UrWrapped

deriving anyclass instance DistributesAlias UrMaybeWrapped
