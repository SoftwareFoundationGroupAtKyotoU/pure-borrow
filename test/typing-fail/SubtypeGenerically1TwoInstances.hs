{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SubtypeGenerically1TwoInstances where

import Control.Monad.Borrow.Pure.BO (Mut)
import Data.Coerce.Directed (upcast1)
import Data.Ref.Linear (Ref)
import Generics.Linear

data P t α β a where
  P :: Mut α (Ref Int) %1 -> Mut β (Ref Int) %1 -> a %1 -> P t α β a

type Sel = 'MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

type Wrap f = D1 ('MetaData "P" "SubtypeGenerically1TwoInstances" "main" 'False) (C1 ('MetaCons "P" 'PrefixI 'False) f)

instance Generic1 (P Int α β) where
  type Rep1 (P Int α β) = Wrap (S1 Sel (K1 R (Mut α (Ref Int))) :*: (S1 Sel (K1 R (Mut β (Ref Int))) :*: S1 Sel Par1))
  from1 (P x y z) = M1 (M1 (M1 (K1 x) :*: (M1 (K1 y) :*: M1 (Par1 z))))
  to1 (M1 (M1 (M1 (K1 x) :*: (M1 (K1 y) :*: M1 (Par1 z))))) = P x y z

instance Generic1 (P Bool α β) where
  type Rep1 (P Bool α β) = Wrap (S1 Sel (K1 R (Mut β (Ref Int))) :*: (S1 Sel (K1 R (Mut α (Ref Int))) :*: S1 Sel Par1))
  from1 (P x y z) = M1 (M1 (M1 (K1 y) :*: (M1 (K1 x) :*: M1 (Par1 z))))
  to1 (M1 (M1 (M1 (K1 y) :*: (M1 (K1 x) :*: M1 (Par1 z))))) = P x y z

-- This fixture must not typecheck: two instances that list the fields in other orders would swap the lifetimes of two Muts.
-- EXPECT: No instance for
-- EXPECT: Generically1 (P Bool β α)
swap :: P Int α β () %1 -> Generically1 (P Bool β α) ()
swap = upcast1
