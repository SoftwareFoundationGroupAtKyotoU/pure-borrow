{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Coerce.Directed.TypingCases (
  module Data.Coerce.Directed.TypingCases,
) where

import Control.Monad.Borrow.Pure.BO (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken)
import Data.Coerce.Directed.Internal (MultLe)
import Data.Coerce.Directed.Unsafe (Subtype1 (..), SubtypeWitness, type (<:) (..))
import Data.Ref.Linear (Ref)
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import GHC.Exts (Multiplicity (..))
import Generics.Linear (Generically (..), Generically1 (..))
import Generics.Linear qualified as GL
import Generics.Linear.TH (deriveGeneric)
import Prelude.Linear

-- 'upcast' between function types requires the source multiplicity to be at
-- most the target one, with 'One' below 'Many': a linear function may be used
-- as an unrestricted one, never the other way round.

oneBelowMany :: MultLe 'One 'Many :~: 'True
oneBelowMany = Refl

manyBelowMany :: MultLe 'Many 'Many :~: 'True
manyBelowMany = Refl

-- This must not typecheck: an equation that ignored its arguments once made
-- 'Many' compare below 'One', which let 'upcast' turn @a -> (a, a)@ into
-- @a %1 -> (a, a)@.
badManyBelowOne :: MultLe 'Many 'One :~: 'True
badManyBelowOne = Refl

-- Two distinct types must be refused by 'Generically'.
data DistinctLeft = DistinctLeft Int

deriveGeneric ''DistinctLeft

data DistinctRight = DistinctRight Int

deriveGeneric ''DistinctRight

badGenericallyDistinctTypes :: SubtypeWitness DistinctLeft (Generically DistinctRight)
badGenericallyDistinctTypes = subtype

-- The refusal of 'Subtype1' for 'Mut', which is invariant in what it borrows, is raised where its witness is forced.
badSubtype1Mut :: SubtypeWitness (Mut Static Int) (Mut (Al 0) Int)
badSubtype1Mut = subtype1 (subtype @Int @Int)

-- Two distinct functors must be refused by 'Generically1'.
badGenerically1DistinctFunctors :: SubtypeWitness (Maybe Int) (Generically1 [] Int)
badGenerically1DistinctFunctors = subtype1 @Maybe @(Generically1 []) (subtype @Int @Int)

-- Two separate 'Generic' instances of one type constructor, which list the fields in other orders, must not relate its two instantiations.
data TwoInst tag a b = TwoInst a b

instance GL.Generic (TwoInst Int a b) where
  type Rep (TwoInst Int a b) = GL.Rep (a, b)
  from (TwoInst x y) = GL.from (x, y)
  to r = case GL.to r of (x, y) -> TwoInst x y

instance GL.Generic (TwoInst Bool a b) where
  type Rep (TwoInst Bool a b) = GL.Rep (b, a)
  from (TwoInst x y) = GL.from (y, x)
  to r = case GL.to r of (y, x) -> TwoInst x y

badGenericallyTwoInstances ::
  SubtypeWitness
    (TwoInst Int (Mut α (Ref Int)) (Mut β (Ref Int)))
    (Generically (TwoInst Bool (Mut β (Ref Int)) (Mut α (Ref Int))))
badGenericallyTwoInstances = subtype

-- The same through 'Generically1', with two separate 'Generic1' instances.
data P t α β a where
  P :: Mut α (Ref Int) %1 -> Mut β (Ref Int) %1 -> a %1 -> P t α β a

type Sel = 'GL.MetaSel 'Nothing 'GL.NoSourceUnpackedness 'GL.NoSourceStrictness 'GL.DecidedLazy

type Wrap f = GL.D1 ('GL.MetaData "P" "Data.Coerce.Directed.TypingCases" "main" 'False) (GL.C1 ('GL.MetaCons "P" 'GL.PrefixI 'False) f)

instance GL.Generic1 (P Int α β) where
  type Rep1 (P Int α β) = Wrap (GL.S1 Sel (GL.K1 GL.R (Mut α (Ref Int))) GL.:*: (GL.S1 Sel (GL.K1 GL.R (Mut β (Ref Int))) GL.:*: GL.S1 Sel GL.Par1))
  from1 (P x y z) = GL.M1 (GL.M1 (GL.M1 (GL.K1 x) GL.:*: (GL.M1 (GL.K1 y) GL.:*: GL.M1 (GL.Par1 z))))
  to1 (GL.M1 (GL.M1 (GL.M1 (GL.K1 x) GL.:*: (GL.M1 (GL.K1 y) GL.:*: GL.M1 (GL.Par1 z))))) = P x y z

instance GL.Generic1 (P Bool α β) where
  type Rep1 (P Bool α β) = Wrap (GL.S1 Sel (GL.K1 GL.R (Mut β (Ref Int))) GL.:*: (GL.S1 Sel (GL.K1 GL.R (Mut α (Ref Int))) GL.:*: GL.S1 Sel GL.Par1))
  from1 (P x y z) = GL.M1 (GL.M1 (GL.M1 (GL.K1 y) GL.:*: (GL.M1 (GL.K1 x) GL.:*: GL.M1 (GL.Par1 z))))
  to1 (GL.M1 (GL.M1 (GL.M1 (GL.K1 y) GL.:*: (GL.M1 (GL.K1 x) GL.:*: GL.M1 (GL.Par1 z))))) = P x y z

badGenerically1TwoInstances :: SubtypeWitness (P Int α β ()) (Generically1 (P Bool β α) ())
badGenerically1TwoInstances = subtype1 (subtype @() @())

-- A field held linearly must not be read back as unrestricted.
badSubtype1Multiplicity :: SubtypeWitness (GL.MP1 'One GL.Par1 Int) (GL.MP1 'Many GL.Par1 Int)
badSubtype1Multiplicity = subtype1 (subtype @Int @Int)

-- The guards refuse a 'Generic' instance for an abstract type, raised where its 'from' is forced.
badGenericVector :: VL.Vector Int %1 -> GL.Rep (VL.Vector Int) ()
badGenericVector = GL.from

badGenericMut :: Mut Static Int %1 -> GL.Rep (Mut Static Int) ()
badGenericMut = GL.from

badGenericEndToken :: EndToken Static %1 -> GL.Rep (EndToken Static) ()
badGenericEndToken = GL.from
