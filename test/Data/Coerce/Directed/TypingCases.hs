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
import Data.Coerce.Directed.Internal (MultLe)
import Data.Coerce.Directed.Unsafe (Subtype1 (..), SubtypeWitness, type (<:) (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exts (Multiplicity (..))
import Generics.Linear (Generic (..), Generically (..), Generically1 (..), K1 (..), R)
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

-- Two distinct types, even with metadata-free 'Generic' instances, must be refused by 'Generically'.
newtype MetaFreeLeft = MetaFreeLeft Int

instance Generic MetaFreeLeft where
  type Rep MetaFreeLeft = K1 R Int
  from (MetaFreeLeft x) = K1 x
  to (K1 x) = MetaFreeLeft x

newtype MetaFreeRight = MetaFreeRight Int

instance Generic MetaFreeRight where
  type Rep MetaFreeRight = K1 R Int
  from (MetaFreeRight x) = K1 x
  to (K1 x) = MetaFreeRight x

badGenericallyTwoTypes :: SubtypeWitness MetaFreeLeft (Generically MetaFreeRight)
badGenericallyTwoTypes = subtype

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
