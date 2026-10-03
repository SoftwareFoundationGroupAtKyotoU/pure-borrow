{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | The class behind '(<:)', for declaring that a type of your own is a subtype of another.

An instance promises that 'upcast', which is @unsafeCoerce@, turns every value of the first type into a valid value of the second: the two have the same representation, and a borrow reached through the result neither lives longer nor allows more than the one it came from.
The library cannot check that promise, which is why the safe modules export only the synonym '(<:)', against which no instance can be written.

The instance head must name 'Subtype', not '(<:)'.
An instance written for 0.1.0.0 changes like this:

> -- 0.1.0.0
> deriving via Generically (T b) instance (a <: b) => T a <: T b
> -- 0.2.0.0, with this module imported
> deriving via Generically (T b) instance (a <: b) => Subtype (T a) (T b)

A hand-written instance defines the method as @'subtype' = 'UnsafeSubtype'@.
To convert a value between two types of the same shape without declaring an instance, use 'genericUpcast', which needs the 'Generics.Linear.Generic' instances of linear-generics, derived with @Generics.Linear.TH.deriveGeneric@.
-}
module Data.Coerce.Directed.Unsafe (
  Subtype (..),
  type (<:),
  SubtypeWitness (..),
  upcast,
  AsCoercible (..),
  GenericSubtype,
  genericUpcast,
) where

import Data.Coerce.Directed.Internal
