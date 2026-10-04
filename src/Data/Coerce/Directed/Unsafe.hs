{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | The unsafe internals of subtyping, meant to be used only by library implementors.

An instance of '(<:)' written with 'UnsafeSubtype' promises that 'upcast', which is @unsafeCoerce@, turns every value of the first type into a valid value of the second: the two have the same representation, and a borrow reached through the result neither lives longer nor allows more than the one it came from.
-}
module Data.Coerce.Directed.Unsafe (
  type (<:) (..),
  SubtypeWitness (..),
  upcast,
  AsCoercible (..),
  GenericSubtype,
  genericUpcast,
) where

import Data.Coerce.Directed.Internal
