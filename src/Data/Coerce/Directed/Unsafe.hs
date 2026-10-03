{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | The unsafe internals of subtyping: the witness, for writing an instance of '(<:)' by hand, and 'deriveSubtype', for deriving one for a type of your own.

An instance of '(<:)' written with 'UnsafeSubtype' promises that 'upcast', which is @unsafeCoerce@, turns every value of the first type into a valid value of the second: the two have the same representation, and a borrow reached through the result neither lives longer nor allows more than the one it came from.
'deriveSubtype' keeps that promise by reading the declaration of the type, under the caveat in its Haddock; import it alone, as @import Data.Coerce.Directed.Unsafe (deriveSubtype)@.
-}
module Data.Coerce.Directed.Unsafe (
  type (<:) (..),
  SubtypeWitness (..),
  upcast,
  AsCoercible (..),
  GenericSubtype,
  genericUpcast,
  deriveSubtype,
) where

import Data.Coerce.Directed.Internal
import Data.Coerce.Directed.TH.Internal (deriveSubtype)
