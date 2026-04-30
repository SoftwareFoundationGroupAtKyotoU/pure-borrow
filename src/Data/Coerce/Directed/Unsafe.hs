{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module exposes the unsafe internals of subtyping, which is only meant to be used for library implementors.
module Data.Coerce.Directed.Unsafe (
  type (<:) (..),
  SubtypeWitness (..),
  upcast,
  AsCoercible (..),
  GenericSubtype,
  genericUpcast,
) where

import Data.Coerce.Directed.Internal
