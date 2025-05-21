{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Borrow.Pure.Affine (
  -- * Affine Types
  Affine,
  pop,

  -- ** Linear Generics
  GenericAffine,

  -- * Affine modality
  Aff,
  unaff,
  aff,
  fromAffine,
) where

import Control.Borrow.Pure.Affine.Internal
