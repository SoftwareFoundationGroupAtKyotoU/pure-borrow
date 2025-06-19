{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Borrow.Pure.Affine (
  -- * Affine Modality
  Affable (..),
  AsAffable (..),
  Aff,
  affu,
  unaff,
  pop,

  -- ** Linear Generics
  GenericAffable,
  GenericallyAffable (..),
) where

import Control.Borrow.Pure.Affine.Internal
