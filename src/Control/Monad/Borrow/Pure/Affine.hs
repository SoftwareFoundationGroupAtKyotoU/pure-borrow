{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Monad.Borrow.Pure.Affine (
  -- * Affine Modality
  Affine (..),
  AsAffine (..),
  Aff,
  affu,
  unaff,
  pop,

  -- ** Linear Generics
  GenericAffine,
  GenericallyAffine (..),
) where

import Control.Monad.Borrow.Pure.Affine.Internal
