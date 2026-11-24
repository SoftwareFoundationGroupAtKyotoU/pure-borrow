{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Coerce.Directed (
  type (<:) (),
  upcast,
  Subtype1 (),
  upcast1,
  AsCoercible (..),
  GenericSubtype,
  genericUpcast,
) where

import Data.Coerce.Directed.Internal
