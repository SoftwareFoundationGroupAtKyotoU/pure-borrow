{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeExistential where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: an existential field cannot be related at two instantiations.
-- EXPECT: whose fields mention a, which is not a parameter of Hidden
data Hidden b = forall a. Hidden a b

deriveSubtype ''Hidden
