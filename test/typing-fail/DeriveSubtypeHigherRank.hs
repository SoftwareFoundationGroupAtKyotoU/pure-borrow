{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeHigherRank where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: a field with a forall is not related by (<:).
-- EXPECT: has a forall or a constraint in the type of field 1 of Church
data Church a = Church (forall r. (a -> r) -> r)

deriveSubtype ''Church
