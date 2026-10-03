{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DeriveSubtypeHigherRankSynonym where

import Data.Coerce.Directed.Unsafe (deriveSubtype)

-- This fixture must not typecheck: a forall behind a type synonym is refused as well.
-- EXPECT: has a forall or a constraint in the type of field 1 of Church
type Cont a = forall r. (a -> r) -> r

data Church a = Church (Cont a)

deriveSubtype ''Church
