{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module SubtypeSelfDerivingVia where

import Control.Monad.Borrow.Pure (Mut)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Data.Coerce.Directed (type (<:))
import Data.Ref.Linear (Ref)

data Cell = Cell Int

-- This fixture must not typecheck: deriving via the instance's own type passes
-- any role check and yields a self-referential witness.
-- A local type in the head draws no orphan warning, and the library's
-- compound instances (pairs, lists, 'Generically') never force a component's
-- witness, so @upcast (m, ())@ used to lengthen a live 'Mut' to 'Static' with
-- no warning at all.
-- EXPECT: Illegal instance for type synonym
deriving via (Mut Static (Ref Cell)) instance {-# OVERLAPPING #-} Mut α (Ref Cell) <: Mut Static (Ref Cell)
