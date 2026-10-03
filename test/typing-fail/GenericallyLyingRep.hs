{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}

module GenericallyLyingRep where

import Control.Monad.Borrow.Pure (Mut, Static)
import Data.Coerce.Directed (upcast)
import Data.Ref.Linear (Ref)
import GHC.Generics (Generically (..), K1, R)
import Generics.Linear (Generic (..))

-- This fixture must not typecheck: 0.1.0.0's instance of (<:) to Generically trusted the Rep of a Generic instance, which anyone can write by hand.
-- This one omits the lifetime, with methods that are never called, and made upcast lengthen a borrow to 'Static'.
-- EXPECT: can no longer be derived via Generically
data Box α = Box (Mut α (Ref Int))

instance Generic (Box α) where
  type Rep (Box α) = K1 R (Mut Static (Ref Int))
  from = error "unused"
  to = error "unused"

lengthen :: Mut α (Ref Int) %1 -> Mut Static (Ref Int)
lengthen m = case upcast (Box m) :: Generically (Box Static) of
  Generically (Box m') -> m'
