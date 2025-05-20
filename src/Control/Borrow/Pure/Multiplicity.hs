{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Control.Borrow.Pure.Multiplicity (Affine (..), Persist (..), AsPersist (..)) where

import Data.Int
import Data.Kind
import Data.Monoid qualified as Mon
import Data.Semigroup qualified as Sem
import Data.Unrestricted.Linear (Consumable (consume))
import Data.Word
import GHC.Base
import Numeric.Natural (Natural)
import Prelude.Linear (Ur)

{- | The levity-polymorphic variant of "Prelude.Linear.Consumable".

NOTE: I couldn't see the reason for separating 'pop' from the method.
After all, making this a method would not restrict the flexibility
and allows a room for custom release function.
-}
type Affine :: forall {rep}. TYPE rep -> Constraint
class Affine a where
  pop :: a %1 -> ()

newtype WrapConsumable a = WrapConsumable {runWrapConsumable :: a}

instance (Consumable a) => Affine (WrapConsumable a) where
  pop (WrapConsumable x) = consume x
  {-# INLINE pop #-}

deriving via WrapConsumable Bool instance Affine Bool

deriving via WrapConsumable Mon.All instance Affine Mon.All

deriving via WrapConsumable Mon.Any instance Affine Mon.Any

deriving via WrapConsumable Void instance Affine Void

deriving via WrapConsumable Char instance Affine Char

deriving via WrapConsumable Int instance Affine Int

deriving via WrapConsumable Int8 instance Affine Int8

deriving via WrapConsumable Int16 instance Affine Int16

deriving via WrapConsumable Int32 instance Affine Int32

deriving via WrapConsumable Int64 instance Affine Int64

deriving via WrapConsumable Word instance Affine Word

deriving via WrapConsumable Word8 instance Affine Word8

deriving via WrapConsumable Word16 instance Affine Word16

deriving via WrapConsumable Word32 instance Affine Word32

deriving via WrapConsumable Word64 instance Affine Word64

deriving via WrapConsumable Ordering instance Affine Ordering

deriving via WrapConsumable Integer instance Affine Integer

deriving via WrapConsumable Natural instance Affine Natural

deriving via WrapConsumable () instance Affine ()

deriving newtype instance (Affine a) => Affine (Sem.First a)

deriving newtype instance (Affine a) => Affine (Sem.Last a)

deriving newtype instance (Affine a) => Affine (Sem.Dual a)

deriving newtype instance (Affine a) => Affine (Sem.Min a)

deriving newtype instance (Affine a) => Affine (Sem.Max a)

instance (Affine a) => Affine (Maybe a) where
  pop (Just x) = pop x
  pop Nothing = ()
  {-# INLINE pop #-}

deriving via Maybe a instance (Affine a) => Affine (Mon.First a)

deriving via Maybe a instance (Affine a) => Affine (Mon.Last a)

deriving newtype instance (Affine a) => Affine (Mon.Product a)

deriving newtype instance (Affine a) => Affine (Mon.Sum a)

newtype AsPersist a = AsPersist {runAsPersistent :: a}

class (Affine a) => Persist a where
  persist :: a %1 -> Ur a
