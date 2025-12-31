{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  module Control.Monad.Borrow.Pure.Lifetime.Token.Internal,
) where

import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Internal
import Data.Coerce.Directed (SubtypeWitness (UnsafeSubtype), type (<:) (..))
import Data.Kind (Constraint)
import Data.Unrestricted.Linear
import GHC.Base (TYPE, UnliftedType, noinline)
import GHC.Exts qualified as GHC
import GHC.Stack (HasCallStack)

type role Now nominal

data Now (α :: Lifetime) = UnsafeNow

type role End nominal

data End (α :: Lifetime) = UnsafeEnd

data Linearly = UnsafeLinearly

linearly :: (Movable a) => (Linearly %1 -> a) %1 -> a
{-# NOINLINE linearly #-}
linearly = GHC.noinline \f ->
  case move (f UnsafeLinearly) of
    Ur !x -> x

data LinearOnlyWitness a = UnsafeLinearOnly

type LinearOnly :: forall rep. TYPE rep -> Constraint
class LinearOnly a where
  linearOnly :: LinearOnlyWitness a

withLinearly :: (LinearOnly a) => a %1 -> (Linearly, a)
{-# NOINLINE withLinearly #-}
withLinearly = noinline \ !a -> (UnsafeLinearly, a)

withLinearly# :: forall (a :: UnliftedType). (LinearOnly a) => a %1 -> (# Linearly, a #)
withLinearly# = noinline \ !a -> (# UnsafeLinearly, a #)

instance LinearOnly Linearly where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance Consumable Linearly where
  consume = \UnsafeLinearly -> ()
  {-# INLINE consume #-}

instance Dupable Linearly where
  -- NOTE: without inlining, GHC optimizer (especially, full-laziness and demand analysis)
  -- can eliminate duplicated 'Linearly's too eagerly, ruining the state-threading,
  -- and result in resource corruption in some cases.
  -- Such optimization can manifest when, for example, one duplicates 'Linearly'
  -- tokens multiple times and feed them to different allocation functions.
  -- Although we are not able to detect the exact situation, but we believe that
  -- GHC optimizer then eliminates every invocation on bulk alloction functions
  -- into a single one, which introduces unintended reuse of linear resources.
  -- Hence, we must instruct GHC not to inline this function and force
  dup2 = GHC.noinline \UnsafeLinearly -> (UnsafeLinearly, UnsafeLinearly)
  {-# NOINLINE dup2 #-}

instance Affine (Now α) where
  aff UnsafeNow = UnsafeAff UnsafeNow
  {-# INLINE aff #-}

instance LinearOnly (Now α) where
  linearOnly = UnsafeLinearOnly
  {-# INLINE linearOnly #-}

instance Affine (End α) where
  aff UnsafeEnd = UnsafeAff UnsafeEnd
  {-# INLINE aff #-}

instance Consumable (End α) where
  consume UnsafeEnd = ()
  {-# INLINE consume #-}

instance Dupable (End α) where
  dup2 UnsafeEnd = (UnsafeEnd, UnsafeEnd)
  {-# INLINE dup2 #-}

instance Movable (End α) where
  move UnsafeEnd = Ur UnsafeEnd
  {-# INLINE move #-}

endLifetime :: Now (Al i) %1 -> (Ur (End (Al i)))
endLifetime UnsafeNow = Ur UnsafeEnd

data SomeNow where
  MkSomeNow :: Now (Al i) %1 -> SomeNow

instance (β <= α) => End α <: End β where
  subtype = UnsafeSubtype

newLifetime :: Linearly %1 -> SomeNow
newLifetime UnsafeLinearly = MkSomeNow UnsafeNow

newLifetime' :: Linearly %1 -> (forall ι. Now (Al ι) %1 -> a) %1 -> a
newLifetime' lin k =
  case newLifetime lin of
    MkSomeNow now -> k now

-- | Static Lifetime is always available
nowStatic :: Now Static
nowStatic = UnsafeNow

-- | Static lifetime lasts forever
neverEnds :: (HasCallStack) => End Static %1 -> a
neverEnds UnsafeEnd = error "Unreachable: if you see this, you created an End Static in the internal code!"
