{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  module Control.Monad.Borrow.Pure.Lifetime.Token.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Affine.Internal
import Control.Monad.Borrow.Pure.Lifetime.Internal
import Data.Coerce.Directed (SubtypeWitness (UnsafeSubtype), upcast, type (<:) (..))
import Data.Functor.Linear qualified as Data
import Data.Kind (Constraint)
import Data.Unrestricted.Linear
import GHC.Base (TYPE, UnliftedType, noinline, withDict)
import GHC.Exts qualified as GHC
import GHC.Stack (HasCallStack)
import Unsafe.Linear qualified as Unsafe

type role Now nominal

data Now (α :: Lifetime) = UnsafeNow

type role EndToken nominal

data EndToken (α :: Lifetime) = UnsafeEnd

class End (α :: Lifetime) where
  endToken :: EndToken α

newtype After α a = After ((End α) => a)

instance (α <= β, a <: b) => After α a <: After β b where
  subtype = UnsafeSubtype

unAfter :: (End α) => After α a %1 -> a
{-# INLINE unAfter #-}
unAfter (After r) = r

withEnd :: forall α r. EndToken α -> After α r %1 -> r
{-# INLINE withEnd #-}
withEnd end (After a) = Unsafe.toLinear (withDict @(End α) end) a

instance Data.Functor (After α) where
  fmap f (After r) = After (f r)
  {-# INLINE fmap #-}

instance Control.Functor (After α) where
  fmap f (After r) = After (f r)
  {-# INLINE fmap #-}

instance Data.Applicative (After α) where
  pure a = After a
  {-# INLINE pure #-}
  After f <*> After r = After (f r)
  {-# INLINE (<*>) #-}

instance Control.Applicative (After α) where
  pure a = After a
  {-# INLINE pure #-}
  After f <*> After r = After (f r)
  {-# INLINE (<*>) #-}

instance Control.Monad (After α) where
  After r >>= k = After (unAfter (k r))
  {-# INLINE (>>=) #-}

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
  -- GHC optimizer then eliminates every invocation on bulk allocation functions
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

endLifetime :: Now (Al i) %1 -> (Ur (EndToken (Al i)))
endLifetime UnsafeNow = Ur UnsafeEnd

data SomeNow where
  MkSomeNow :: Now (Al i) %1 -> SomeNow

instance (α >= β) => EndToken α <: EndToken β where
  subtype = UnsafeSubtype

newLifetime :: Linearly %1 -> SomeNow
newLifetime UnsafeLinearly = MkSomeNow UnsafeNow

newLifetime' :: Linearly %1 -> (forall ι. Now (Al ι) %1 -> a) %1 -> a
newLifetime' lin k =
  case newLifetime lin of
    MkSomeNow now -> k now

-- | Static Lifetime is always available.
nowStatic :: Now Static
nowStatic = UnsafeNow

-- | Static lifetime lasts forever.
neverEnds :: (HasCallStack, End Static) => a
neverEnds = error "Unreachable: if you see this, you created an End Static in the internal code!"
