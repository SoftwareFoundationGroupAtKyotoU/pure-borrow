{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Control.Monad.Borrow.Pure.Internal (
  module Control.Monad.Borrow.Pure.Internal,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.Lifetime
import Control.Monad.Borrow.Pure.Lifetime.Token
import Control.Monad.Borrow.Pure.Lifetime.Token.Internal
import Control.Monad.ST.Strict (ST)
import Data.Functor.Linear qualified as Data
import Data.Kind (Type)
import GHC.Base (TYPE)
import GHC.Base qualified as GHC
import GHC.Exts (State#, realWorld#)
import GHC.ST qualified as ST
import Prelude.Linear qualified as PL
import System.IO.Linear qualified as L
import Unsafe.Linear qualified as Unsafe

-- NOTE: We want to use `TypeData` extension for 'ForBO', but it makes Haddock panic!

type ForBO :: Lifetime -> Type
data ForBO α

-- Morally an ST Monad, but linear!
newtype BO α a = BO (State# (ForBO α) %1 -> (# State# (ForBO α), a #))

instance Data.Functor (BO α) where
  fmap f (BO g) = BO \s -> case g s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Control.Functor (BO α) where
  fmap f (BO g) = BO \s -> case g s of
    (# s', a #) -> (# s', f a #)
  {-# INLINE fmap #-}

instance Data.Applicative (BO α) where
  pure a = Control.pure a
  {-# INLINE pure #-}

  (<*>) = \f g -> f Control.<*> g
  {-# INLINE (<*>) #-}

instance Control.Applicative (BO α) where
  pure a = BO \s -> (# s, a #)
  {-# INLINE pure #-}

  BO f <*> BO g = BO \s -> case f s of
    (# s', h #) -> case g s' of
      (# s'', a #) -> (# s'', h a #)
  {-# INLINE (<*>) #-}

instance Control.Monad (BO α) where
  BO fa >>= f = BO \s -> case fa s of
    (# s', a #) -> (f a) PL.& \(BO g) -> g s'
  {-# INLINE (>>=) #-}

unsafeBOToLinIO :: BO α a %1 -> L.IO a
{-# INLINE unsafeBOToLinIO #-}
unsafeBOToLinIO (BO f) = L.IO (Unsafe.coerce f)

runBO# :: forall {rep} α (o :: TYPE rep). (State# (ForBO α) %1 -> o) %1 -> o
{-# NOINLINE runBO# #-}
runBO# f = Unsafe.coerce f realWorld#

execBO :: BO α a %1 -> Now α %1 -> a
{-# INLINE execBO #-}
execBO (BO f) !now =
  now `PL.lseq` case runBO# f of
    (# s, !a #) -> dropState# s `PL.lseq` a

dropState# :: State# a %1 -> ()
{-# INLINE dropState# #-}
dropState# = Unsafe.toLinear \_ -> ()

runBO :: (forall α. BO α (End α -> a)) %1 -> Linearly %1 -> a
{-# INLINE runBO #-}
runBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> execBO bo now UnsafeEnd

sexecBO :: BO (α /\ β) a %1 -> Now α %1 -> BO β a
{-# INLINE sexecBO #-}
sexecBO f now = now `PL.lseq` unsafeCastBO f

unsafeCastBO :: BO α a %1 -> BO β a
{-# INLINE unsafeCastBO #-}
unsafeCastBO = Unsafe.coerce

unsafeSTToBO :: ST s a %1 -> BO α a
{-# INLINE unsafeSTToBO #-}
unsafeSTToBO (ST.ST f) = BO (Unsafe.coerce f)

unsafeBOToST :: BO α a %1 -> ST s a
{-# INLINE unsafeBOToST #-}
unsafeBOToST (BO f) = ST.ST (Unsafe.coerce f)

unsafeIOToBO :: L.IO a %1 -> BO α a
{-# INLINE unsafeIOToBO #-}
unsafeIOToBO (L.IO f) = BO (Unsafe.coerce f)

srunBO :: (forall α. BO (α /\ β) (End α -> a)) %1 -> Linearly %1 -> BO β a
{-# INLINE srunBO #-}
srunBO bo lin =
  case newLifetime lin of
    MkSomeNow now -> Control.do
      f <- sexecBO bo now
      Control.pure (f UnsafeEnd)

unsafeSystemIOToBO :: IO a %1 -> BO α a
{-# INLINE unsafeSystemIOToBO #-}
unsafeSystemIOToBO (GHC.IO a) = BO (Unsafe.coerce a)

unsafeBOToSystemIO :: BO α a %1 -> IO a
{-# INLINE unsafeBOToSystemIO #-}
unsafeBOToSystemIO (BO f) = GHC.IO (Unsafe.coerce f)

unsafePerformUndupableBO :: BO α a %1 -> a
unsafePerformUndupableBO (BO f) = runBO# \s ->
  case Unsafe.toLinear GHC.noDuplicate# s of
    s -> case f s of
      (# s, !a #) -> dropState# s `PL.lseq` a

-- | Run two computations in parallel, returning their results as a tuple.
parBO :: BO α a %1 -> BO α b %1 -> BO α (a, b)
{-# INLINE parBO #-}
parBO a b =
  -- TODO: define explicit rules to when to invoke noDuplicate#
  BO \s -> case Unsafe.toLinear GHC.noDuplicate# s of
    s -> case Unsafe.toLinear2 GHC.spark# (unsafePerformUndupableBO a) s of
      (# s, a #) -> case Unsafe.toLinear2 GHC.spark# (unsafePerformUndupableBO b) s of
        (# s, b #) -> case Unsafe.toLinear2 GHC.seq# a s of
          (# s, a #) -> case Unsafe.toLinear2 GHC.seq# b s of
            (# s, b #) -> (# s, (a, b) #)
