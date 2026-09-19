{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Borrowing with effects sequenced in IO.

Use 'runBIO' in linear IO, or 'withBIO' to return an unrestricted result in ordinary IO.
The borrowing operations in "Control.Monad.Borrow.BO" and the container modules work in both pure and impure worlds.
Use 'liftSystemIOU' for ordinary IO results that are already unrestricted.
'liftSystemIO' instead transfers its result into linear ownership and requires the freshness discipline documented by linear-base.

Parallel BIO actions may observe nondeterministic IO effects.
The current parallel primitives do not propagate child exceptions or cancel children when the parent is interrupted.
A child exception can leave the parent waiting indefinitely, and child effects can continue after parent cancellation.
-}
module Control.Monad.Borrow.IO (
  BIO,
  runBIO,
  runBIOLend,
  runBIO_,
  withBIO,
  execBIO,
  liftBO,
  MonadIO (..),
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.BO
import Control.Monad.Borrow.Internal (execBIO, unsafeBOToLinIO)
import Control.Monad.IO.Class.Linear (MonadIO (..))
import GHC.Exts (RealWorld)
import Prelude.Linear
import System.IO.Linear qualified as L

{- | Run an IO-world action and discharge its finalizer after its fresh lifetime ends.
The enclosing linear IO action supplies the state token; no pure runner is used.
-}
runBIO :: forall a. (forall α. BIO α (After α a)) %1 -> L.IO a
{-# INLINE runBIO #-}
runBIO action = Control.do
  lin <- unsafeBOToLinIO (askLinearly @Static @RealWorld)
  newLifetime' lin \now -> Control.do
    (finished, after) <- execBIO action now
    case endLifetime finished of
      Ur end -> Control.pure $! withEnd end after

-- | Run an IO-world action and reclaim the owner after its lifetime ends.
runBIOLend :: forall a. (forall α. BIO α (Lend α a)) %1 -> L.IO a
{-# INLINE runBIOLend #-}
runBIOLend action = runBIO (reclaim' Control.<$> action)

-- | Run an IO-world action whose result does not mention its private lifetime.
runBIO_ :: forall a. (forall α. BIO α a) %1 -> L.IO a
{-# INLINE runBIO_ #-}
runBIO_ action = runBIO Control.do
  a <- action
  pureAfter a

{- | Run borrowing in ordinary IO and return an unrestricted result.
The callback is unrestricted, so it cannot capture a linearly owned resource and can safely be executed again by IO.
-}
withBIO :: forall a. (forall α. BIO α (After α (Ur a))) -> IO a
{-# INLINE withBIO #-}
withBIO action = L.withLinearIO (runBIO action)
