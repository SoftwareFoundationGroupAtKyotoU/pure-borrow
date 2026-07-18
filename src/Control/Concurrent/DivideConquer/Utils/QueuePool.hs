{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{- | Implements a hybrid scheduler of work-stealing and work-sharing.
Each thread has its own local queue and steals from others when it's idle, but sleeps and waits for works to be pushed after several tries.
When pushing works, it shares a batch with a waiter (if any) to avoid starvation.
-}
module Control.Concurrent.DivideConquer.Utils.QueuePool (
  QueuePool,
  newQueuePool,
  pushWorks,
  popWork,
  pushWorkMaster,
) where

import Control.Applicative qualified as P
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar, yield)
import Control.Concurrent.Queue.ChaseLev (ChaseLevDeq, StealResult (..), close, estimateSize, isClosed, newDeq, pushFront, pushFronts, stealHalf, tryPopFront)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMQueue (TMQueue, closeTMQueue, isClosedTMQueue, newTMQueueIO, readTMQueue, tryReadTMQueue, unGetTMQueue, writeTMQueue)
import Control.Monad (forM_)
import Control.Monad qualified as NonLinear
import Control.Monad qualified as P
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (..), unsafeSystemIOToBO)
import Data.Function (fix)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.V.Linear (V, theLength)
import Data.V.Linear.Internal (V (..))
import Data.Vector qualified as V
-- import Debug.Trace (traceEventIO)
import GHC.Exts qualified as GHC
import GHC.IO qualified as GHC
import GHC.TypeLits (KnownNat)
import Prelude.Linear
import System.Random.Stateful (Random (randoms), RandomGen, StdGen, mkStdGen, randomR)
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as P

data QueuePool a = QueuePool
  { mine :: !(ChaseLevDeq a)
  , others :: !(V.Vector (ChaseLevDeq a))
  , mySwitch :: {-# UNPACK #-} !(MVar ())
  , waiting :: {-# UNPACK #-} !(TMQueue (MVar ()))
  , injection :: !(TMQueue (NonEmpty a))
  , num :: {-# UNPACK #-} !Int
  , gen :: {-# UNPACK #-} !(IORef StdGen)
  }

data MasterQueuePool a = MasterQueuePool
  { pools :: ![ChaseLevDeq a]
  , switches :: ![MVar ()]
  , waiting :: !(TMQueue (MVar ()))
  , injection :: !(TMQueue (NonEmpty a))
  }

instance Consumable (MasterQueuePool a) where
  {-# NOINLINE consume #-}
  consume = GHC.noinline $ Unsafe.toLinear \MasterQueuePool {..} -> GHC.unsafePerformIO do
    P.mapM_ close pools
    P.mapM_ (flip tryPutMVar ()) switches
    atomically do
      closeTMQueue waiting
      closeTMQueue injection

newQueuePool ::
  forall n a α g.
  (KnownNat n, RandomGen g) =>
  g ->
  BO α (V n (Mut α (QueuePool a)), MasterQueuePool a)
newQueuePool g = unsafeSystemIOToBO do
  let n = theLength @n

  qs <- NonLinear.replicateM n newDeq
  waiting <- newTMQueueIO
  injection <- newTMQueueIO
  qs <-
    P.mapM
      ( \(num, ini, mine, tl, seed) -> do
          let others = V.fromList $ tl <> ini
          gen <- newIORef $ mkStdGen seed
          mySwitch <- newEmptyMVar
          P.pure P.$ QueuePool {others, ..}
      )
      P.$ L.zip5
        [0 ..]
        (L.inits qs)
        qs
        (P.drop 1 $ L.tails qs)
        (randoms g)
  let pools = P.map (.mine) qs
      switches = P.map (.mySwitch) qs
      master = MasterQueuePool {..}
  P.pure (V $ V.fromList $ map UnsafeAlias qs, master)

pushWorkMaster :: Mut α (MasterQueuePool a) %1 -> a %1 -> BO α (Mut α (MasterQueuePool a))
pushWorkMaster = Unsafe.toLinear2 \pool@(UnsafeAlias (MasterQueuePool {pools})) work ->
  case pools of
    (q : _) -> unsafeSystemIOToBO do
      pushFront q work
      P.pure pool
    [] -> error "impossible: the length of pools is determined by the type-level nat n and cannot be zero"

-- | Pushes works, the last element is on the front.
pushWorks :: Mut α (QueuePool a) %1 -> [a] %1 -> BO α (Mut α (QueuePool a))
pushWorks = Unsafe.toLinear2 \(UnsafeAlias QueuePool {..}) works ->
  unsafeSystemIOToBO do
    pushFronts mine works
    -- If a worker is asleep, give it a batch on every publication.  The batch
    -- must be visible before the wakeup, so the signalled worker can never wake
    -- and wait for a not-yet-published injection.
    token <- atomically do
      P.join P.<$> tryReadTMQueue waiting
    forM_ token \token -> fix \self -> do
      half <- stealHalf mine
      case half of
        Nothing -> do
          putMVar token ()
        Just (Found ts) -> do
          atomically $ writeTMQueue injection ts
          putMVar token ()
        Just Race -> do
          yield
          self
        Just Empty -> do
          atomically $ unGetTMQueue waiting token

    P.pure $ UnsafeAlias QueuePool {..}

popWork :: Mut α (QueuePool a) %1 -> BO α (Maybe (a, Mut α (QueuePool a)))
popWork = Unsafe.toLinear \qs@(UnsafeAlias QueuePool {..}) ->
  unsafeSystemIOToBO do
    -- num <- estimateSize mine
    -- traceEventIO $ "EVT: Estimated size: " <> show num
    tryPopFront mine P.>>= \case
      Nothing -> do
        -- traceEventIO "EVT: Finished!"
        P.pure Nothing
      Just (Just x) -> do
        -- traceEventIO "EVT: Got work from own queue!"
        P.pure $ Just (x, qs)
      Just Nothing ->
        (0 :: Int) & fix \self !retry ->
          if retry >= V.length others + 32
            then do
              -- traceEventIO "EVT: Too many retries. Sleep until pushed..."
              closed <- atomically $ do
                writeTMQueue waiting mySwitch
                isClosedTMQueue waiting
              if closed
                then P.pure Nothing
                else do
                  takeMVar mySwitch
                  -- traceEventIO $ "EVT: Woken up! Retrying to pop..."
                  mtasks <- atomically $ readTMQueue injection
                  -- traceEventIO $ "EVT: Got " <> show (P.fmap P.length mtasks) <> " injected tasks!"
                  case mtasks of
                    Nothing -> P.pure Nothing
                    Just (x :| xs) -> do
                      pushFronts mine xs
                      P.pure $ Just (x, qs)
            else do
              let sleep = P.unless (retry < V.length others) do
                    g <- readIORef gen
                    let !wait = min 100 (1.5 ^ retry :: Double)
                        (!q, g') = randomR (1, floor wait) g
                    writeIORef gen g'
                    -- traceEventIO $ "EVT: Failed to steal. Waiting for " <> show q <> " us..."
                    if q > 10 then threadDelay q else yield
              cls <- isClosed mine
              if cls
                then do
                  P.pure Nothing
                else do
                  let !nOthers = V.length others
                  if
                    | V.null others -> P.pure Nothing
                    | nOthers == 1 -> do
                        let !q = V.unsafeHead others
                        progress <- stealHalf q
                        case progress of
                          Nothing -> do
                            -- traceEventIO "EVT: Closing..."
                            P.pure Nothing
                          Just (Found (x :| xs)) -> do
                            -- traceEventIO $ "EVT: Stolen! " <> P.show (P.length xs P.+ 1)
                            pushFronts mine xs
                            P.pure $ Just (x, qs)
                          Just Empty -> self (retry + 1)
                          Just Race -> sleep P.*> self (retry + 1)
                    | otherwise -> do
                        g0 <- readIORef gen
                        let (!i, !g1) = randomR (0, nOthers - 1) g0
                            (!j0, !g2) = randomR (0, nOthers - 2) g1
                            !j = if j0 P.== i then nOthers - 1 else j0
                        writeIORef gen g2
                        let !q1 = V.unsafeIndex others i
                            !q2 = V.unsafeIndex others j
                        !s1 <- estimateSize q1
                        !s2 <- estimateSize q2
                        -- traceEventIO $ "EVT: Steal candidates' sizes: " <> show (s1, s2)
                        let !targ = if s1 P.>= s2 then q1 else q2
                        progress <- stealHalf targ
                        case progress of
                          Nothing -> do
                            -- traceEventIO "EVT: Closing..."
                            P.pure Nothing
                          Just (Found (x :| xs)) -> do
                            -- traceEventIO $ "EVT: Stolen! " <> P.show (P.length xs P.+ 1)
                            pushFronts mine xs
                            P.pure $ Just (x, qs)
                          Just Empty -> self (retry + 1)
                          Just Race -> sleep P.*> self (retry + 1)
