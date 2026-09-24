{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Exception behaviour of 'parBO'. See Note [parBO and exceptions] in "Control.Monad.Borrow.Pure.BO.Internal".

Branches wait with 'threadDelay' rather than spinning, because a loop that never allocates cannot be interrupted.
Every timed wait catches 'ErrorCall' only: a 'try' for 'SomeException' inside 'timeout' would swallow the 'Timeout' itself.
-}
module Control.Monad.Borrow.Pure.ParBOSpec (
  module Control.Monad.Borrow.Pure.ParBOSpec,
) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, yield)
import Control.Exception (ErrorCall (..), SomeException, catch, evaluate, throwIO, try, uninterruptibleMask_)
import Control.Monad (filterM, forM_, replicateM_, void)
import Control.Monad.Borrow.Pure (BO, linearly, parBO, runBO_)
import Control.Monad.Borrow.Pure.BO.Unsafe (unsafeSystemIOToBO)
import Data.Functor.Linear qualified as Data
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, isJust)
import GHC.Conc (BlockReason (..), ThreadStatus (..), getUncaughtExceptionHandler, listThreads, mkWeakThreadId, setUncaughtExceptionHandler, threadStatus)
import GHC.IO (unsafePerformIO)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit
import Unsafe.Linear qualified as Unsafe

-- | Wait, then return.
delayed :: Int -> Int -> BO α Int
delayed milliseconds value =
  unsafeSystemIOToBO (threadDelay (milliseconds * 1_000) >> pure value)

-- | Wait, then fail.
failing :: Int -> String -> BO α Int
failing milliseconds message =
  unsafeSystemIOToBO (threadDelay (milliseconds * 1_000) >> throwIO (ErrorCall message))

runPair :: (forall α. BO α (Int, Int)) -> (Int, Int)
runPair action = linearly \lin -> runBO_ lin action

-- | Evaluate within five seconds and report which 'ErrorCall' it raised, if any.
failureWithin :: (Int, Int) -> IO (Maybe (Either String (Int, Int)))
failureWithin value =
  fmap (either (\(ErrorCall message) -> Left message) Right)
    <$> timeout 5_000_000 (try (evaluate value))

slowPair :: (Int, Int)
{-# NOINLINE slowPair #-}
slowPair = runPair (parBO (delayed 300 1) (delayed 300 2))

sharedPair :: (Int, Int)
{-# NOINLINE sharedPair #-}
sharedPair = runPair (parBO (delayed 300 3) (delayed 300 4))

-- | A shared value whose evaluation rethrows synchronously whatever interrupts it, so that a stop poisons it.
poisoned :: Int
{-# NOINLINE poisoned #-}
poisoned = unsafePerformIO do
  (threadDelay 2_000_000 >> pure 1) `catch` \(e :: SomeException) -> throwIO e

forcePoisoned :: BO α Int
forcePoisoned = unsafeSystemIOToBO (evaluate poisoned)

-- | Poll a condition every 10 ms until it holds or the deadline, in microseconds, passes.
pollFor :: Int -> IO Bool -> IO Bool
pollFor budget condition = do
  holds <- condition
  if holds || budget <= 0
    then pure holds
    else threadDelay 10_000 >> pollFor (budget - 10_000) condition

threadsBlockedOnThrowTo :: IO [ThreadId]
threadsBlockedOnThrowTo =
  listThreads >>= filterM \thread -> (== ThreadBlocked BlockedOnException) <$> threadStatus thread

{- | Record the running thread through a weak reference, which does not keep it alive, and return the given value.

A loop that runs 'parBO' must pass its index, here or to 'failing', into the branches: GHC floats a 'parBO' that does not depend on the index out of the loop, and the loop then runs it only once.
-}
recordThread :: IORef [Weak ThreadId] -> Int -> BO α Int
recordThread threads value = unsafeSystemIOToBO do
  weak <- myThreadId >>= mkWeakThreadId
  atomicModifyIORef' threads \weaks -> (weak : weaks, ())
  pure value

-- | Wait until the given number of threads have been recorded.
waitForThreads :: IORef [Weak ThreadId] -> Int -> IO ()
waitForThreads threads count = do
  recorded <- pollFor 5_000_000 ((>= count) . length <$> readIORef threads)
  assertBool "the branches did not all start" recorded

-- | Wait until every recorded thread has finished.
settle :: IORef [Weak ThreadId] -> IO ()
settle threads = do
  settled <- pollFor 5_000_000 do
    found <- readIORef threads >>= mapM deRefWeak
    statuses <- mapM threadStatus (catMaybes found)
    pure (all (`elem` [ThreadFinished, ThreadDied]) statuses)
  assertBool "the recorded branches did not finish" settled

-- | How many of the recorded threads are still reachable.
countAlive :: IORef [Weak ThreadId] -> IO Int
countAlive threads = length . filter isJust <$> (readIORef threads >>= mapM deRefWeak)

{- | Collect, and count the recorded threads that are still reachable, collecting again, up to four times, while any is.

One collection occasionally left one finished branch reachable (in 2 runs of the suite out of about 40, under load), and the next one freed it, whereas a reference that keeps the threads alive keeps them through every collection.
-}
aliveAfterCollecting :: IORef [Weak ThreadId] -> IO Int
aliveAfterCollecting threads = go (4 :: Int)
  where
    go n = do
      performMajorGC
      alive <- countAlive threads
      if alive == 0 || n <= 1 then pure alive else yield >> go (n - 1)

test_parBOExceptions :: TestTree
test_parBOExceptions =
  testGroup
    "parBO exceptions"
    [ testCase "both branches succeed" do
        failureWithin (runPair (parBO (delayed 10 1) (delayed 20 2)))
          >>= (@?= Just (Right (1, 2)))
    , testCase "a failing left branch propagates while the right one is still running" do
        failureWithin (runPair (parBO (failing 10 "left") (delayed 10_000 2)))
          >>= (@?= Just (Left "left"))
    , testCase "a failing right branch propagates while the left one is still running" do
        failureWithin (runPair (parBO (delayed 10_000 1) (failing 10 "right")))
          >>= (@?= Just (Left "right"))
    , testCase "when both branches fail, one of their exceptions propagates" do
        result <- failureWithin (runPair (parBO (failing 10 "left") (failing 10 "right")))
        assertBool
          ("unexpected result: " <> show result)
          (result `elem` [Just (Left "left"), Just (Left "right")])
    , testCase "a nested failure propagates through both levels" do
        let inner :: BO α Int
            inner = Data.fmap (Unsafe.toLinear fst) (parBO (delayed 10_000 1) (failing 10 "inner"))
        failureWithin (runPair (parBO inner (delayed 10_000 2)))
          >>= (@?= Just (Left "inner"))
    , testCase "a value interrupted by a timeout completes when forced again" do
        interrupted <- timeout 50_000 (evaluate slowPair)
        interrupted @?= Nothing
        performMajorGC
        resumed <- evaluate slowPair
        resumed @?= (1, 2)
    , testCase "a shared value forced by a stopped branch completes later" do
        let forceShared :: BO α Int
            forceShared = unsafeSystemIOToBO (evaluate sharedPair >> pure 0)
        failureWithin (runPair (parBO forceShared (failing 50 "boom")))
          >>= (@?= Just (Left "boom"))
        shared <- evaluate sharedPair
        shared @?= (3, 4)
    , testCase "branches of a dropped value run to completion" do
        ticks <- newIORef (0 :: Int)
        let count :: BO α Int
            count = unsafeSystemIOToBO (replicateM_ 20 (threadDelay 5_000 >> atomicModifyIORef' ticks \n -> (n + 1, ())) >> pure 0)
        void (timeout 10_000 (evaluate (runPair (parBO count (delayed 0 0)))))
        performMajorGC
        finished <- pollFor 5_000_000 ((== 20) <$> readIORef ticks)
        assertBool "the branch of the dropped value did not finish" finished
    , testCase "a re-raised stop counts as a failure of the branch that raises it" do
        void (failureWithin (runPair (parBO forcePoisoned (failing 50 "boom"))))
        -- The stop that interrupted the evaluation of the shared value is now its value.
        -- A branch forcing it re-raises that stop, which is not its sibling's, so the sibling must be stopped rather than awaited for ten seconds.
        -- The 'try' is outside the 'timeout', so that the timeout's own exception cannot pass for the failure.
        result <- try @SomeException (timeout 5_000_000 (evaluate (runPair (parBO forcePoisoned (delayed 10_000 2)))))
        case result of
          Left _ -> pure ()
          Right Nothing -> assertFailure "the sibling of the re-raising branch was awaited instead of stopped"
          Right (Just pair) -> assertFailure ("expected a failure, got " <> show pair)
    , testCase "two branches failing under uninterruptibleMask leave no thread stuck" do
        forM_ [1 .. 100 :: Int] \i -> do
          -- The index in the messages makes every iteration run a 'parBO' of its own: see 'recordThread'.
          result <- uninterruptibleMask_ (try (evaluate (runPair (parBO (failing 1 ("left " <> show i)) (failing 1 ("right " <> show i))))))
          case result of
            Left (ErrorCall message) -> assertBool ("a failure of another iteration: " <> message) (show i `isSuffixOf` message)
            Right pair -> assertFailure ("expected a failure, got " <> show pair)
        -- The branches may still be finishing their exchange of stops when the parent rethrows, so poll: a thread blocked in throwTo for good is what a deadlock would leave behind.
        settled <- pollFor 5_000_000 (null <$> threadsBlockedOnThrowTo)
        assertBool "branches are stuck throwing to each other" settled
    , testCase "no branch thread is kept alive once parBO has returned" do
        threads <- newIORef []
        forM_ [1 .. 200 :: Int] \i ->
          evaluate (runPair (parBO (recordThread threads i) (recordThread threads i)))
        recorded <- length <$> readIORef threads
        recorded @?= 400
        -- Nothing may keep a finished branch alive, so collecting must free every one.
        -- A finalizer that kept the threads would not show here, since the weak references die in the collection that queues it; the chains below catch one.
        settle threads
        alive <- aliveAfterCollecting threads
        alive @?= 0
    , testCase "a finished left branch is not kept alive while its sibling runs" do
        threads <- newIORef []
        gate <- newEmptyMVar
        -- Each level pairs a quick computation, first, with the rest of the chain, as 'Par' does over a list.
        let chain :: Int -> BO α Int
            chain 0 = unsafeSystemIOToBO (readMVar gate >> pure 0)
            chain n = Data.fmap (Unsafe.toLinear \(a, b) -> a + b) (parBO (recordThread threads 1) (chain (n - 1)))
        done <- newEmptyMVar
        _ <- forkIO (try @SomeException (evaluate (runPair (Data.fmap (\x -> (x, 0)) (chain 200)))) >>= putMVar done)
        waitForThreads threads 200
        settle threads
        alive <- aliveAfterCollecting threads
        putMVar gate ()
        void (takeMVar done)
        alive @?= 0
    , expectFailBecause "the left branch's handler keeps its reference to the right branch's thread until the left branch finishes" $
        testCase "a finished right branch is not kept alive while its sibling runs" do
          threads <- newIORef []
          gate <- newEmptyMVar
          let chain :: Int -> BO α Int
              chain 0 = unsafeSystemIOToBO (readMVar gate >> pure 0)
              chain n = Data.fmap (Unsafe.toLinear \(a, b) -> a + b) (parBO (chain (n - 1)) (recordThread threads 1))
          done <- newEmptyMVar
          _ <- forkIO (try @SomeException (evaluate (runPair (Data.fmap (\x -> (x, 0)) (chain 200)))) >>= putMVar done)
          waitForThreads threads 200
          settle threads
          alive <- aliveAfterCollecting threads
          putMVar gate ()
          void (takeMVar done)
          alive @?= 0
    , testCase "no branch reaches the uncaught-exception handler" do
        -- Branches are forked with fork#, whose threads never reach that handler whatever escapes them;
        -- this guards against a return to forkIO, whose threads do.
        uncaught <- newIORef (0 :: Int)
        previous <- getUncaughtExceptionHandler
        setUncaughtExceptionHandler \_ -> atomicModifyIORef' uncaught \n -> (n + 1, ())
        void (failureWithin (runPair (parBO (failing 10 "left") (delayed 10_000 2))))
        void (failureWithin (runPair (parBO (delayed 10_000 1) (failing 10 "right"))))
        void (failureWithin (runPair (parBO (failing 10 "left") (failing 10 "right"))))
        threadDelay 100_000
        setUncaughtExceptionHandler previous
        readIORef uncaught >>= (@?= 0)
    ]
