{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

{- | The Chase-Lev deque in which the work-stealing scheduler of "Control.Concurrent.DivideConquer.Linear" keeps its tasks.

Its owner pushes and pops at the front, and thieves take from the back.
Every element pushed must be taken exactly once: a lost task leaves the caller waiting forever, and a task taken twice runs the mutable borrows it carries twice.
Elements are forced where they are taken, since a slot the deque never wrote holds 'undefined'.
-}
module Control.Concurrent.Queue.ChaseLevSpec (
  module Control.Concurrent.Queue.ChaseLevSpec,
) where

import Control.Concurrent (forkOn, getNumCapabilities, killThread, newEmptyMVar, putMVar, takeMVar, yield)
import Control.Concurrent.Queue.ChaseLev
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forM, forM_, replicateM, replicateM_, when)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (group, sort)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import System.IO.Unsafe (unsafePerformIO)
import System.Random (StdGen, mkStdGen, randomR)
import System.Timeout (timeout)
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F
import Test.Tasty.HUnit

-- | An element as taken from the deque, forced: the message of the exception it raised, or its value.
type Taken = Either String Int

forced :: Int -> IO Taken
forced x = either (\e -> Left (show (e :: SomeException))) Right <$> try (evaluate x)

-- | Move top and bottom to @k@ on an empty deque, by pushing one element and stealing it back, @k@ times.
advance :: ChaseLevDeq Int -> Int -> IO ()
advance q k = replicateM_ k do
  pushFront q (-1)
  _ <- tryPopBack q
  pure ()

-- | Steal every element, oldest first.
stealAll :: ChaseLevDeq Int -> IO [Taken]
stealAll q =
  tryPopBack q >>= \case
    Just (Found x) -> (:) <$> forced x <*> stealAll q
    _ -> pure []

-- | Pop every element at the owner's end, newest first.
popAll :: ChaseLevDeq Int -> IO [Taken]
popAll q =
  tryPopFront q >>= \case
    Just (Just x) -> (:) <$> forced x <*> popAll q
    _ -> pure []

{- | With top and bottom moved to @start@, push @before@ elements, then @after@ more in one push that grows the deque.
Returns the deque and its capacities before and after the second push.
-}
grown :: Int -> Int -> Int -> IO (ChaseLevDeq Int, Int, Int)
grown start before after = do
  q <- newDeq
  advance q start
  pushFronts q [0 .. before - 1]
  c0 <- capacity q
  pushFronts q [before .. before + after - 1]
  c1 <- capacity q
  pure (q, c0, c1)

{- | A resize must put element @k@ where readers look for it, at @k@ modulo the new capacity.
Each case moves top to where a copy of the old array's slots, into the same slots of the new array, would put the elements elsewhere.
-}
test_resize :: TestTree
test_resize =
  testGroup
    "growing the deque keeps every element"
    [ testCase "after top has passed the capacity" do
        (q, c0, c1) <- grown 32 28 5
        assertBool "the deque did not grow" (c1 > c0)
        stealAll q >>= (@?= map Right [0 .. 32])
    , testCase "with the elements wrapped around the end of the array" do
        (q, c0, c1) <- grown 52 20 12
        assertBool "the deque did not grow" (c1 > c0)
        stealAll q >>= (@?= map Right [0 .. 31])
    , testCase "when one push more than doubles the capacity" do
        (q, c0, c1) <- grown 40 10 200
        assertBool "the capacity did not more than double" (c1 > 2 * c0)
        stealAll q >>= (@?= map Right [0 .. 209])
    , testCase "for the owner, who pops the newest first" do
        (q, _, _) <- grown 32 28 5
        popAll q >>= (@?= map Right [32, 31 .. 0])
    , testCase "when it grows twice" do
        (q, _, c1) <- grown 32 28 5
        pushFronts q [33 .. 72]
        c2 <- capacity q
        assertBool "the deque did not grow a second time" (c2 > c1)
        stealAll q >>= (@?= map Right [0 .. 72])
    ]

-- | An operation on the deque from a single thread.
data Op = Push Int | PopFront | PopBack | StealHalf
  deriving stock (Show)

genOp :: G.Gen Op
genOp =
  G.int (G.between (0, 4)) >>= \case
    0 -> Push <$> G.int (G.between (1, 70))
    1 -> Push <$> G.int (G.between (1, 3))
    2 -> pure PopFront
    3 -> pure PopBack
    _ -> pure StealHalf

-- | Run the operations on a new deque, pushing consecutive integers, and return what each took.
runOps :: [Op] -> [[Taken]]
{-# NOINLINE runOps #-}
runOps ops = unsafePerformIO do
  q <- newDeq
  next <- newIORef 0
  forM ops \case
    Push n -> do
      c <- readIORef next
      writeIORef next (c + n)
      pushFronts q [c .. c + n - 1]
      pure []
    PopFront ->
      tryPopFront q >>= \case
        Just (Just x) -> pure <$> forced x
        _ -> pure []
    PopBack ->
      tryPopBack q >>= \case
        Just (Found x) -> pure <$> forced x
        _ -> pure []
    StealHalf ->
      stealHalf q >>= \case
        Just (Found xs) -> traverse forced (NE.toList xs)
        _ -> pure []

-- | What 'runOps' must return: the deque as a sequence, oldest first.
model :: [Op] -> [[Taken]]
model = go Seq.empty 0
  where
    go _ _ [] = []
    go s c (op : ops) = case op of
      Push n -> [] : go (s <> Seq.fromList [c .. c + n - 1]) (c + n) ops
      PopFront -> case Seq.viewr s of
        rest Seq.:> x -> [Right x] : go rest c ops
        Seq.EmptyR -> [] : go s c ops
      PopBack -> case Seq.viewl s of
        x Seq.:< rest -> [Right x] : go rest c ops
        Seq.EmptyL -> [] : go s c ops
      StealHalf ->
        let avail = Seq.length s
            (taken, rest) = Seq.splitAt (if avail == 1 then 1 else avail `quot` 2) s
         in map Right (toList taken) : go rest c ops

test_sequential :: TestTree
test_sequential =
  testProperty "on one thread, the deque takes what a sequence would" do
    ops <- F.gen $ G.list (G.between (0, 300)) genOp
    F.assert $ P.expect (model ops) P..$ ("deque", runOps ops)

-- | How a thief takes elements from the back: by halves, one at a time, or alternately.
data Thief = Halves | Singles | Mixed
  deriving stock (Show)

-- | What one attempt of a thief gave.
data Attempt = Took [Int] | Retry | Idle

{- | Take elements with the given method until the owner is done and the deque is empty.

A thief that finds nothing yields: a loop that does not allocate cannot be stopped for a garbage collection, which then waits for it forever.
One that lost a race retries at once, since another thread's CAS is needed to make it lose again.
-}
thief :: Thief -> ChaseLevDeq Int -> IORef Bool -> IO [Taken]
thief how q done = go (0 :: Int) []
  where
    go k acc =
      attempt k >>= \case
        Took xs -> traverse forced xs >>= \ys -> go (k + 1) (ys <> acc)
        Retry -> go (k + 1) acc
        Idle ->
          readIORef done >>= \case
            True -> pure acc
            False -> yield >> go (k + 1) acc
    attempt k = case how of
      Halves -> halves
      Singles -> single
      Mixed -> if even k then halves else single
    halves =
      stealHalf q >>= \case
        Just (Found xs) -> pure (Took (NE.toList xs))
        Just Race -> pure Retry
        _ -> pure Idle
    single =
      tryPopBack q >>= \case
        Just (Found x) -> pure (Took [x])
        Just Race -> pure Retry
        _ -> pure Idle

{- | Run an owner against thieves on a new deque, the owner on the first capability and each thief on one of its own where there are enough.
The owner then takes what is left; the result is everything taken, or 'Nothing' when that took more than 10 seconds.
-}
race :: Int -> Thief -> (ChaseLevDeq Int -> IO [Taken]) -> IO (Maybe [Taken])
race thieves how owner = do
  q <- newDeq
  done <- newIORef False
  caps <- getNumCapabilities
  outs <- forM [1 .. thieves] \i -> do
    out <- newEmptyMVar
    tid <- forkOn (i `mod` caps) (thief how q done >>= putMVar out)
    pure (tid, out)
  ownerOut <- newEmptyMVar
  ownerThread <- forkOn 0 (owner q >>= putMVar ownerOut)
  let collect = do
        mine <- takeMVar ownerOut
        writeIORef done True
        rest <- popAll q
        stolen <- traverse (takeMVar . snd) outs
        pure (mine <> rest <> concat stolen)
  -- Stop every thread on a timeout too, so that none goes on spinning through the rest of the suite.
  timeout 10_000_000 collect `finally` do
    writeIORef done True
    mapM_ killThread (ownerThread : map fst outs)

-- | Push the integers below @total@ in batches of 1 to 24, and pop a random number of each batch back at once.
pushAndPop :: Int -> StdGen -> ChaseLevDeq Int -> IO [Taken]
pushAndPop total gen0 q = go 0 gen0 []
  where
    go next gen acc
      | next >= total = pure acc
      | otherwise = do
          let (size, gen1) = randomR (1, 24) gen
              (pops, gen2) = randomR (0, size) gen1
              end = min total (next + size)
          pushFronts q [next .. end - 1]
          popped <- replicateM pops (tryPopFront q)
          ys <- traverse forced [x | Just (Just x) <- popped]
          go end gen2 (ys <> acc)

{- | Push the integers below @total@ in batches of 1 to 8, after half of the batches steal half of the deque's own elements, as the scheduler does to feed a sleeping worker, and then pop a random number of them back at once.
Batches this small keep the deque nearly empty, so that thieves take elements just after they are written.
-}
pushStealAndPop :: Int -> StdGen -> ChaseLevDeq Int -> IO [Taken]
pushStealAndPop total gen0 q = go 0 gen0 []
  where
    go next gen acc
      | next >= total = pure acc
      | otherwise = do
          let (size, gen1) = randomR (1, 8) gen
              (steal, gen2) = randomR (False, True) gen1
              (pops, gen3) = randomR (0, size) gen2
              end = min total (next + size)
          pushFronts q [next .. end - 1]
          stolen <- if steal then stealOwn else pure []
          popped <- replicateM pops (tryPopFront q)
          ys <- traverse forced (stolen <> [x | Just (Just x) <- popped])
          go end gen3 (ys <> acc)
    stealOwn =
      stealHalf q >>= \case
        Just (Found xs) -> pure (NE.toList xs)
        Just Race -> stealOwn
        _ -> pure []

-- | Push the integers below @total@ one at a time and never pop, so that the thieves keep the deque nearly empty and take each element just after it is written.
pushOnly :: Int -> ChaseLevDeq Int -> IO [Taken]
pushOnly total q = [] <$ mapM_ (pushFront q) [0 .. total - 1]

-- | Push 40 integers one at a time, which the thieves take as they come and so move top past the capacity, then 40 more at once, which grow the deque.
pushThenGrow :: ChaseLevDeq Int -> IO [Taken]
pushThenGrow q = do
  mapM_ (pushFront q) [0 .. 39]
  pushFronts q [40 .. 79]
  pure []

-- | Report the elements taken twice, not at all, or wrong.
exactlyOnce :: Int -> [Taken] -> Assertion
exactlyOnce total taken = do
  let values = sort [x | Right x <- taken]
      twice = [x | x : _ : _ <- group values]
      missing = Set.size (Set.fromList [0 .. total - 1] `Set.difference` Set.fromList values)
      wrong = [e | Left e <- taken] <> [show x | x <- values, x < 0 || x >= total]
  assertBool
    ( "taken twice: "
        <> show (take 10 twice)
        <> ", missing: "
        <> show missing
        <> ", wrong: "
        <> show (take 10 wrong)
    )
    (null twice && missing == 0 && null wrong)

-- | Run the races, and check that each took every element below @total@ exactly once.
allExactlyOnce :: Int -> [IO (Maybe [Taken])] -> Assertion
allExactlyOnce total races = do
  results <- sequence races
  case sequence results of
    Nothing -> assertFailure "an owner or a thief did not finish within 10 seconds"
    Just runs -> mapM_ (exactlyOnce total) runs

{- | Every element pushed is taken exactly once, by the owner or by one thief.

Each case targets one way the deque has failed.
A thief that claimed several elements with one CAS took elements that the owner had popped without one, when the owner pushed and popped in small batches.
A thief whose read of a slot was not ordered after its read of @bottom@ took what the slot held before the owner's write, when the owner pushed into a nearly empty deque, on a weakly ordered machine such as ARM64; it did so most often when the owner also stole from its own deque, as the scheduler does.
A resize that put elements in the wrong slots handed out the new array's filler.
These cases catch the first in every run, and the resize cases above the last; the second cannot happen on x86-64, and on an ARM64 Apple M4 these cases caught it in about two runs of three.
-}
test_concurrent :: TestTree
test_concurrent =
  testGroup
    "an owner and thieves take every element exactly once"
    [ testCase "when the owner pushes and pops in batches while thieves steal halves" do
        allExactlyOnce 50_000 [race 4 Halves (pushAndPop 50_000 (mkStdGen seed)) | seed <- [1 .. 8]]
    , testCase "when the owner pushes, steals half of its own deque and pops, as the scheduler does, while thieves steal" do
        allExactlyOnce 200_000 [race 8 Mixed (pushStealAndPop 200_000 (mkStdGen seed)) | seed <- [1 .. 4]]
    , testCase "when the owner only pushes while thieves steal halves" do
        allExactlyOnce 100_000 (replicate 4 (race 8 Halves (pushOnly 100_000)))
    , testCase "when the owner only pushes while thieves take one element at a time" do
        allExactlyOnce 100_000 (replicate 4 (race 8 Singles (pushOnly 100_000)))
    , testCase "when the deque grows while thieves take one element at a time" do
        allExactlyOnce 80 (replicate 500 (race 8 Singles pushThenGrow))
    ]

{- | A thief of a deque that its owner closes meanwhile: it alternates 'stealHalf' and 'tryPopBack', and stops at the first 'Nothing'.
Returns what it took, and whether it got 'Nothing' before the owner had closed the deque.
-}
closingThief :: ChaseLevDeq Int -> IORef Bool -> IO ([Taken], Bool)
closingThief q closedByOwner = go (0 :: Int) []
  where
    go k acc =
      attempt k >>= \case
        Nothing -> (,) acc . not <$> readIORef closedByOwner
        Just [] -> yield >> go (k + 1) acc
        Just xs -> traverse forced xs >>= \ys -> go (k + 1) (ys <> acc)
    attempt k
      | even k =
          stealHalf q >>= \case
            Nothing -> pure Nothing
            Just (Found xs) -> pure (Just (NE.toList xs))
            Just _ -> pure (Just [])
      | otherwise =
          tryPopBack q >>= \case
            Nothing -> pure Nothing
            Just (Found x) -> pure (Just [x])
            Just _ -> pure (Just [])

{- | The owner pushes 60 batches of 1 to 70 integers, some large enough to grow the deque, and pops up to 3 back after each, while 8 thieves steal; it closes the deque after a random batch and goes on pushing, then takes what is left.
Returns the integers pushed before 'close', everything taken, and how many thieves got 'Nothing' before 'close'; or 'Nothing' when that took more than 10 seconds.
-}
closeWhileStealing :: StdGen -> IO (Maybe ([Int], [Taken], Int))
closeWhileStealing gen0 = do
  q <- newDeq
  closedByOwner <- newIORef False
  caps <- getNumCapabilities
  outs <- forM [1 .. 8 :: Int] \i -> do
    out <- newEmptyMVar
    tid <- forkOn (i `mod` caps) (closingThief q closedByOwner >>= putMVar out)
    pure (tid, out)
  let (closeAt, gen1) = randomR (0, 40 :: Int) gen0
      step s gen next before acc
        | s >= (60 :: Int) = pure (before, acc)
        | otherwise = do
            let (size, gen2) = randomR (1, 70) gen
                (pops, gen3) = randomR (0, 3 :: Int) gen2
                batch = [next .. next + size - 1]
            wasClosed <- readIORef closedByOwner
            pushFronts q batch
            when (s == closeAt) do
              writeIORef closedByOwner True
              close q
            popped <- replicateM pops (tryPopFront q)
            ys <- traverse forced [x | Just (Just x) <- popped]
            step (s + 1) gen3 (next + size) (if wasClosed then before else batch <> before) (ys <> acc)
      run = do
        (before, mine) <- step 0 gen1 0 [] []
        rest <- popAll q
        stolen <- traverse (takeMVar . snd) outs
        pure (before, mine <> rest <> concatMap fst stolen, length (filter snd stolen))
  -- Stop the thieves on a timeout too, so that none goes on spinning through the rest of the suite.
  timeout 10_000_000 run `finally` mapM_ (killThread . fst) outs

-- | After 'close', the deque drops what is pushed, hands out what was pushed before exactly once, and gives thieves 'Nothing' only once it is empty.
test_close :: TestTree
test_close =
  testCase "a deque closed while thieves steal hands out exactly what was pushed before it" do
    forM_ [1 .. 200 :: Int] \seed ->
      closeWhileStealing (mkStdGen seed) >>= \case
        Nothing -> assertFailure "an owner or a thief did not finish within 10 seconds"
        Just (before, taken, early) -> do
          let values = sort [x | Right x <- taken]
              wrong = [e | Left e <- taken]
          assertBool
            ( "seed "
                <> show seed
                <> ": taken "
                <> show (length values)
                <> " of "
                <> show (length before)
                <> ", undefined "
                <> show (length wrong)
                <> ", Nothing before close "
                <> show early
            )
            (values == sort before && null wrong && early == 0)
