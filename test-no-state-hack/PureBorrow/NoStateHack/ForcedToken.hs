{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -fno-state-hack #-}

{- | User code that forces the 'EndToken' before discharging an 'After' with it, built with @-fno-state-hack@.

With a nullary 'EndToken', the 'srunForced' kernels here returned @(0, 0)@ and 'givenAway' handed back a freed reference, whether 'withEnd' forced the token or not.

'endLifetime' returns the token unrestricted, so a user may 'seq' it.
With a nullary token, GHC then knows the token is the constant constructor, and the dependency on the opaque call that produced it may be lost.
Each kernel duplicates a reference before the scope, bumps the first copy through a borrow inside the scope, and reads the reclaimed owner in the 'After'.
Each is expected to return @(1, 0)@.
Only safe modules are imported.
-}
module PureBorrow.NoStateHack.ForcedToken (kernels, givenAway) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO (execBO, sexecBO)
import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken, SomeNow (..), endLifetime, newLifetime, newLifetime', withEnd)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

type Duper = Ref.Ref Int %1 -> Linearly %1 -> (Ref.Ref Int, Ref.Ref Int)

realDup :: Duper
{-# INLINE realDup #-}
realDup r l = l `lseq` dup2 r

-- | The 0.1.0.0 'Dupable (Ref a)': a pure read of the reference that hands the same reference back.
oldDup :: Duper
{-# INLINE oldDup #-}
oldDup = Unsafe.toLinear2 \r l -> case Ref.free r of !x -> (r, Ref.new x l)

bump :: Mut α (Ref.Ref Int) %1 -> BO α ()
bump m = consume Control.<$> RefB.modify (+ 1) m

-- | 'runBO' as a user could write it, forcing the token before 'withEnd'.
runForced :: forall a. Linearly %1 -> (forall α. BO α (After α a)) %1 -> a
{-# INLINE runForced #-}
runForced lin bo = case newLifetime lin of
  MkSomeNow now -> case execBO bo now of
    (now, f) -> case endLifetime now of
      Ur end -> forceToken end (\end' -> withEnd end' f)

forceToken :: EndToken α -> (EndToken α -> r) %1 -> r
{-# INLINE forceToken #-}
forceToken !end k = k end

viaForced :: Duper -> (Int, Int)
{-# INLINE viaForced #-}
viaForced d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  runForced l2 Control.do
    (m, lend) <- borrowM r1
    bump m
    pureAfter (Ref.free (reclaim lend), Ref.free r2)

forcedReal, forcedOld :: (Int, Int)
{-# NOINLINE forcedReal #-}
forcedReal = viaForced realDup
{-# NOINLINE forcedOld #-}
forcedOld = viaForced oldDup

-- | The same, with the result read through a second, independent pure read before the scope.
viaForcedTwice :: Duper -> (Int, Int)
{-# INLINE viaForcedTwice #-}
viaForcedTwice d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  runForced l2 Control.do
    (m, lend) <- borrowM r1
    bump m
    Control.pure (Control.fmap (\r -> (Ref.free r, Ref.free r2)) (reclaim' lend))

forcedTwiceReal, forcedTwiceOld :: (Int, Int)
{-# NOINLINE forcedTwiceReal #-}
forcedTwiceReal = viaForcedTwice realDup
{-# NOINLINE forcedTwiceOld #-}
forcedTwiceOld = viaForcedTwice oldDup

-- | The +slow 'srunBO', as a user could write it from safe modules, forcing the token and the result.
srunForced :: (forall α. BO (α /\ β) (After α a)) %1 -> BO β a
{-# INLINE srunForced #-}
srunForced bo = asksLinearlyM \lin -> newLifetime' lin \now -> Control.do
  (now, f) <- sexecBO bo now
  Ur end <- Control.pure (endLifetime now)
  Control.pure $! forceToken end (\end' -> withEnd end' f)

viaSrunForced :: Duper -> (Int, Int)
{-# INLINE viaSrunForced #-}
viaSrunForced d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  runBO l2 Control.do
    x <- srunForced Control.do
      (m, lend) <- borrowM r1
      bump m
      Control.pure (upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    pureAfter (x, Ref.free r2)

srunForcedReal, srunForcedOld :: (Int, Int)
{-# NOINLINE srunForcedReal #-}
srunForcedReal = viaSrunForced realDup
{-# NOINLINE srunForcedOld #-}
srunForcedOld = viaSrunForced oldDup

{- | 'ownerGivenAway' of "PureBorrow.NoStateHack.Scopes" through 'srunForced': take the inner reference out of the outer one inside the scope, bump and free it there, and read the reclaimed outer one in the 'After'.

Expected @(100, 0)@; a stale read returns the freed inner reference's contents, 1.
-}
givenAwayForced :: (Int, Int)
{-# NOINLINE givenAwayForced #-}
givenAwayForced = linearly \lin -> DataFlow.do
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  inner <- Ref.new (0 :: Int) l1
  outer <- Ref.new inner l2
  runBO l3 Control.do
    current <- srunForced Control.do
      (m, lend) <- borrowM outer
      fresh <- asksLinearly (Ref.new (100 :: Int))
      (taken, m) <- RefB.update (\old -> Control.pure (old, fresh)) m
      Control.pure (consume m)
      taken <- asksLinearly \l -> modifyBO_ taken l bump
      Control.pure (consume taken)
      Control.pure (upcast @_ @(After _ (Ref.Ref Int)) (readOuter Control.<$> reclaim' lend))
    pureAfter (Ref.free current, 0)
  where
    readOuter :: Ref.Ref (Ref.Ref Int) %1 -> Ref.Ref Int
    readOuter outer' = case Ref.free outer' of !cur -> cur

kernels :: [(NonLinear.String, (Int, Int))]
kernels =
  [ ("forced token, runBO shape, real dup2", forcedReal)
  , ("forced token, runBO shape, old dup2", forcedOld)
  , ("forced token, reclaim' shape, real dup2", forcedTwiceReal)
  , ("forced token, reclaim' shape, old dup2", forcedTwiceOld)
  , ("forced token, user srunBO, real dup2", srunForcedReal)
  , ("forced token, user srunBO, old dup2", srunForcedOld)
  ]

-- | Expected (100, 0).
givenAway :: (Int, Int)
givenAway = givenAwayForced
