{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Owners handed back after a scope, one kernel per path, built at the suite's @-O2@.

Each kernel duplicates a reference before the scope, captures the first copy in the scope, bumps it through a borrow, reclaims it in the scope's 'After' (or the path's equivalent) and frees it; the second copy is freed as is.
Each is expected to return @(1, 0)@.

Every kernel comes in two flavours.
'realDup' duplicates with the library's 'dup2'.
'oldDup' is a verbatim copy of the 0.1.0.0 'Dupable (Ref a)': a pure read of the reference that hands the same reference back, standing for any owner-level operation of that shape.
With it, a free of the reclaimed owner is syntactically the read 'oldDup' made before the scope, so only the barrier inside 'reclaim' keeps the two apart.
See Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal".

@PureBorrow.NoStateHack.Owners@, in the @pure-borrow-no-state-hack@ test suite, is a copy of this module built with @-fno-state-hack@; keep the two copies identical.
-}
module Control.Monad.Borrow.Pure.OrderingSpec.Owners (
  ownerKernels,
  earlyReclaim,
  earlyReclaimWithDict,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.Experimental.Borrows qualified as Borrows
import Control.Monad.Borrow.Pure.Lifetime.Token (EndToken, withEnd)
import Control.Syntax.DataFlow qualified as DataFlow
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import GHC.Exts (withDict)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NonLinear

type Duper = Ref.Ref Int %1 -> Linearly %1 -> (Ref.Ref Int, Ref.Ref Int)

realDup :: Duper
{-# INLINE realDup #-}
realDup r l = l `lseq` dup2 r

oldDup :: Duper
{-# INLINE oldDup #-}
oldDup = Unsafe.toLinear2 \r l -> case Ref.free r of !x -> (r, Ref.new x l)

bump :: Mut α (Ref.Ref Int) %1 -> BO α ()
bump m = consume Control.<$> RefB.modify (+ 1) m

viaRunBO :: Duper -> (Int, Int)
{-# INLINE viaRunBO #-}
viaRunBO d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  runBO l2 Control.do
    (m, lend) <- borrowM r1
    bump m
    pureAfter (Ref.free (reclaim lend), Ref.free r2)

viaReborrowing' :: Duper -> (Int, Int)
{-# INLINE viaReborrowing' #-}
viaReborrowing' d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    (x, md) <- reborrowing' md \md' -> Control.do
      (m, lend) <- borrowM r1
      bump m
      Control.pure (consume md' `lseq` upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    Control.pure (consume md)
    pureAfter (consume (reclaim lendD) `lseq` (x, Ref.free r2))

viaSharing' :: Duper -> (Int, Int)
{-# INLINE viaSharing' #-}
viaSharing' d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    (x, md) <- sharing' md \_ -> Control.do
      (m, lend) <- borrowM r1
      bump m
      Control.pure (upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    Control.pure (consume md)
    pureAfter (consume (reclaim lendD) `lseq` (x, Ref.free r2))

viaSrunBO :: Duper -> (Int, Int)
{-# INLINE viaSrunBO #-}
viaSrunBO d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  runBO l2 Control.do
    x <- srunBO Control.do
      (m, lend) <- borrowM r1
      bump m
      Control.pure (upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    pureAfter (x, Ref.free r2)

viaReborrowings' :: Duper -> (Int, Int)
{-# INLINE viaReborrowings' #-}
viaReborrowings' d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, lin) <- dup lin
  (l2, l3) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  dummy <- Ref.new (0 :: Int) l3
  runBO l2 Control.do
    (md, lendD) <- borrowM dummy
    (x, bundle) <- Borrows.reborrowings' (md Borrows.:- Borrows.BNil) \(md' Borrows.:- Borrows.BNil) -> Control.do
      (m, lend) <- borrowM r1
      bump m
      Control.pure (consume md' `lseq` upcast @_ @(After _ Int) (Ref.free Control.<$> reclaim' lend))
    case bundle of
      md Borrows.:- Borrows.BNil -> Control.do
        Control.pure (consume md)
        pureAfter (consume (reclaim lendD) `lseq` (x, Ref.free r2))

viaRunBOLend :: Duper -> (Int, Int)
{-# INLINE viaRunBOLend #-}
viaRunBOLend d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  r1 <- runBOLend l2 Control.do
    (m, lend) <- borrowM r1
    bump m
    Control.pure lend
  (Ref.free r1, Ref.free r2)

viaModifyLinearOnly :: Duper -> (Int, Int)
{-# INLINE viaModifyLinearOnly #-}
viaModifyLinearOnly d = linearly \lin -> DataFlow.do
  (l0, l1) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  r1 <- modifyLinearOnlyBO_ r1 bump
  (Ref.free r1, Ref.free r2)

viaModifyBO :: Duper -> (Int, Int)
{-# INLINE viaModifyBO #-}
viaModifyBO d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  r1 <- modifyBO_ r1 l2 bump
  (Ref.free r1, Ref.free r2)

viaAfterBind :: Duper -> (Int, Int)
{-# INLINE viaAfterBind #-}
viaAfterBind d = linearly \lin -> DataFlow.do
  (l0, lin) <- dup lin
  (l1, l2) <- dup lin
  r0 <- Ref.new (0 :: Int) l0
  (r1, r2) <- d r0 l1
  runBO l2 Control.do
    (m, lend) <- borrowM r1
    bump m
    Control.pure (reclaim' lend Control.>>= \r -> Control.pure (Ref.free r, Ref.free r2))

realRunBO, oldRunBO, realReborrowing', oldReborrowing', realSharing', oldSharing' :: (Int, Int)
realSrunBO, oldSrunBO, realReborrowings', oldReborrowings', realRunBOLend, oldRunBOLend :: (Int, Int)
realModifyLinearOnly, oldModifyLinearOnly, realModifyBO, oldModifyBO, realAfterBind, oldAfterBind :: (Int, Int)
{-# NOINLINE realRunBO #-}
{-# NOINLINE oldRunBO #-}
{-# NOINLINE realReborrowing' #-}
{-# NOINLINE oldReborrowing' #-}
{-# NOINLINE realSharing' #-}
{-# NOINLINE oldSharing' #-}

{-# NOINLINE realSrunBO #-}

{-# NOINLINE oldSrunBO #-}

{-# NOINLINE realReborrowings' #-}

{-# NOINLINE oldReborrowings' #-}

{-# NOINLINE realRunBOLend #-}

{-# NOINLINE oldRunBOLend #-}

{-# NOINLINE realModifyLinearOnly #-}

{-# NOINLINE oldModifyLinearOnly #-}

{-# NOINLINE realModifyBO #-}

{-# NOINLINE oldModifyBO #-}

{-# NOINLINE realAfterBind #-}

{-# NOINLINE oldAfterBind #-}

realRunBO = viaRunBO realDup

oldRunBO = viaRunBO oldDup

realReborrowing' = viaReborrowing' realDup

oldReborrowing' = viaReborrowing' oldDup

realSharing' = viaSharing' realDup

oldSharing' = viaSharing' oldDup

realSrunBO = viaSrunBO realDup

oldSrunBO = viaSrunBO oldDup

realReborrowings' = viaReborrowings' realDup

oldReborrowings' = viaReborrowings' oldDup

realRunBOLend = viaRunBOLend realDup

oldRunBOLend = viaRunBOLend oldDup

realModifyLinearOnly = viaModifyLinearOnly realDup

oldModifyLinearOnly = viaModifyLinearOnly oldDup

realModifyBO = viaModifyBO realDup

oldModifyBO = viaModifyBO oldDup

realAfterBind = viaAfterBind realDup

oldAfterBind = viaAfterBind oldDup

-- | Every kernel with the path it goes through and the duplication it uses.
ownerKernels :: [(NonLinear.String, (Int, Int))]
ownerKernels =
  [ ("runBO, dup2", realRunBO)
  , ("runBO, old dup2", oldRunBO)
  , ("reborrowing', dup2", realReborrowing')
  , ("reborrowing', old dup2", oldReborrowing')
  , ("sharing', dup2", realSharing')
  , ("sharing', old dup2", oldSharing')
  , ("srunBO, dup2", realSrunBO)
  , ("srunBO, old dup2", oldSrunBO)
  , ("reborrowings', dup2", realReborrowings')
  , ("reborrowings', old dup2", oldReborrowings')
  , ("runBOLend, dup2", realRunBOLend)
  , ("runBOLend, old dup2", oldRunBOLend)
  , ("modifyLinearOnlyBO_, dup2", realModifyLinearOnly)
  , ("modifyLinearOnlyBO_, old dup2", oldModifyLinearOnly)
  , ("modifyBO_, dup2", realModifyBO)
  , ("modifyBO_, old dup2", oldModifyBO)
  , ("After's >>=, dup2", realAfterBind)
  , ("After's >>=, old dup2", oldAfterBind)
  ]

bumpBy :: Int -> Mut α (Ref.Ref Int) %1 -> BO α (Mut α (Ref.Ref Int))
bumpBy n = RefB.modify (+ n)

{- | A reclaim discharged through 'withEnd' with a bottom 'EndToken', which anyone can write, while the borrow is still live.

'withEnd' leaves the token alone, so it is 'reclaim' that must force it.
Were the token not forced, the owner would come back at once and a second 'Mut' borrowed from it could write the same cell as the first, giving @11@.
It must throw instead.
-}
earlyReclaim :: Int
{-# NOINLINE earlyReclaim #-}
earlyReclaim = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r <- Ref.new (0 :: Int) l1
  runBO l2 Control.do
    (m1, lend1) <- borrowM r
    (m2, lend2) <- borrowM (withEnd NonLinear.undefined (reclaim' lend1))
    m1 <- bumpBy 1 m1
    m2 <- bumpBy 10 m2
    Control.pure (consume m1 `lseq` consume m2)
    pureAfter (Ref.free (reclaim lend2))

{- | The same through GHC's own 'withDict', which bypasses 'withEnd': 'reclaim' forces the token itself.

'withDict' is outside the library's guarantee, since GHC classifies it as unsafe (see Note [Sealing classes behind synonyms] in "Data.Coerce.Directed.Internal"); this only pins that 'reclaim', not 'withEnd', does the forcing.
-}
earlyReclaimWithDict :: Int
{-# NOINLINE earlyReclaimWithDict #-}
earlyReclaimWithDict = linearly \lin -> DataFlow.do
  (l1, l2) <- dup lin
  r <- Ref.new (0 :: Int) l1
  runBO l2 Control.do
    (m1, lend1) <- borrowM r
    (m2, lend2) <- borrowM (early lend1)
    m1 <- bumpBy 1 m1
    m2 <- bumpBy 10 m2
    Control.pure (consume m1 `lseq` consume m2)
    pureAfter (Ref.free (reclaim lend2))
  where
    early :: forall α a. Lend α a %1 -> a
    early = withDict @(End α) (NonLinear.undefined :: EndToken α) (reclaim @α @a)
