{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SubtypeVariance where

import Control.Monad.Borrow.Pure.BO (After, BO, Lend, Mut, Share)
import Control.Monad.Borrow.Pure.Lifetime (Static)
import Control.Monad.Borrow.Pure.Lifetime.Internal (Lifetime (Al))
import Data.Coerce.Directed (upcast, upcast1)
import Generics.Linear (Generically1)
import Generics.Linear.TH (deriveGenericAnd1)

data MV α a = MV (Mut α a) Int

deriveGenericAnd1 ''MV

-- This fixture must not typecheck: each binding below would move a lifetime the wrong way, or the contents of a Mut.
-- Each error sits in the context of an instance that resolution picks, which no deferred evaluation reaches.
-- Each binding has a lifetime of its own, since GHC reports two identical unsolved constraints only once.
-- EXPECT: <=!!
-- EXPECT: Mut is invariant in the type it borrows
-- EXPECT: badBO = upcast1
-- EXPECT: badShare = upcast1
-- EXPECT: badLend = upcast1
-- EXPECT: badAfter = upcast1
-- EXPECT: badFunArgument = upcast1
-- EXPECT: badFunResult = upcast1
-- EXPECT: badElement = upcast
-- EXPECT: badMutContent = upcast1

-- A computation of a shorter lifetime would run as one of the static lifetime.
badBO :: BO (Al 0) Int %1 -> BO Static Int
badBO = upcast1

-- A shared borrow would outlive its lifetime.
badShare :: Share (Al 1) Int %1 -> Share Static Int
badShare = upcast1

-- A lender would be reclaimed before the static lifetime ends.
badLend :: Lend Static Int %1 -> Lend (Al 2) Int
badLend = upcast1

-- A finalizer would be discharged before the static lifetime ends.
badAfter :: After Static Int %1 -> After (Al 3) Int
badAfter = upcast1

-- A function that needs a static borrow would accept one of a shorter lifetime.
badFunArgument :: (Mut Static Int %1 -> Int) %1 -> (Mut (Al 4) Int %1 -> Int)
badFunArgument = upcast1

-- A function returning a borrow of a shorter lifetime would claim to return a static one.
badFunResult :: (Int %1 -> Mut (Al 5) Int) %1 -> (Int %1 -> Mut Static Int)
badFunResult = upcast1

-- A lender under a Share would be reclaimable before the static lifetime ends.
badElement :: Share Static (Lend Static Int) %1 -> Generically1 (Share Static) (Lend (Al 6) Int)
badElement = upcast

-- A Mut of a static Share would let a Share of a shorter lifetime be written through it.
badMutContent :: MV α (Share Static Int) %1 -> Generically1 (MV α) (Share α Int)
badMutContent = upcast1
