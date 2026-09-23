{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -dsuppress-coercions #-}
{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}
{-# OPTIONS_GHC -dsuppress-type-applications #-}
{-# OPTIONS_GHC -dsuppress-type-signatures #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}

{- |
The barriers of Note [Owners handed back by reclaim] in "Control.Monad.Borrow.Pure.BO.Internal" that every run and every 'reclaim' goes through must survive as calls in optimized Core.

Each barrier is @OPAQUE@; made @INLINE@, or replaced by a @case@, it would vanish from the Core of its callers, and with it the ordering it provides.
Made @NOINLINE@, it would stay a call, but worker/wrapper would split it into a worker that drops its token, which the obligations here also catch: they name the barrier itself, which its callers then no longer use.
So each obligation here states that a probe /uses/ its barrier: it is a 'doesNotUse' that is expected to fail.
An expected failure passes whatever made the obligation fail, including a probe that inspection-testing cannot inspect at all, such as one imported from another module; the obligation that the probe uses no 'unmentioned' name must therefore pass, and turns the group red if the probe stops being inspectable.
"PureBorrow.Inspection.Sublifetime" states the same for the delimiters' own barriers, next to its probes.
-}
module PureBorrow.Inspection.Barriers (tests) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Internal (reviveNow, reviveOwner)
import Control.Monad.Borrow.Pure.Lifetime.Token (endLifetime)
import Data.Ref.Linear (Ref)
import Data.Ref.Linear qualified as Ref
import Data.Ref.Linear.Borrow qualified as RefB
import Prelude.Linear
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Inspection

-- | A run that bumps a reference through a borrow and reclaims its owner: 'runBO' ends a lifetime, and 'reclaim' hands the owner back.
runBOReclaimAt :: Ref Int %1 -> Linearly %1 -> Int
{-# NOINLINE runBOReclaimAt #-}
runBOReclaimAt ref lin = runBO lin Control.do
  (m, lend) <- borrowM ref
  bumped <- RefB.modify (+ 1) m
  Control.pure (consume bumped)
  pureAfter (Ref.free (reclaim lend))

-- | A name that nothing mentions, for the obligation that says the probe can be inspected at all.
unmentioned :: ()
{-# NOINLINE unmentioned #-}
unmentioned = ()

-- | Turn a 'doesNotUse' obligation into the statement that the probe uses the name.
uses :: TestTree -> TestTree
uses = expectFailBecause "the probe must call the barrier"

tests :: TestTree
tests =
  testGroup
    "barriers survive as calls"
    [ $( inspectTest
           ( (doesNotUse 'runBOReclaimAt 'unmentioned)
               { testName = Just "the probe can be inspected"
               }
           )
       )
    , uses
        $( inspectTest
             ( (doesNotUse 'runBOReclaimAt 'reviveOwner)
                 { testName = Just "reclaim hands the owner back through reviveOwner"
                 }
             )
         )
    , uses
        $( inspectTest
             ( (doesNotUse 'runBOReclaimAt 'reviveNow)
                 { testName = Just "runBO hands its Now back through reviveNow"
                 }
             )
         )
    , uses
        $( inspectTest
             ( (doesNotUse 'runBOReclaimAt 'endLifetime)
                 { testName = Just "runBO ends the lifetime through endLifetime"
                 }
             )
         )
    ]
