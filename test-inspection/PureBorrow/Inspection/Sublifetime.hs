{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- The set inspection-testing's own "Help, I am drowning in Core!" recommends.
-- A failing obligation prints the Core the way GHC would, and without these a
-- single 'BO' term is mostly coercions, uniques and IdInfo.
-- These stay here rather than in the component's @ghc-options@: `cabal check`
-- rejects `-d*` flags in a distributed package.
{-# OPTIONS_GHC -dsuppress-coercions #-}
{-# OPTIONS_GHC -dsuppress-idinfo #-}
{-# OPTIONS_GHC -dsuppress-module-prefixes #-}
{-# OPTIONS_GHC -dsuppress-type-applications #-}
{-# OPTIONS_GHC -dsuppress-type-signatures #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}

{- |
Core-level obligations for the statically erased sublifetime delimiters.

With the @slow@ flag off, 'srunBO_' must compile to the identity, and 'srunBO' to nothing beyond handing the runtime-erased 'EndToken' to the 'After' the delimited action returned.
In particular neither may retain a runtime lifetime token, nor the 'Linearly' witness that 'newLifetime' consumes to produce one.
This has to hold through 'reborrowing'' and 'sharing'' too, which are the public combinators built on 'srunBO'.

The @+slow@ build restores the token-allocating implementations, so every obligation here is inverted there rather than dropped; see "PureBorrow.Inspection.Flags".
-}
module PureBorrow.Inspection.Sublifetime (
  tests,
  srunBOAt,
  endTokenAt,
  srunBO_At,
  idBOAt,
  reborrowingRefAt,
  sharingRefAt,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (EndToken (..))
import Data.Ref.Linear (Ref)
import Data.Ref.Linear.Borrow qualified as Ref
import Prelude.Linear
import PureBorrow.Inspection.Flags (expectFailIfBecause, isSlowAPI)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection
import Prelude qualified as NonLinear

{- | 'srunBO' at a concrete carrier.

The rank-2 argument is kept, so whatever survives here survives because the delimiter emitted it, not because a caller's action was specialised.

The @'Control.fmap' (+ 1)@ is not part of what is being measured; it is there to stop GHC from eta-reducing the probe.
Both @srunBOAt = srunBO@ and @srunBOAt bo = srunBO bo@ collapse to a bare reference to 'srunBO', which is an unsaturated call, so the @INLINE@ never fires.
Every obligation below would then hold vacuously, of a probe containing no delimiter at all.
-}
srunBOAt :: (forall α. BO (α /\ β) (After α Int)) %1 -> BO β Int
{-# NOINLINE srunBOAt #-}
srunBOAt bo = Control.fmap (+ 1) (srunBO bo)

{- | The specification 'srunBOAt' must meet: run the action, then apply the runtime-erased 'EndToken' to the 'After' it returned, under the same eta-reduction guard.

Note the signature: no rank-2 argument and no @/\\@ anywhere.
An equality with 'srunBOAt' therefore says the sublifetime left no residue at all.
-}
endTokenAt :: BO β (After γ Int) %1 -> BO β Int
{-# NOINLINE endTokenAt #-}
endTokenAt bo =
  Control.fmap (+ 1) Control.do
    after <- bo
    Control.pure $! withEnd UnsafeEnd after

-- | 'srunBO_' at a concrete carrier, saturated and rank-2 as above.
srunBO_At :: (forall α. BO (α /\ β) Int) %1 -> BO β Int
{-# NOINLINE srunBO_At #-}
srunBO_At bo = srunBO_ bo

-- | The specification 'srunBO_At' must meet: the identity.
idBOAt :: BO β Int %1 -> BO β Int
{-# NOINLINE idBOAt #-}
idBOAt bo = bo

-- | A client-level probe: 'reborrowing'' is one of the two public combinators built on 'srunBO', so the erasure has to reach through it too.
reborrowingRefAt :: Mut α (Ref Int) %1 -> BO α (Int, Mut α (Ref Int))
{-# NOINLINE reborrowingRefAt #-}
reborrowingRefAt ref = reborrowing' ref \borrowed -> Control.do
  (old, spent) <-
    Ref.update (\x -> dup2 x & \(seen, next) -> Control.pure (seen, next + 1)) borrowed
  spent `lseq` Control.pure (After old)

-- | The other 'srunBO' client, on the shared side.
sharingRefAt :: Mut α (Ref Int) %1 -> BO α (Int, Mut α (Ref Int))
{-# NOINLINE sharingRefAt #-}
sharingRefAt ref = sharing' ref \shared -> Control.do
  seen <- Ref.copyRef shared
  Control.pure (After seen)

{- | Every obligation below describes the statically erased delimiters, so under @+slow@ — where the sublifetime is a genuine runtime token by construction — each one is expected to fail rather than to be skipped.
That inversion is what keeps them honest: an obligation that also holds of the allocating implementation is no evidence about this one, and turns the group red under @+slow@ until it is either sharpened or dropped.

Two plausible-looking obligations were dropped for exactly that reason.
@'hasNoType' \'srunBOAt ''SomeNow@ holds under both, because 'MkSomeNow' wraps a nullary 'Now' and case-of-known-constructor removes the box either way.
@'hasNoType' \'reborrowingRefAt ''Now@ likewise: through 'reborrowing'' the token itself is always erased, and what the allocating version actually leaves behind is the 'Linearly' that produced it.
-}
tests :: TestTree
tests =
  testGroup "sublifetime delimiting" $
    NonLinear.map
      ( expectFailIfBecause
          isSlowAPI
          "+slow restores the token-allocating sublifetime delimiters"
      )
      [ $( inspectTest
             ( ('srunBO_At ==- 'idBOAt)
                 { testName =
                     Just "srunBO_ is the identity"
                 }
             )
         )
      , $( inspectTest
             ( ('srunBOAt ==- 'endTokenAt)
                 { testName =
                     Just "srunBO only supplies the erased end token"
                 }
             )
         )
      , $( inspectTest
             ( (hasNoType 'srunBOAt ''Now)
                 { testName =
                     Just "srunBO allocates no lifetime token"
                 }
             )
         )
      , $( inspectTest
             ( (hasNoType 'srunBOAt ''Linearly)
                 { testName =
                     Just "srunBO needs no linearity witness"
                 }
             )
         )
      , $( inspectTest
             ( (doesNotUse 'srunBOAt 'askLinearly)
                 { testName =
                     Just "srunBO does not reach for the ambient Linearly"
                 }
             )
         )
      , $( inspectTest
             ( (hasNoType 'reborrowingRefAt ''Linearly)
                 { testName =
                     Just "reborrowing' needs no linearity witness"
                 }
             )
         )
      , $( inspectTest
             ( (doesNotUse 'reborrowingRefAt 'askLinearly)
                 { testName =
                     Just "reborrowing' does not reach for the ambient Linearly"
                 }
             )
         )
      , $( inspectTest
             ( (hasNoType 'sharingRefAt ''Linearly)
                 { testName =
                     Just "sharing' needs no linearity witness"
                 }
             )
         )
      , $( inspectTest
             ( (doesNotUse 'sharingRefAt 'askLinearly)
                 { testName =
                     Just "sharing' does not reach for the ambient Linearly"
                 }
             )
         )
      ]
