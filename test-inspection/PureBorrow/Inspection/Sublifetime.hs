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
  reborrowingValueRefAt,
  bumpRefAt,
  bumpRefAfterAt,
  sharingRefAt,
  sharingValueRefAt,
  copyRefAt,
  copyRefAfterAt,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure.BO
import Control.Monad.Borrow.Pure.BO.Unsafe (reviveAlias)
import Control.Monad.Borrow.Pure.Lifetime.Token.Unsafe (EndToken (..))
import Data.Ref.Linear (Ref)
import Data.Ref.Linear.Borrow qualified as Ref
import Prelude.Linear
import PureBorrow.Inspection.Flags (expectFailIfBecause, isSlowAPI)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Inspection
import Unsafe.Linear qualified as Unsafe
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

{- | The body every mutable-side probe below delimits: read the reference, put back its successor, and report what was there.

Sharing it between the probes and their specification is what makes the equalities statements about the delimiter alone.
-}
bumpRef :: (α >= β) => Mut α (Ref Int) %1 -> BO β (Int, Mut α (Ref Int))
{-# INLINE bumpRef #-}
bumpRef = Ref.update \x -> dup2 x & \(seen, next) -> Control.pure (seen, next + 1)

{- | A client-level probe: 'reborrowing'' is one of the two public combinators built on 'srunBO', so the erasure has to reach through it too.

The restored borrow is dropped rather than returned, because a delimiter-free specification cannot both operate on the caller's 'Mut' and hand it back — handing it back is precisely the service the delimiter provides.
Returning it would compare a probe that gives back the box it was handed against a specification that rebuilds one, and the two differ in worker/wrapper shape for a reason that has nothing to do with the sublifetime.
-}
reborrowingRefAt :: Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE reborrowingRefAt #-}
reborrowingRefAt ref =
  reborrowing'
    ref
    ( \borrowed -> Control.do
        (old, spent) <- bumpRef borrowed
        spent `lseq` Control.pure (After old)
    )
    Control.<&> \(old, mut) -> mut `lseq` old

-- | 'reborrowing', the same probe with the result returned directly instead of 'After' the sublifetime.
reborrowingValueRefAt :: Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE reborrowingValueRefAt #-}
reborrowingValueRefAt ref =
  reborrowing
    ref
    ( \borrowed -> Control.do
        (old, spent) <- bumpRef borrowed
        spent `lseq` Control.pure old
    )
    Control.<&> \(old, mut) -> mut `lseq` old

{- | The specification 'reborrowingValueRefAt' must meet: 'bumpRef' on the caller's own borrow, plus the one 'reviveAlias' through which the delimiter restores that borrow.

Note the signature — no rank-2 argument and no @/\\@ — so an equality says the reborrow left no sublifetime residue at all, not merely that it allocated no token.
The 'reviveAlias' is not residue of the sublifetime; it is the delimiter's entire remaining cost, and it is load-bearing rather than incidental — see Note [Restoring a borrow must break its Core identity] in "Control.Monad.Borrow.Pure.BO.Internal".
Naming it here is what keeps these equalities honest: an earlier revision of this module asserted equality with the bare body, which said the delimiter compiled to nothing at all, and that was exactly the property that made it unsound.

The specification has to reach for 'Unsafe.toLinear' because it mentions the caller's borrow twice, once to bump through and once to restore, which is precisely what the delimiter does with 'Unsafe.toLinear2'.
-}
bumpRefAt :: Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE bumpRefAt #-}
bumpRefAt = Unsafe.toLinear \ref -> Control.do
  (old, spent) <- bumpRef ref
  spent `lseq` Control.do
    restored <- reviveAlias ref
    Control.pure (restored `lseq` old)

{- | The specification 'reborrowingRefAt' must meet: 'bumpRefAt', plus the application that hands the runtime-erased 'EndToken' to the 'After' the continuation returned.

That application is the one thing a delimiter taking an @'After' β r@ cannot drop, and 'srunBO' pays it too — see 'endTokenAt'.
-}
bumpRefAfterAt :: forall α. Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE bumpRefAfterAt #-}
bumpRefAfterAt = Unsafe.toLinear \ref -> Control.do
  (old, spent) <- bumpRef ref
  spent `lseq` Control.do
    restored <- reviveAlias ref
    Control.pure (restored `lseq` withEnd (UnsafeEnd @α) (After old))

-- | The other 'srunBO' client, on the shared side, with the restored borrow dropped as above.
sharingRefAt :: Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE sharingRefAt #-}
sharingRefAt ref =
  sharing'
    ref
    ( \shared -> Control.do
        seen <- Ref.copyRef shared
        Control.pure (After seen)
    )
    Control.<&> \(seen, mut) -> mut `lseq` seen

-- | 'sharing', the same probe with the result returned directly instead of 'After' the sublifetime.
sharingValueRefAt :: Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE sharingValueRefAt #-}
sharingValueRefAt ref =
  sharing ref (\shared -> Ref.copyRef shared) Control.<&> \(seen, mut) -> mut `lseq` seen

{- | The specification 'sharingValueRefAt' must meet: read through the caller's own borrow, plus the restoring 'reviveAlias', with no sublifetime anywhere.

'Data.Ref.Linear.Borrow.copyRef' reads through a borrow of either kind, so the 'Mut' serves here where the probes pass a 'Share' narrowed to the sublifetime.
The 'Unsafe.toLinear' is there for the same reason as in 'bumpRefAt'.
-}
copyRefAt :: Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE copyRefAt #-}
copyRefAt = Unsafe.toLinear \ref -> Control.do
  seen <- Ref.copyRef ref
  restored <- reviveAlias ref
  Control.pure (restored `lseq` seen)

-- | The specification 'sharingRefAt' must meet: 'copyRefAt' plus the end-token application, as 'bumpRefAfterAt' is to 'bumpRefAt'.
copyRefAfterAt :: forall α. Mut α (Ref Int) %1 -> BO α Int
{-# NOINLINE copyRefAfterAt #-}
copyRefAfterAt = Unsafe.toLinear \ref -> Control.do
  seen <- Ref.copyRef ref
  restored <- reviveAlias ref
  Control.pure (restored `lseq` withEnd (UnsafeEnd @α) (After seen))

{- | Every obligation below describes the statically erased delimiters, so under @+slow@ — where the sublifetime is a genuine runtime token by construction — each one is expected to fail rather than to be skipped.
That inversion is what keeps them honest: an obligation that also holds of the allocating implementation is no evidence about this one, and turns the group red under @+slow@ until it is either sharpened or dropped.

Two plausible-looking obligations were dropped for exactly that reason.
@'hasNoType' \'srunBOAt ''SomeNow@ holds under both, because 'MkSomeNow' wraps a nullary 'Now' and case-of-known-constructor removes the box either way.
@'hasNoType' \'reborrowingRefAt ''Now@ likewise: through 'reborrowing'' the token itself is always erased, and what the allocating version actually leaves behind is the 'Linearly' that produced it.

The four borrow-scope equalities now pin the 'Control.Monad.Borrow.Pure.BO.Unsafe.reviveAlias' barrier as well, because their specifications name it.
That matters beyond bookkeeping: the runtime regressions in "Control.Monad.Borrow.Pure.BOSpec" observe a wrong /answer/, so on a compiler that stopped performing the merge they would go vacuously green rather than red, and these equalities would be the only thing left that notices the barrier being dropped.
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
      , $( inspectTest
             ( ('reborrowingRefAt ==- 'bumpRefAfterAt)
                 { testName =
                     Just "reborrowing' only supplies the erased end token"
                 }
             )
         )
      , $( inspectTest
             ( ('reborrowingValueRefAt ==- 'bumpRefAt)
                 { testName =
                     Just "reborrowing costs nothing over the update it delimits"
                 }
             )
         )
      , $( inspectTest
             ( ('sharingRefAt ==- 'copyRefAfterAt)
                 { testName =
                     Just "sharing' only supplies the erased end token"
                 }
             )
         )
      , $( inspectTest
             ( ('sharingValueRefAt ==- 'copyRefAt)
                 { testName =
                     Just "sharing costs nothing over the read it delimits"
                 }
             )
         )
      ]
