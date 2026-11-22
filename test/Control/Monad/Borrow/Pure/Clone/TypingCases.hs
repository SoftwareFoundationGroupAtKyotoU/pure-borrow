{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -O0 -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{- | Clones that must not typecheck: contents that are 'Dupable' but not 'Clone' cannot be cloned through a shared borrow of their container.

'dup2' consumes the piece it duplicates, and linear-base's laws do not say which copy, if either, is the original, so a clone through 'dup2' either consumes the original twice or can hand it to the clone.
See Note [Cloning the contents of a shared borrow] in "Data.Ref.Linear.Internal".

Each case runs the clone, because the missing instance is only needed, and its deferred error only raised, when the container's 'clone' clones a piece of the contents.

Nor may linear-base's mutable arrays and vectors be copied out of a borrow with 'copy': 'copy' works outside 'BO', so its copy would not be ordered with the writes of the state thread.
Their 'Copyable' instances are unsatisfiable, and each case forces 'copy', whose method raises the deferred message.
-}
module Control.Monad.Borrow.Pure.Clone.TypingCases (
  module Control.Monad.Borrow.Pure.Clone.TypingCases,
) where

import Control.Functor.Linear qualified as Control
import Control.Monad.Borrow.Pure
import Control.Monad.Borrow.Pure.BO.Unsafe (Alias (UnsafeAlias))
import Data.Array.Mutable.Linear qualified as LA
import Data.HashMap.Mutable.Linear qualified as LH
import Data.Ref.Linear qualified as Ref
import Data.Set.Mutable.Linear qualified as LS
import Data.Vector.Mutable.Growable.Linear.Borrow qualified as VG
import Data.Vector.Mutable.Linear qualified as LV
import Data.Vector.Mutable.Linear.Borrow qualified as VL
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable.Growable.Linear.Borrow qualified as UG
import Data.Vector.Unboxed.Mutable.Linear.Borrow qualified as UV
import Prelude.Linear
import Prelude qualified as NonLinear

-- | Contents that can be duplicated, but have no 'Clone' instance.
newtype DupableOnly = DupableOnly Int

instance Consumable DupableOnly where
  consume (DupableOnly n) = consume n

instance Dupable DupableOnly where
  dup2 (DupableOnly n) = dup2 n & \(a, b) -> (DupableOnly a, DupableOnly b)

-- | Clone a container of 'DupableOnly' through a shared borrow, then consume the clone and the original.
cloneDupableOnly ::
  (Consumable (f DupableOnly)) =>
  (forall α. Share α (f DupableOnly) %1 -> BO α (f DupableOnly)) ->
  (Linearly %1 -> f DupableOnly) %1 ->
  ()
cloneDupableOnly cloneIt build = linearly \lin -> runBO lin Control.do
  container <- asksLinearly build
  (borrowed, lend) <- borrowM container
  let !(Ur shared) = share borrowed
  cloned <- cloneIt shared
  pureAfter (consume cloned `lseq` consume (reclaim lend))

refOfDupableOnly :: ()
refOfDupableOnly = cloneDupableOnly clone (Ref.new (DupableOnly 1))

vectorOfDupableOnly :: ()
vectorOfDupableOnly = cloneDupableOnly clone (VL.fromList [DupableOnly 1])

-- | 'copy' of a shared linear-base array; the message must point to 'clone'.
copyOfSharedArray :: LA.Array Int
copyOfSharedArray = copy (UnsafeAlias NonLinear.undefined :: Share Static (LA.Array Int))

-- | 'copy' of a shared linear-base vector must point to 'clone'.
copyOfSharedVector :: LV.Vector Int
copyOfSharedVector = copy (UnsafeAlias NonLinear.undefined :: Share Static (LV.Vector Int))

copyOfSharedHashMap :: LH.HashMap Int Int
copyOfSharedHashMap = copy (UnsafeAlias NonLinear.undefined :: Share Static (LH.HashMap Int Int))

copyOfSharedSet :: LS.Set Int
copyOfSharedSet = copy (UnsafeAlias NonLinear.undefined :: Share Static (LS.Set Int))

growableOfDupableOnly :: ()
growableOfDupableOnly = cloneDupableOnly clone (VG.fromList [DupableOnly 1])

instance Consumable (U.DoNotUnboxLazy (Ref.Ref Int)) where
  consume (U.DoNotUnboxLazy ref) = consume ref
  {-# NOINLINE consume #-}

unboxedRefsWithoutClone :: ()
unboxedRefsWithoutClone = linearly \lin -> runBO lin Control.do
  ref <- asksLinearly (Ref.new (1 :: Int))
  owner <- asksLinearly (UV.fromList [U.DoNotUnboxLazy ref])
  (mut, lend) <- borrowM owner
  Ur shared <- Control.pure (share mut)
  copied <- clone shared
  pureAfter (consume copied `lseq` consume (reclaim lend))

unboxedGrowableRefsWithoutClone :: ()
unboxedGrowableRefsWithoutClone = linearly \lin -> runBO lin Control.do
  ref <- asksLinearly (Ref.new (1 :: Int))
  owner <- asksLinearly (UG.fromList [U.DoNotUnboxLazy ref])
  (mut, lend) <- borrowM owner
  Ur shared <- Control.pure (share mut)
  copied <- clone shared
  pureAfter (consume copied `lseq` consume (reclaim lend))
