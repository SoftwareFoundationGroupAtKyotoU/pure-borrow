{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Data.Unique.Linear (
  UniqueSource,
  new,
  sample,
  split,
  splitN,
  splitV,
  split3,
  split4,
  split5,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  Linearly,
 )
import Data.Proxy (Proxy (Proxy))
import Data.V.Linear.Internal hiding (consume)
import Data.Vector qualified as V
import GHC.TypeNats (KnownNat, natVal)
import Prelude.Linear
import Unsafe.Linear qualified as Unsafe
import Prelude qualified as NL

data UniqueSource where
  -- | Seed, multiplier, constant.
  UniqueSource :: !Int %1 -> !Int %1 -> !Int %1 -> UniqueSource
  deriving (Show, NL.Eq, NL.Ord)

instance Consumable UniqueSource where
  consume (UniqueSource seed multiplier constant) =
    seed
      `lseq` multiplier
      `lseq` consume constant
  {-# INLINE consume #-}

new :: Linearly %1 -> UniqueSource
new lin = lin `lseq` UniqueSource 0 1 0

sample :: UniqueSource %1 -> (Int, UniqueSource)
sample =
  Unsafe.toLinear \(UniqueSource x a b) ->
    (x * a + b, UniqueSource (x + 1) a b)

{- | Split a 'UniqueSource' into two, each with non-overlapping ranges.

See also 'splitN' and 'splitV'.
-}
split :: UniqueSource %1 -> (UniqueSource, UniqueSource)
split =
  Unsafe.toLinear \(UniqueSource x a b) ->
    (x `quotRem` 2) & \(q, r) ->
      if r == 0
        then
          ( UniqueSource q (a * 2) b
          , UniqueSource q (a * 2) (a + b)
          )
        else
          ( UniqueSource q (a * 2) (a + b)
          , UniqueSource (q + 1) (a * 2) b
          )

splitN :: Int -> UniqueSource %1 -> V.Vector UniqueSource
{-# INLINE splitN #-}
splitN n = Unsafe.toLinear \(UniqueSource x a b) ->
  let (q, r) = x `quotRem` n
   in V.generate n \((+ r) -> i) ->
        let (offx, offb) = i `quotRem` n
            !x = q + offx
         in UniqueSource x (a * n) (b + a * offb)

splitV :: forall n. (KnownNat n) => UniqueSource %1 -> V n UniqueSource
{-# INLINE splitV #-}
splitV = V . splitN (fromIntegral $ natVal $ Proxy @n)

split3 :: UniqueSource -> (UniqueSource, UniqueSource, UniqueSource)
split3 = elim (,,) . splitV

split4 :: UniqueSource -> (UniqueSource, UniqueSource, UniqueSource, UniqueSource)
split4 = elim (,,,) . splitV

split5 :: UniqueSource -> (UniqueSource, UniqueSource, UniqueSource, UniqueSource, UniqueSource)
split5 = elim (,,,,) . splitV
