{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Array.Mutable.Linear.Witness (
  allocL,
  fromListL,
) where

import Control.Monad.Borrow.Pure.Lifetime.Token.Internal (
  Linearly,
 )
import Data.Array.Mutable.Linear
import Data.Array.Mutable.Linear.Internal
import Data.Array.Mutable.Unlifted.Linear.Witness qualified as Unlifted
import GHC.Stack (HasCallStack)
import Prelude.Linear
import Prelude qualified as P

allocL :: Int -> a -> Linearly %1 -> Array a
allocL i a l = Array (Unlifted.allocL i a l)

fromListL :: (HasCallStack) => [a] -> Linearly %1 -> Array a
fromListL (list :: [a]) =
  insert
    . allocL
      (P.length list)
      (error "invariant violation: unintialized array position")
  where
    insert :: Array a %1 -> Array a
    insert = doWrites (P.zip list [0 ..])

    doWrites :: [(a, Int)] -> Array a %1 -> Array a
    doWrites [] arr = arr
    doWrites ((a, ix) : xs) arr = doWrites xs (unsafeSet ix a arr)
