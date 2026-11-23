-- | Keys with chosen fingerprints, for steering entries into chosen buckets of a Robin Hood hash table.
module Data.HashMap.RobinHood.Mutable.LinearSpec.Steered (
  Steered (..),
  unmixHash,
  unmixWord64,
  unmixWord32,
) where

import Data.Bits (finiteBitSize, shiftR, xor)
import Data.Hashable (Hashable (..))
import Data.Word (Word32, Word64)

-- | A key whose fingerprint in the table is the number it holds.
newtype Steered = Steered Int
  deriving (Show, Eq, Ord)

instance Hashable Steered where
  hashWithSalt _ (Steered fp) = unmixHash fp

-- | The inverse of the table's @mixHash@, so that a 'Steered' key gets the fingerprint it holds.
unmixHash :: Int -> Int
unmixHash fp
  | finiteBitSize fp == 64 = fromIntegral (unmixWord64 (fromIntegral fp))
  | otherwise = fromIntegral (unmixWord32 (fromIntegral fp))

-- | The inverse of the table's @mixWord64@.
unmixWord64 :: Word64 -> Word64
unmixWord64 w = shift33 (shift33 (shift33 w * inverse 0xc4ceb9fe1a85ec53) * inverse 0xff51afd7ed558ccd)
  where
    shift33 x = x `xor` (x `shiftR` 33)

-- | The inverse of the table's @mixWord32@.
unmixWord32 :: Word32 -> Word32
unmixWord32 w = shift16 (unshift13 (shift16 w * inverse 0xc2b2ae35) * inverse 0x85ebca6b)
  where
    shift16 x = x `xor` (x `shiftR` 16)
    -- The inverse of xoring a word with itself shifted right by 13.
    unshift13 x = let y = x `xor` (x `shiftR` 13) in y `xor` (y `shiftR` 26)

-- | The inverse of an odd number modulo the word size, by Newton's iteration.
inverse :: (Num a) => a -> a
inverse a = iterate (\x -> x * (2 - a * x)) a !! 5
