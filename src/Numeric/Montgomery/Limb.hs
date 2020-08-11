-- |
-- Module      : Numeric.Montgomery.Limb
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Numeric.Montgomery.Limb
    ( Limb
    , DoubleLimb
    , limbBits
    , limbNotZero
    , limbClz
    , limbAdd
    , limbAdd2
    , limbSub
    , limbMul
    , limbInv
    , limbModInv
    ) where

import Data.Bits

import GHC.Exts

-- | A limb is a machine word storing numbers.
type Limb = Word

-- | A pair of limbs storing low and high limbs.
type DoubleLimb = (Limb, Limb)  -- lo, hi

-- | The number of bits in a limb.
limbBits :: Int
limbBits = finiteBitSize (undefined :: Word)

-- | Return a mask with all bits set to the complement of the most significant
-- bit of the input.
limbMsbComplement :: Limb -> Limb
limbMsbComplement x = unsafeShiftR x (limbBits - 1) - 1

-- | Return a mask with all bits set when the input is not zero.
limbNotZero :: Limb -> Limb
limbNotZero x = limbMsbComplement (complement x .&. (x - 1))

-- | Number of leading zeros, between 0 and 'limbBits'.
limbClz :: Limb -> Limb
limbClz (W# x#) = W# (clz# x#)

-- | Add two limbs, return sum and carry.
limbAdd :: Limb -> Limb -> (Limb, Limb)
limbAdd (W# x#) (W# y#) =
    let !(# c#, z# #) = plusWord2# x# y# in (W# z#, W# c#)

-- | Add two limb pairs, both containing low/high limbs.
limbAdd2 :: DoubleLimb -> DoubleLimb -> DoubleLimb
limbAdd2 (x0, x1) (y0, y1) = (z0, x1 + y1 + c0)
  where (z0, c0) = limbAdd x0 y0

-- | Substract two limbs, return difference and borrow.
limbSub :: Limb -> Limb -> (Limb, Limb)
limbSub (W# x#) (W# y#) =
    let !(# z#, b# #) = subWordC# x# y# in (W# z#, W# (int2Word# b#))

-- | Multiply two limbs and return a pair containing low/high limbs.
limbMul :: Limb -> Limb -> DoubleLimb
limbMul (W# x#) (W# y#) =
    let !(# hi#, lo# #) = timesWord2# x# y# in (W# lo#, W# hi#)

-- | Return the approximate reciprocal of @d@, defined as:
-- @v = floor ( ((2^limbBits)^2 - 1) / d ) - 2^limbBits@
--
-- Input @d@ must be normalized, i.e. have most significant bit set.
--
-- See section II of "Improved division by invariant integers", by Niels Möller
-- and Torbjörn Granlund.
limbInv :: Limb -> Limb
limbInv (W# d#) =
    let !(# q#, _ #) = quotRemWord2# (minusWord# bm1# d#) bm1# d# in W# q#
  where bm1# = not# 0##  -- 2^limbBits - 1

-- | Inverse of @n@ modulo @2^limbBits@, assuming @n@ is odd.
limbModInv :: Limb -> Limb
limbModInv n = loop 8 inv8
  where
    idx8 = fromIntegral (half n .&. 0x7f)
    inv8 = limbModInv8 idx8

    double i = unsafeShiftL i 1
    half i = unsafeShiftR i 1

    -- Go from inverse modulo 2^k to inverse modulo 2^(2*k):
    -- when n*i == 1 mod 2^k, then n*i*(2-n*i) == 1 mod 2^(2*k)
    f i = double i - i * i * n

    loop bits invb
        | bits > limbBits = invb
        | otherwise       = loop (double bits) (f invb)

-- Index i contains the multiplicative inverse of (2*idx+1) mod 2^8.
-- Precomputed for i in range [ 0 .. 127 ].
limbModInv8 :: Int -> Word
limbModInv8 (I# i#) = W# (indexWord8OffAddr# a# i#)
  where
    a# = "\x01\xab\xcd\xb7\x39\xa3\xc5\xef\
         \\xf1\x1b\x3d\xa7\x29\x13\x35\xdf\
         \\xe1\x8b\xad\x97\x19\x83\xa5\xcf\
         \\xd1\xfb\x1d\x87\x09\xf3\x15\xbf\
         \\xc1\x6b\x8d\x77\xf9\x63\x85\xaf\
         \\xb1\xdb\xfd\x67\xe9\xd3\xf5\x9f\
         \\xa1\x4b\x6d\x57\xd9\x43\x65\x8f\
         \\x91\xbb\xdd\x47\xc9\xb3\xd5\x7f\
         \\x81\x2b\x4d\x37\xb9\x23\x45\x6f\
         \\x71\x9b\xbd\x27\xa9\x93\xb5\x5f\
         \\x61\x0b\x2d\x17\x99\x03\x25\x4f\
         \\x51\x7b\x9d\x07\x89\x73\x95\x3f\
         \\x41\xeb\x0d\xf7\x79\xe3\x05\x2f\
         \\x31\x5b\x7d\xe7\x69\x53\x75\x1f\
         \\x21\xcb\xed\xd7\x59\xc3\xe5\x0f\
         \\x11\x3b\x5d\xc7\x49\x33\x55\xff"#
