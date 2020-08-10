-- |
-- Module      : Numeric.Montgomery.LimbDiv
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
module Numeric.Montgomery.LimbDiv
    ( limbDiv2x1
    ) where

import Data.Bits

import Numeric.Montgomery.Limb

-- | Divide a two-word number by a single word.
--
-- Divisor must be normalized, i.e. have most significant bit set.  High word
-- of the numerator must be smaller than the divisor, so the quotient fits in
-- a single word.
--
-- Implementation is algorithm 4 from "Improved division by invariant integers"
-- by Niels Möller and Torbjörn Granlund.
limbDiv2x1 :: DoubleLimb -> Limb -> (Limb, Limb)
limbDiv2x1 (u0, u1) d =
    let v = limbInv d

        -- ⟨q1, q0⟩ ← v.u1
        (q0, q1) = limbMul v u1

        -- ⟨q1, q0⟩ ← ⟨q1, q0⟩ + ⟨u1, u0⟩
        (q0', q1') = limbAdd2 (q0, q1) (u0, u1)

        -- q1 ← (q1 + 1) mod β
        (q0'', q1'') = (q0', q1' + 1)

        -- r ← (u0 − q1.d) mod β
        r'' = u0 - q1'' * d

        {-
            if r > q0
                q1 ← (q1 − 1) mod β
                r ← (r + d) mod β
        -}
        (_, b'') = limbSub q0'' r''
        (q1''', r''') = (q1'' - b'', r'' + (d .&. negate b''))

        {-
            if r ≥ d
                q1 ← q1 + 1
                r ← r − d
        -}
        (_, b''') = limbSub r''' d
        c''' = 1 - b'''
        (q1'''', r'''') = (q1''' + c''', r''' - (d .&. negate c'''))

        -- return q1, r
     in (q1'''', r'''')
