-- |
-- Module      : Numeric.Montgomery
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Montgomery
    ( Bn
    -- * Limbs
    , Limb
    , limbBits
    , limbDiv2x1
    -- * Conversion functions
    , fromLimbs
    , toLimbs
    , toNatural
    , fromNatural
    -- * Arithmetic functions
    , bnAdd
    , bnAdd1
    , bnNegate
    , bnSub
    , bnSub1
    , bnMul
    , bnMul1
    , bnSqr
    , bnModSlow
    , bnDivMod
    -- Bit functions
    , bnShiftL
    , bnShiftR
    -- Montgomery functions
    , bnRedc
    , bnRedcSimple
    , bnAddMont
    , bnMulMont
    , bnSqrMont
    , bnPowMont
    ) where

import Basement.Block as Block
import qualified Basement.Block.Mutable as Mutable
import Basement.Compat.IsList
import Basement.NormalForm
import Basement.Types.OffsetSize

import Control.DeepSeq
import Control.Monad.ST

import Data.Bits
import qualified Data.List as List

import Numeric
import Numeric.Montgomery.Limb
import Numeric.Montgomery.LimbDiv
import Numeric.Natural

import Text.Show


{- Bn -}

-- | A big number split over multiple limbs.  Least-significant limb is stored
-- first.  Designed for a fixed number of limbs, even when not all are
-- significant (i.e. the array may contain limbs that are 0 at the tail).
--
-- Numbers are equal only when they have the same number of limbs, including
-- non-significant limbs.
newtype Bn = Bn { unBn :: Block Limb }
    deriving Eq  -- not time-constant Eq, for test purpose

instance Show Bn where
    showsPrec d bn = showParen (d > 10) $
        showString "fromLimbs " . showListWith hex (toLimbs bn)
      where hex limb = showString "0x" . showHex limb

instance NFData Bn where
    rnf = toNormalForm . unBn


{- Block utility functions -}

withMutable
    :: CountOf Limb
    -> (forall s . MutableBlock Limb s -> ST s a)
    -> (Bn, a)
withMutable len action = runST $ do
    mut <- Mutable.new len
    res <- action mut
    !out <- Block.unsafeFreeze mut
    return (Bn out, res)

withMutable_
    :: CountOf Limb
    -> (forall s . MutableBlock Limb s -> ST s ())
    -> Bn
withMutable_ len action = runST $ do
    mut <- Mutable.new len
    action mut
    !out <- Block.unsafeFreeze mut
    return (Bn out)

uIndex :: Block Limb -> Offset Limb -> Limb
uIndex = Block.unsafeIndex

uFreeze :: MutableBlock Limb s -> ST s (Block Limb)
uFreeze = Block.unsafeFreeze

uFreezeSub :: MutableBlock Limb s -> Offset Limb -> CountOf Limb
           -> ST s (Block Limb)
uFreezeSub m off cnt = do
    a <- Block.unsafeFreeze m
    return $! Block.sub a off (off `offsetPlusE` cnt)

mutRead :: MutableBlock Limb s -> Offset Limb -> ST s Limb
mutRead = Mutable.unsafeRead

mutWrite :: MutableBlock Limb s -> Offset Limb -> Limb -> ST s ()
mutWrite = Mutable.unsafeWrite

mutPlus :: MutableBlock Limb s -> Offset Limb -> Limb -> ST s ()
mutPlus m i x = do { limb <- mutRead m i; mutWrite m i (limb + x) }

mutClear :: MutableBlock Limb s -> Offset Limb -> CountOf Limb -> ST s ()
mutClear !m off len = loop 0
  where
    loop i | i .==# len = return ()
           | otherwise  = mutWrite m (off + i) 0 >> loop (succ i)

mutCopy :: MutableBlock Limb s -> Offset Limb -> Block Limb -> Offset Limb
        -> CountOf Limb -> ST s ()
mutCopy !dst dstoff !src srcoff len = loop 0
  where
    loop i
        | i .==# len = return ()
        | otherwise  = do
            mutWrite dst (dstoff + i) $ uIndex src (srcoff + i)
            loop (succ i)


{- Extension and truncation -}

bnTrunc :: Bn -> CountOf Limb -> Bn
bnTrunc (Bn a) r = Bn $ Block.sub a 0 (sizeAsOffset r)

bnExtendL :: Bn -> CountOf Limb -> Bn
bnExtendL (Bn a) r = Bn $ a <> Block.replicate r 0

bnExtendR :: Bn -> CountOf Limb -> Bn
bnExtendR (Bn a) r = Bn $ Block.replicate r 0 <> a


{- Conversions -}

-- | Build a number from a list of limbs (least significant first).
fromLimbs :: [Limb] -> Bn
fromLimbs = Bn . fromList

-- | Return the list of limbs (least significant first).
toLimbs :: Bn -> [Limb]
toLimbs = toList . unBn

-- | Conversion to a natural number.
toNatural :: Bn -> Natural
toNatural = Block.foldr f 0 . unBn
  where f limb acc = shiftL acc limbBits + fromIntegral limb

-- | Conversion from a natural number.
fromNatural :: Natural -> Bn
fromNatural = fromLimbs . go
  where
    go 0 = []
    go i = fromIntegral i : go (shiftR i limbBits)


{- Addition -}

-- | Add a limb to a number.
bnAdd1 :: Bn -> Limb -> Bn
bnAdd1 x y = fst (bnAdd1C x y)

bnAdd1C :: Bn -> Limb -> (Bn, Limb)
bnAdd1C (Bn x) y = withMutable len $ \m -> loop m y 0
  where
    len = Block.length x

    loop !m c i
        | i .==# len = return $! c
        | otherwise  = do
            let (limb, c') = limbAdd (uIndex x i) c
            mutWrite m i limb
            loop m c' (succ i)

-- | Add two numbers of the same size.
bnAdd :: Bn -> Bn -> Bn
bnAdd x y = fst (bnAddC x y 0)

bnAddC :: Bn -> Bn -> Limb -> (Bn, Limb)
bnAddC (Bn x) (Bn y) c
    | xLen /= yLen = error "bnAdd: arguments must have same length"
    | otherwise    = withMutable xLen $ \m -> loop m c 0
  where
    xLen = Block.length x
    yLen = Block.length y

    loop !m c0 i
        | i .==# xLen = return $! c0
        | otherwise   = do
            let (limb1, c1) = limbAdd (uIndex x i) (uIndex y i)
                (limb2, c2) = limbAdd limb1 c0
            mutWrite m i limb2
            loop m (c1 + c2) (succ i)

mutAddMut :: MutableBlock Limb s -> Offset Limb -> CountOf Limb
          -> MutableBlock Limb s -> ST s Limb
mutAddMut !x off xLen y = loop 0 0
  where
    loop c0 i
        | i .==# xLen = return $! c0
        | otherwise   = do
            limb  <- mutRead y i
            limb0 <- mutRead x (off + i)
            let (limb1, c1) = limbAdd limb0 limb
                (limb2, c2) = limbAdd limb1 c0
            mutWrite x (off + i) limb2
            loop (c1 + c2) (succ i)

-- | Conditionally add two numbers of the same size.
bnAddCond :: Bn -> Limb -> Bn -> Bn
bnAddCond x mask y = fst (bnAddCondC x mask y 0)

bnAddCondC :: Bn -> Limb -> Bn -> Limb -> (Bn, Limb)
bnAddCondC (Bn x) !mask (Bn y) c
    | xLen /= yLen = error "bnAddCond: arguments must have same length"
    | otherwise    = withMutable xLen $ \m -> loop m c 0
  where
    xLen = Block.length x
    yLen = Block.length y

    loop !m c0 i
        | i .==# xLen = return $! c0
        | otherwise   = do
            let (limb1, c1) = limbAdd (uIndex x i) (uIndex y i .&. mask)
                (limb2, c2) = limbAdd limb1 c0
            mutWrite m i limb2
            loop m (c1 + c2) (succ i)

mutAddCond :: MutableBlock Limb s -> Offset Limb -> CountOf Limb -> Limb
           -> Block Limb -> ST s Limb
mutAddCond !x off xLen !mask y = loop 0 0
  where
    loop c0 i
        | i .==# xLen = return $! c0
        | otherwise   = do
            limb0 <- mutRead x (off + i)
            let (limb1, c1) = limbAdd limb0 (uIndex y i .&. mask)
                (limb2, c2) = limbAdd limb1 c0
            mutWrite x (off + i) limb2
            loop (c1 + c2) (succ i)


{- Negation -}

-- | Negate a number.
bnNegate :: Bn -> Bn
bnNegate x = fst (bnNegateB x 0)

bnNegateB :: Bn -> Limb -> (Bn, Limb)
bnNegateB (Bn x) b = withMutable len $ \m -> loop m b 0
  where
    len = Block.length x

    loop !m b0 i
        | i .==# len = return $! b0
        | otherwise  = do
            let (limb1, b1) = limbSub 0 (uIndex x i)
                (limb2, b2) = limbSub limb1 b0
            mutWrite m i limb2
            loop m (b1 + b2) (succ i)


{- Substraction -}

-- | Substract a limb from a number.
bnSub1 :: Bn -> Limb -> Bn
bnSub1 x y = fst (bnSub1B x y)

bnSub1B :: Bn -> Limb -> (Bn, Limb)
bnSub1B (Bn x) b = withMutable len $ \m -> loop m b 0
  where
    len = Block.length x

    loop !m b0 i
        | i .==# len = return $! b0
        | otherwise  = do
            let (limb, b') = limbSub (uIndex x i) b0
            mutWrite m i limb
            loop m b' (succ i)

-- | Substract two numbers of the same size.
bnSub :: Bn -> Bn -> Bn
bnSub x y = fst (bnSubB x y 0)

bnSubB :: Bn -> Bn -> Limb -> (Bn, Limb)
bnSubB (Bn x) (Bn y) b
    | xLen /= yLen = error "bnSub: arguments must have same length"
    | otherwise    = withMutable xLen $ \m -> loop m b 0
  where
    xLen = Block.length x
    yLen = Block.length y

    loop !m b0 i
        | i .==# xLen = return $! b0
        | otherwise   = do
            let (limb1, b1) = limbSub (uIndex x i) (uIndex y i)
                (limb2, b2) = limbSub limb1 b0
            mutWrite m i limb2
            loop m (b1 + b2) (succ i)

mutSub :: MutableBlock Limb s -> Offset Limb -> CountOf Limb -> Block Limb
       -> ST s Limb
mutSub !x off xLen y = loop 0 0
  where
    loop b0 i
        | i .==# xLen = return $! b0
        | otherwise   = do
            limb0 <- mutRead x (off + i)
            let (limb1, b1) = limbSub limb0 (uIndex y i)
                (limb2, b2) = limbSub limb1 b0
            mutWrite x (off + i) limb2
            loop (b1 + b2) (succ i)

-- | Conditionally substract two numbers of the same size.
bnSubCond :: Bn -> Limb -> Bn -> Bn
bnSubCond x mask y = fst (bnSubCondB x mask y 0)

bnSubCondB :: Bn -> Limb -> Bn -> Limb -> (Bn, Limb)
bnSubCondB (Bn x) !mask (Bn y) b
    | xLen /= yLen = error "bnSubCond: arguments must have same length"
    | otherwise    = withMutable xLen $ \m -> loop m b 0
  where
    xLen = Block.length x
    yLen = Block.length y

    loop !m b0 i
        | i .==# xLen = return $! b0
        | otherwise   = do
            let (limb1, b1) = limbSub (uIndex x i) (uIndex y i .&. mask)
                (limb2, b2) = limbSub limb1 b0
            mutWrite m i limb2
            loop m (b1 + b2) (succ i)

mutSubCond :: MutableBlock Limb s -> Offset Limb -> CountOf Limb -> Limb
           -> Block Limb -> ST s Limb
mutSubCond !x off xLen !mask y = loop 0 0
  where
    loop b0 i
        | i .==# xLen = return $! b0
        | otherwise   = do
            limb0 <- mutRead x (off + i)
            let (limb1, b1) = limbSub limb0 (uIndex y i .&. mask)
                (limb2, b2) = limbSub limb1 b0
            mutWrite x (off + i) limb2
            loop (b1 + b2) (succ i)


{- Multiplication -}

-- | Multiply a number and a limb.
bnMul1 :: Bn -> Limb -> Bn
bnMul1 (Bn x) !y = withMutable_ (len + 1) $ \m -> loop m 0 0
  where
    len = Block.length x

    loop !m c0 i
        | i .==# len = mutWrite m i c0
        | otherwise  = do
            let (limb1, c1) = limbMul (uIndex x i) y
                (limb2, c2) = limbAdd limb1 c0
            mutWrite m i limb2
            loop m (c1 + c2) (succ i)

mutAddMul1
    :: MutableBlock Limb s -> Offset Limb
    -> Block Limb -> CountOf Limb
    -> Limb -> ST s Limb
mutAddMul1 !m off x xLen !y = loop 0 0
  where
    loop c0 i
        | i .==# xLen = return $! c0
        | otherwise   = do
            limb0 <- mutRead m (off + i)
            let (limb1, c1) = limbMul (uIndex x i) y
                (limb2, c2) = limbAdd limb0 limb1
                (limb3, c3) = limbAdd limb2 c0
            mutWrite m (off + i) limb3
            loop (c1 + c2 + c3) (succ i)

mutSubMul1
    :: MutableBlock Limb s -> Offset Limb
    -> Block Limb -> CountOf Limb
    -> Limb -> ST s Limb
mutSubMul1 !m off x xLen !y = loop 0 0
  where
    loop b0 i
        | i .==# xLen = return $! b0
        | otherwise   = do
            limb0 <- mutRead m (off + i)
            let (limb1, b1) = limbMul (uIndex x i) y
                (limb2, b2) = limbSub limb0 limb1
                (limb3, b3) = limbSub limb2 b0
            mutWrite m (off + i) limb3
            loop (b1 + b2 + b3) (succ i)

-- | Multiply two numbers.
bnMul :: Bn -> Bn -> Bn
bnMul (Bn x) (Bn y) =
    withMutable_ (xLen + yLen) $ \m -> mutClear m 0 yLen >> loopOuter m 0
  where
    xLen = Block.length x
    yLen = Block.length y

    loopOuter !m i
        | i .==# xLen = return ()
        | otherwise   = loopInner m 0 i 0

    loopInner !m c0 i j
        | j .==# yLen = mutWrite m k c0 >> loopOuter m (succ i)
        | otherwise   = do
            limb0 <- mutRead m k
            let (limb1, c1) = limbMul (uIndex x i) (uIndex y j)
                (limb2, c2) = limbAdd limb0 limb1
                (limb3, c3) = limbAdd limb2 c0
            mutWrite m k limb3
            loopInner m (c1 + c2 + c3) i (succ j)
          where k = i + j

-- | Square a number.
bnSqr :: Bn -> Bn
bnSqr xn@(Bn x)
    | xLen == 0 = xn
    | otherwise = withMutable_ (xLen + xLen) $ \m ->
        mutClear m 0 (xLen + xLen) >> loopOuter m 0
  where
    xLen = Block.length x

    loopOuter !m i = do

        -- handle diagonal term
        let k = i + i
        limb0 <- mutRead m k
        let (limb1, c1) = limbMul (uIndex x i) (uIndex x i)
            (limb2, c2) = limbAdd limb0 limb1
        mutWrite m k limb2

        -- non-diagonal terms with j > i
        loopInner m (c1 + c2, 0) i (succ i)

    -- non-diagonal terms are added twice so the carry has size limbBits+1 and
    -- we pass it as two limbs c and d, with d <= 1
    loopInner !m (c0, d0) i j
        | j .==# xLen, i .==# (xLen - 1) =
            seq d0 $ mutPlus m k c0
        | j .==# xLen = do
            limb0 <- mutRead m k
            let (limb1, d1) = limbAdd limb0 c0
            mutWrite m k limb1
            mutWrite m (succ k) (d0 + d1)
            loopOuter m (succ i)  -- next outer iteration
        | otherwise   = do
            limb0 <- mutRead m k
            let (limb1, c1) = limbMul (uIndex x i) (uIndex x j)
                (limb2, c2) = limbAdd limb0 limb1
                (limb3, c3) = limbAdd limb2 limb1
                (limb4, c4) = limbAdd limb3 c0
                (c5,    d1) = limbAdd (d0 + c1 + c2) (c1 + c3 + c4)
            mutWrite m k limb4
            loopInner m (c5, d1) i (succ j)
          where k = i + j

{- Bit functions -}

bnClzVarTime :: Bn -> Int
bnClzVarTime (Bn x)
    | len == 0  = 0
    | otherwise = go 0 (pred $ sizeAsOffset len)
  where
    len = Block.length x

    go acc i
        | i == 0 || limb > 0 = acc + fromIntegral (limbClz limb)
        | otherwise          = go (acc + limbBits) (pred i)
      where limb = uIndex x i

-- | Shift a number to the left by @n@ bits.
bnShiftL :: Bn -> Int -> Bn
bnShiftL (Bn x) n
    | n < 0      = error "bnShiftL: shift must not be negative"
    | off >= len = Bn (Block.replicate len 0)
    | otherwise  = withMutable_ len $ \m -> clear m 0
  where
    len = Block.length x
    off = CountOf q
    (q, r) = n `divMod` limbBits

    clear !m i
        | i .==# off = copyShift m i
        | otherwise  = mutWrite m i 0 >> clear m (succ i)

    copyShift !m i
        | r == 0    = mutCopy m i x 0 (len - off)
        | otherwise = loop m 0 i

    loop !m prev i
        | i .==# len = seq prev $ return ()
        | otherwise  = do
            let limb  = uIndex x (i `offsetMinusE` off)
                limb' = unsafeShiftL limb r .|. unsafeShiftR prev (limbBits - r)
            mutWrite m i limb'
            loop m limb (succ i)

-- | Shift a number to the right by @n@ bits.
bnShiftR :: Bn -> Int -> Bn
bnShiftR (Bn x) n
    | n < 0      = error "bnShiftR: shift must not be negative"
    | off >= len = Bn (Block.replicate len 0)
    | otherwise  = withMutable_ len $ \m -> clear m (sizeAsOffset len)
  where
    len = Block.length x
    off = CountOf q
    (q, r) = n `divMod` limbBits

    clear !m i
        | i .==# (len - off) = copyShift m i
        | otherwise = let i' = pred i in mutWrite m i' 0 >> clear m i'

    copyShift !m i
        | r == 0    = mutCopy m 0 x (sizeAsOffset off) (len - off)
        | otherwise = loop m 0 i

    loop !m prev i
        | i == 0    = seq prev $ return ()
        | otherwise = do
            let i' = pred i
                limb  = uIndex x (i' `offsetPlusE` off)
                limb' = unsafeShiftR limb r .|. unsafeShiftL prev (limbBits - r)
            mutWrite m i' limb'
            loop m limb i'


{- Modulo -}

-- | Slow modulo function, for inputs of the same size.  Reduces the input bit
-- by bit with a conditional substraction until the result is small enough.
--
-- Perfer instead 'bnDivMod'.
bnModSlow :: Bn -> Bn -> Bn
bnModSlow xn@(Bn x) mn@(Bn m)
    | xLen /= mLen = error "bnModSlow: arguments must have same length"
    | otherwise    = loop (bnClzVarTime mn) xn
  where
    xLen = Block.length x
    mLen = Block.length m

    loop s a
        | s < 0     = a
        | otherwise = loop (pred s) a''
      where
        mShifted = bnShiftL mn s
        (a', b)  = bnSubB a mShifted 0  -- try substraction
        mask     = negate b
        a''      = bnAddCond a' mask mShifted  -- add back when negative

-- | Returns quotient and remainder for a division.
bnDivMod :: Bn -> Bn -> (Bn, Bn)
bnDivMod !nn dn@(Bn d)
    | bits == 0 =
        -- dn' is already normalized so we can call bnNormDivMod directly
        let (qn, rn) = bnNormDivMod nn dn'
         in (qn, bnExtendL rn extra)
    | otherwise =
        -- we make a normalized denominator with a bit shift and apply the same
        -- bit shift to the numerator, then the result of bnNormDivMod is
        -- adjusted accordingly
        let nn'' = bnShiftL (bnExtendL nn 1) bits
            dn'' = bnShiftL dn' bits
            (qn@(Bn q), rn) = bnNormDivMod nn'' dn''
            qn' = bnTrunc qn (Block.length q - 1)     -- undo extra limb
            rn' = bnExtendL (bnShiftR rn bits) extra  -- undo bit shift
         in (qn', rn')
  where
    (limbs, bits) = bnClzVarTime dn `quotRem` limbBits
    extra = CountOf limbs
    dn' = bnTrunc dn (Block.length d - extra)

-- Division by a normalized divisor.
--
-- Algorithm adapted from @mpn_sec_pi1_div_qr@ in GMP library.  Each reduction
-- removes half a limb in constant time.
bnNormDivMod :: Bn -> Bn -> (Bn, Bn)
bnNormDivMod nn@(Bn n) dn@(Bn d)
    | dLen == 0    = error "bnDivMod: divisor cannot be zero"

    | nLen == dLen = runST $ do
        -- denominator has most significant bit set and numerator has same
        -- number of limbs, so there is only one bit to reduce, and we do this
        -- with a single conditional substraction
        mr <- Mutable.new dLen
        b <- subB mr n d nLen 0                 -- first always try to substract
        _ <- mutAddCond mr 0 dLen (negate b) d  -- restore if underflow
        !r <- uFreezeSub mr 0 dLen
        let qn = Bn $ Block.singleton (1 - b)
        return (qn, Bn r)

    | nLen > dLen = runST $ do
        -- copy of numerator as working area for remainder
        mr <- Mutable.new nLen
        mutCopy mr 0 n 0 nLen

        -- allocate buffers for low and high quotients
        mq1 <- Mutable.new qLen
        mq0 <- Mutable.new qLen
        mutWrite mq1 (sizeLastOffset qLen) 0
        mutWrite mq0 (sizeLastOffset qLen) 0

        -- main iterations
        nh <- loop mr mq0 mq1 0 (sizeAsOffset $ nLen - dLen - 1)

        -- 1st adjustment depends on extra high remainder limb
        let cnd = limbNotZero nh
        b <- mutSubCond mr 0 dLen cnd d
        let nh' = nh - b

        -- 2nd adjustment depends on remainder/divisor comparison as well as
        -- whether extra remainder limb was nullified by previous subtract
        cy <- mutSub mr 0 dLen d
        let cy' = cy - nh'
        _ <- mutAddCond mr 0 dLen (limbNotZero cy') d

        -- 3rd adjustment depends on remainder/divisor comparison
        cy'' <- mutSub mr 0 dLen d
        _ <- mutAddCond mr 0 dLen (limbNotZero cy'') d

        -- mutWrite all adjustments to quotient
        mutPlus mq0 0 $ (cnd .&. 1) + (1 - cy') + (1 - cy'')

        -- add low and high quotients together
        addShiftHalfL mq1 mq0 qLen
        !q <- uFreeze mq1

        !r <- uFreezeSub mr 0 dLen
        return (Bn q, Bn r)

    | otherwise =
        let qn = Bn mempty
            rn = bnExtendL nn (dLen - nLen)
         in (qn, rn)

  where
    nLen = Block.length n
    dLen = Block.length d
    qLen = nLen - dLen + 1

    limbHalfBits = unsafeShiftR limbBits 1
    Bn h = bnShiftL (bnExtendL dn 1) limbHalfBits  -- shift divisor by half limb

    dLast = uIndex d (sizeLastOffset dLen)
    v | dLast < maxBound = limbInv (succ dLast)
      | otherwise = limbInv dLast

    loop !mr !mq0 !mq1 nh i
        | i < 0     = return $! nh
        | otherwise = do

            -- high half
            limb <- mutRead mr (i `offsetPlusE` dLen)
            let nh' = unsafeShiftL nh limbHalfBits + unsafeShiftR limb limbHalfBits
                (_, q1h) = limbMul nh' v
                q1h' = q1h + nh'
            mutWrite mq1 i q1h'
            _ <- mutSubMul1 mr i h (dLen + 1) q1h'

            -- low half
            nh'' <- mutRead mr (i `offsetPlusE` dLen)
            let (_, q0h) = limbMul nh'' v
                q0h' = q0h + nh''
            mutWrite mq0 i q0h'
            b <- mutSubMul1 mr i d dLen q0h'

            -- next iteration
            loop mr mq0 mq1 (nh'' - b) (pred i)

    -- set M to X - Y
    subB !m x y len b = go b 0
      where
        go b0 i
            | i .==# len = return $! b0
            | otherwise   = do
                let (limb1, b1) = limbSub (uIndex x i) (uIndex y i)
                    (limb2, b2) = limbSub limb1 b0
                mutWrite m i limb2
                go (b1 + b2) (succ i)

    -- shift X to the left by limbHalfBits and add Y
    addShiftHalfL mx my len = go 0 0 0
      where
        go !prev c0 i
            | i .==# len = seq c0 $ return ()
            | otherwise  = do
                limb <- mutRead mx i
                let limb' = unsafeShiftL limb limbHalfBits .|.
                            unsafeShiftR prev limbHalfBits
                limb0 <- mutRead my i
                let (limb1, c1) = limbAdd limb0 limb'
                    (limb2, c2) = limbAdd limb1 c0
                mutWrite mx i limb2
                go limb (c1 + c2) (succ i)


{- Montgomery -}

-- | Multi-precision REDC.
bnRedc :: Bn -> CountOf Limb -> Bn -> Bn
bnRedc (Bn n) r (Bn t)
    | p == 0       = error "bnRedc: n must be > 0"
    | even n0      = error "bnRedc: n must be odd"
    | tLen > r + p = error "bnRedc: t is too large"
    | otherwise    = runST $ do
        tmp <- Mutable.new (r + p + 1)
        tCopy tmp 0
        loop1 tmp 0
        let off = sizeAsOffset r
        subIfNeeded tmp off
        Bn <$> uFreezeSub tmp off p
  where
    p    = Block.length n
    tLen = Block.length t

    n0   = uIndex n 0
    n0'  = negate (limbModInv n0)

    tCopy !m i
        | i .==# tLen = tCopy' m i
        | otherwise   =
            mutWrite m i (uIndex t i) >> tCopy m (succ i)

    tCopy' !m i
        | i .==# (r + p) = mutWrite m i 0
        | otherwise      = mutWrite m i 0 >> tCopy' m (succ i)

    loop1 !tmp i
        | i .==# r  = return ()
        | otherwise = do
            tmpi <- mutRead tmp i
            loop2 tmp (tmpi * n0') i 0 0

    loop2 !tmp m i c0 j
        | j .==# p  = seq m $ loop3 tmp c0 i k
        | otherwise = do
            limb0 <- mutRead tmp k
            let (limb1, c1) = limbMul m (uIndex n j)
                (limb2, c2) = limbAdd limb0 limb1
                (limb3, c3) = limbAdd limb2 c0
            mutWrite tmp k limb3
            loop2 tmp m i (c1 + c2 + c3) (succ j)
      where k = i + j

    loop3 !tmp c0 !i k
        | k .==# (r + p + 1) = seq c0 $ loop1 tmp (succ i)
        | otherwise = do
            limb0 <- mutRead tmp k
            let (limb1, c1) = limbAdd limb0 c0
            mutWrite tmp k limb1
            loop3 tmp c1 i (succ k)

    -- after the main loop we get a result < 2n so need a conditional
    -- substraction to make it < n
    subIfNeeded tmp off = do
        b0 <- mutSub tmp off p n
        let i = off `offsetPlusE` p
        limb0 <- mutRead tmp i
        let (limb1, b1) = limbSub limb0 b0
        mutWrite tmp i limb1
        _ <- mutAddCond tmp off p (negate b1) n
        return ()

-- | Multi-precision REDC with @r@ equal to size of @n@.
bnRedcSimple :: Bn -> CountOf Limb -> Bn -> Bn
bnRedcSimple (Bn n) r (Bn t)
    | p == 0       = error "bnRedcSimple: n must be > 0"
    | even n0      = error "bnRedcSimple: n must be odd"
    | tLen > r + p = error "bnRedcSimple: t is too large"
    | p /= r       = error "bnRedcSimple: n has not size r"
    | otherwise    = runST $ do
        tmp <- Mutable.new (r + p)
        mutCopy tmp 0 t 0 tLen
        mutClear tmp (sizeAsOffset tLen) (r + p - tLen)
        loop tmp 0
        let off = sizeAsOffset p
        c <- mutAddMut tmp off p tmp
        _ <- mutSubCond tmp off p (negate c) n
        Bn <$> uFreezeSub tmp off p
  where
    p    = Block.length n
    tLen = Block.length t

    n0   = uIndex n 0
    n0'  = negate (limbModInv n0)

    loop !m i
        | i .==# p  = return ()
        | otherwise = do
            tmpi <- mutRead m i
            mutAddMul1 m i n p (tmpi * n0') >>= mutWrite m i
            loop m (succ i)

bnToMontgomery :: Bn -> CountOf Limb -> Bn -> Bn
bnToMontgomery m !r x = snd $ bnDivMod (bnExtendR x r) m

bnFromMontgomery :: Bn -> CountOf Limb -> Bn -> Bn
bnFromMontgomery = bnRedcSimple

-- | Perform addition in Montgomery form, to return (x + y) mod m.
--
-- The transformation is not good for performance when doing a single operation.
-- But the function is used for unit testing.
bnAddMont :: Bn -> Bn -> Bn -> Bn
bnAddMont x y m =
    let x' = bnToMontgomery m r x
        y' = bnToMontgomery m r y
        z' = add x' y'
     in bnFromMontgomery m r z'
  where
    r = Block.length (unBn m)
    add a b = let (s, c) = bnAddC a b 0 in bnSubCond s (negate c) m

-- | Perform multiplication in Montgomery form, to return (x * y) mod m.
--
-- The transformation is not good for performance when doing a single operation.
-- But the function is used for unit testing.
bnMulMont :: Bn -> Bn -> Bn -> Bn
bnMulMont x y m =
    let x' = bnToMontgomery m r x
        y' = bnToMontgomery m r y
        z' = mul x' y'
     in bnFromMontgomery m r z'
  where
    r = Block.length (unBn m)
    mul a b = bnRedcSimple m r (bnMul a b)

-- | Perform squaring in Montgomery form, to return x^2 mod m.
--
-- The transformation is not good for performance when doing a single operation.
-- But the function is used for unit testing.
bnSqrMont :: Bn -> Bn -> Bn
bnSqrMont x m =
    let x' = bnToMontgomery m r x
        z' = sqr x'
     in bnFromMontgomery m r z'
  where
    r = Block.length (unBn m)
    sqr a = bnRedcSimple m r (bnSqr a)

-- | Perform exponentiation in Montgomery form, to return b^e mod m.
bnPowMont :: Bn -> Bn -> Bn -> Bn
bnPowMont b e m =
    let z' = run nibbles
     in bnFromMontgomery m r z'
  where
    r = Block.length (unBn m)

    mul x y = bnRedcSimple m r (bnMul x y)
    sqr x = bnRedcSimple m r (bnSqr x)
    sqr4 = sqr . sqr . sqr . sqr

    one  = fromLimbs [1]
    one' = bnToMontgomery m r one
    b'   = bnToMontgomery m r b

    -- we will use a window of size of 4 bits, so transform the exponent into a
    -- list of nibbles, starting with the most significant one
    nibbles = Prelude.foldl (nibble limbBits) [] (toLimbs e)

    nibble 0 t w = seq w t
    nibble n t w = nibble (n - 4) ((w .&. 0xf) : t) (unsafeShiftR w 4)

    -- precompute 16 powers of b: b^0 .. b^15
    table  = one' : Prelude.take 15 others
    others = b' : Prelude.map (mul b') others

    -- to iterate over all nibbles we could use a simple fold but want to avoid
    -- the call to sqr4 for the first accumulator, since it is always 1
    run []     = one'
    run (n:ns) = List.foldl' combine (select n) ns

    combine acc n = sqr4 acc `mul` select n

    select = bnSelect table r

-- | Constant-time selection of a number at index @n@ in list @table@.
-- All numbers must have same size equal to argument @len@.
bnSelect :: [Bn] -> CountOf Limb -> Word -> Bn
bnSelect table len !n = withMutable_ len $ \m -> loop m 0
  where
    loop !m i
        | i .==# len = return ()
        | otherwise  = scanWrite m i 0 0 table >> loop m (succ i)

    scanWrite m i acc p []        = seq p $ mutWrite m i acc
    scanWrite m i acc p (Bn a:as) =
        let mask = complement (limbNotZero (n - p))
         in scanWrite m i (acc .|. (uIndex a i .&. mask)) (succ p) as
