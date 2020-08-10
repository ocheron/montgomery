{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Data.Bits
import Data.List (dropWhileEnd)

import Numeric.Montgomery

import Test.Tasty
import Test.Tasty.QuickCheck

numLimbs :: Int
numLimbs = 8

newtype L = L Limb deriving Show

instance Arbitrary L where
    arbitrary = do
        bits <- choose (0, limbBits)
        limb <- arbitraryBoundedIntegral
        return (L $ shiftR limb bits)

arbitraryBn :: Int -> Gen Bn
arbitraryBn 0 = return $ fromLimbs []
arbitraryBn n = do
    L t <- arbitrary
    others <- vectorOf (pred n) arbitraryBoundedIntegral
    return $ fromLimbs (others ++ [t])

instance Arbitrary Bn where
    arbitrary = choose (0, 2 * numLimbs) >>= arbitraryBn

newtype Sz a = Sz a deriving Show

instance Arbitrary (Sz Bn) where
    arbitrary = Sz <$> arbitraryBn numLimbs

newtype SzOdd a = SzOdd a deriving Show

instance Arbitrary (SzOdd Bn) where
    arbitrary = do
        Sz a <- arbitrary
        let (l : ls) = toLimbs a
        return $ SzOdd $ fromLimbs ((1 .|. l) : ls)

newtype Zero a = Zero a deriving Show

instance Arbitrary (Zero Bn) where
    arbitrary = Zero . z <$> choose (0, 2 * numLimbs)

newtype One a = One a deriving Show

instance Arbitrary (One Bn) where
    arbitrary = One . s 1 <$> choose (1, 2 * numLimbs)

newtype Ne a = Ne a deriving Show  -- not empty

instance Arbitrary (Ne Bn) where
    arbitrary = Ne <$> (choose (1, 2 * numLimbs) >>= arbitraryBn)

newtype Nz a = Nz a deriving Show  -- not zero

instance Arbitrary (Nz Bn) where
    arbitrary = do
        Ne bn <- arbitrary
        if all (== 0) (toLimbs bn) then arbitrary else return (Nz bn)

sz :: Bn -> Int
sz = length . toLimbs

z :: Int -> Bn
z n = fromLimbs (replicate n 0)

s :: Limb -> Int -> Bn
s i n = fromLimbs (i : replicate (pred n) 0)

extend :: Int -> Bn -> Bn
extend n = fromLimbs . (++ replicate n 0) . toLimbs

main :: IO ()
main = defaultMain $ testGroup "montgomery"
    [ testGroup "limbs"
        [ testProperty "limbDiv2x1" $ \(L u0) (L u1') (L d') ->
            let u1 = clearBit u1' (limbBits - 1)
                d = setBit d' (limbBits - 1)
                numerator = shiftL (toInteger u1) limbBits + toInteger u0
                denominator = toInteger d
                (q, r) = limbDiv2x1 (u0, u1) d
             in (toInteger q, toInteger r) === numerator `divMod` denominator
        ]
    , testGroup "bn"
        [ testGroup "conversions"
            [ testProperty "fromLimbs . toLimbs == id" $ \bn ->
                fromLimbs (toLimbs bn) === bn
            , testProperty "toLimbs . fromLimbs == id" $ \limbs ->
                toLimbs (fromLimbs limbs) === limbs
            , testProperty "fromNatural . toNatural == id" $ \bn ->
                let r = fromNatural (toNatural bn)
                in extend (sz bn - sz r) r === bn
            , testProperty "toNatural . fromNatural == id" $ \i ->
                let n = fromIntegral (abs i :: Integer)
                in toNatural (fromNatural n) === n
            ]
        , testGroup "add"
            [ testProperty "a + 0 == a" $ \a ->
                bnAdd a (z $ sz a) === a
            , testProperty "0 + a == a" $ \a ->
                bnAdd (z $ sz a) a === a
            , testProperty "(a + b) + c == a + (b + c)" $ \(Sz a) (Sz b) (Sz c) ->
                bnAdd (bnAdd a b) c === bnAdd a (bnAdd b c)
            , testProperty "a + b == b + a" $ \(Sz a) (Sz b) ->
                bnAdd a b === bnAdd b a
            ]
        , testProperty "add1" $ \(Ne a) (L b) -> bnAdd1 a b === bnAdd a (s b $ sz a)
        , testGroup "negate"
            [ testProperty "-(-a) == a" $ \a -> bnNegate (bnNegate a) === a
            , testProperty "a + (-a) == 0" $ \a -> bnAdd a (bnNegate a) === z (sz a)
            ]
        , testGroup "sub"
            [ testProperty "a - 0 == a" $ \a ->
                bnSub a (z $ sz a) === a
            , testProperty "0 - a == -a" $ \a ->
                bnSub (z $ sz a) a === bnNegate a
            , testProperty "(a - b) - c == a - (b + c)" $ \(Sz a) (Sz b) (Sz c) ->
                bnSub (bnSub a b) c === bnSub a (bnAdd b c)
            , testProperty "a - b == -(b - a)" $ \(Sz a) (Sz b) ->
                bnSub a b === bnNegate (bnSub b a)
            ]
        , testProperty "sub1" $ \(Ne a) (L b) -> bnSub1 a b === bnSub a (s b $ sz a)
        , testGroup "mul"
            [ testProperty "a * 0 == 0" $ \a (Zero b) ->
                bnMul a b === z (sz a + sz b)
            , testProperty "0 * a == 0" $ \(Zero a) b ->
                bnMul a b === z (sz a + sz b)
            , testProperty "a * 1 == a" $ \a (One b) ->
                bnMul a b === extend (sz b) a
            , testProperty "1 * a == a" $ \(One a) b ->
                bnMul a b === extend (sz a) b
            , testProperty "(a * b) * c == a * (b * c)" $ \a b c ->
                bnMul (bnMul a b) c === bnMul a (bnMul b c)
            , testProperty "a * b == b * a" $ \a b ->
                bnMul a b === bnMul b a
            , testProperty "sqr(a) == a * a" $ \a ->
                bnSqr a === bnMul a a
            ]
        , testProperty "mul1" $ \a (L b) -> bnMul1 a b === bnMul a (fromLimbs [b])
        , testGroup "shifts"
            [ testProperty "shiftL a 0 == a" $ \a ->
                bnShiftL a 0 === a
            , testProperty "shiftL a 1 == a + a" $ \a ->
                bnShiftL a 1 === bnAdd a a
            , testProperty "shiftL a 2 == a + a + a + a" $ \a ->
                bnShiftL a 2 === let b = bnAdd a a in bnAdd b b
            , testProperty "shiftR a n" $ \a (NonNegative n) ->
                toNatural (bnShiftR a n) === shiftR (toNatural a) n
            ]
        , testGroup "div"
            [ testProperty "divMod" $ \(Nz d) n ->
                let leneff = length (dropWhileEnd (== 0) $ toLimbs d)
                    (q, r) = bnDivMod n d
                in toNatural r < toNatural d &&
                    toNatural q * toNatural d + toNatural r == toNatural n &&
                    length (toLimbs r) == length (toLimbs d) &&
                    length (toLimbs q) == max (length (toLimbs n) - leneff + 1) 0
            , testProperty "modSlow" $ \(Sz x) (Sz m) ->
                toNatural (bnModSlow x m) === toNatural x `mod` toNatural m
            ]
        ]
    , testGroup "modular"
        [ testProperty "redc" $ \(SzOdd n) (Sz t) ->
            let x = bnRedc n (fromIntegral numLimbs) t
             in toNatural x < toNatural n &&
                (shiftL (toNatural x) (limbBits * numLimbs) - toNatural t) `mod` toNatural n == 0
        , testProperty "redc-simple" $ \(SzOdd n) (Sz t) ->
            let r = sz n
                x = bnRedcSimple n (fromIntegral r) t
             in toNatural x < toNatural n &&
                (shiftL (toNatural x) (limbBits * r) - toNatural t) `mod` toNatural n == 0
        , testProperty "add" $ \(SzOdd m) a b ->
            let expected = (toNatural a + toNatural b) `mod` toNatural m
             in toNatural (bnAddMont a b m) === expected
        , testProperty "mul" $ \(SzOdd m) a b ->
            let expected = (toNatural a * toNatural b) `mod` toNatural m
             in toNatural (bnMulMont a b m) === expected
        , testProperty "sqr" $ \(SzOdd m) a ->
            bnSqrMont a m === bnMulMont a a m
        , testProperty "b^0 == 1" $ \(SzOdd m) b (Zero e) ->
            bnPowMont b e m === s 1 (sz m)
        , testProperty "b^1 == b" $ \(SzOdd m) b (One e) ->
            let (_, r) = bnDivMod b m in bnPowMont b e m === r
        , testProperty "b^2 == b * b" $ \(SzOdd m) b (One e) ->
            bnPowMont b (bnAdd e e) m === bnMulMont b b m
        , testProperty "(a.b)^e == (a^e).(b^e)" $ \(SzOdd m) a b e ->
            bnPowMont (bnMul a b) e m === bnMulMont (bnPowMont a e m) (bnPowMont b e m) m
        , testProperty "b^(x.y) == (b^x)^y" $ \(SzOdd m) b x y ->
            bnPowMont b (bnMul x y) m === bnPowMont (bnPowMont b x m) y m
        ]
    ]
