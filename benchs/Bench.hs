module Main where

import Crypto.Number.ModArithmetic

import Gauge.Main

import Numeric.Montgomery

data Params = Params
    { paramsName :: String
    , paramsModulus :: Integer
    }

allParams :: [Params]
allParams =
    [ Params  -- 255-bit prime from curve25519
        { paramsName = "curve25519"
        , paramsModulus = 2^(255::Int) - 19
        }
    , Params  -- 3072-bit prime from ffdhe3072
        { paramsName = "ffdhe3072"
        , paramsModulus = 0xffffffffffffffffadf85458a2bb4a9aafdc5620273d3cf1d8b9c583ce2d3695a9e13641146433fbcc939dce249b3ef97d2fe363630c75d8f681b202aec4617ad3df1ed5d5fd65612433f51f5f066ed0856365553ded1af3b557135e7f57c935984f0c70e0e68b77e2a689daf3efe8721df158a136ade73530acca4f483a797abc0ab182b324fb61d108a94bb2c8e3fbb96adab760d7f4681d4f42a3de394df4ae56ede76372bb190b07a7c8ee0a6d709e02fce1cdf7e2ecc03404cd28342f619172fe9ce98583ff8e4f1232eef28183c3fe3b1b4c6fad733bb5fcbc2ec22005c58ef1837d1683b2c6f34a26c1b2effa886b4238611fcfdcde355b3b6519035bbc34f4def99c023861b46fc9d6e6c9077ad91d2691f7f7ee598cb0fac186d91caefe130985139270b4130c93bc437944f4fd4452e2d74dd364f2e21e71f54bff5cae82ab9c9df69ee86d2bc522363a0dabc521979b0deada1dbf9a42d5c4484e0abcd06bfa53ddef3c1b20ee3fd59d7c25e41d2b66c62e37ffffffffffffffff
        }
    ]

benchExpSafe :: Params -> Benchmark
benchExpSafe params = bench (paramsName params) $ nf (expSafe b e) m
  where
    b = m - 1
    e = m - 1
    m = paramsModulus params

benchBnPowMont :: Params -> Benchmark
benchBnPowMont params = bench (paramsName params) $ nf (bnPowMont b e) m
  where
    b = bnSub1 m 1
    e = bnSub1 m 1
    m = fromNatural . fromIntegral $ paramsModulus params

main :: IO ()
main = defaultMain
    [ bgroup "montgomery"
        [ bgroup "expSafe" $ map benchExpSafe allParams
        , bgroup "bnPowMont" $ map benchBnPowMont allParams
        ]
    ]
