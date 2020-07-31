module Main (main) where

import Numeric.Montgomery

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "montgomery"
    [
    ]
