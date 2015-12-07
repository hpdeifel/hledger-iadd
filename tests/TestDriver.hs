module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified DateParserTest as DP

main :: IO ()
main = defaultMain (testGroup "Tests" tests)

tests :: [TestTree]
tests = [ DP.tests ]
