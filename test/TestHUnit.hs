module Main where

import Test.HUnit
import Change

testGetCoin :: Test
testGetCoin = TestCase (assertEqual "for (getCoin 3)," (Two, 2) (getCoin 3))

testCoinDiv :: Test
testCoinDiv = TestCase (assertEqual "for (coinDiv 3 (Penny, 1)),"
                         ([Penny, Penny, Penny], 0) (coinDiv 3 (Penny, 1)))

testMakeChange :: Test
testMakeChange = TestCase (assertEqual "for (makeChange 31),"
                           ([Twenty, Ten, Penny]) (makeChange 31))

tests :: Test
tests = TestList [TestLabel "test getCoin" testGetCoin
                 , TestLabel "test coinDiv" testCoinDiv
                 , TestLabel "test makeChange" testMakeChange]


main :: IO Counts
main = runTestTT tests
