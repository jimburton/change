module Main where

import Test.HUnit
import Change

-- Test for getCoin
testGetCoin :: Test
testGetCoin = TestCase (assertEqual "for (getCoin 3)," (Two, 2) (getCoin 3))

-- Test for coinDiv
testCoinDiv :: Test
testCoinDiv = TestCase (assertEqual "for (coinDiv 3 (Penny, 1)),"
                         ([Penny, Penny, Penny], 0) (coinDiv 3 (Penny, 1)))

-- Test makeChange for non-zero
testMakeChange :: Test
testMakeChange = TestCase (assertEqual "for (makeChange 31),"
                           [Twenty, Ten, Penny] (makeChange 31))

-- Test makeChange for zero
testMakeChangeZero :: Test
testMakeChangeZero = TestCase (assertEqual "for (makeChange 0),"
                           [] (makeChange 31))

-- All tests combined
tests :: Test
tests = TestList [TestLabel "test getCoin" testGetCoin
                 , TestLabel "test coinDiv" testCoinDiv
                 , TestLabel "test makeChange" testMakeChange
                 , TestLabel "test makeChange input zero" testMakeChangeZero]


main :: IO Counts
main = runTestTT tests
