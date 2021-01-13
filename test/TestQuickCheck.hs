module Main where

import Control.Monad.State.Lazy
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Maybe (fromJust)

import Change

{--
getCoin is intended to work with positive integers only and its behaviour
for any other input is undefined. We use the built-in QuickCheck generator
`Positive' to make sure that only positive integers are supplied. The test
checks that the coin supplied by getCoin is the largest that is less than or
equal to the input.
--}
prop_getCoin :: Positive Int -> Bool
prop_getCoin (Positive x) = let (c,i) = getCoin x in
  x >= i && fst (head (dropWhile ((>x) . snd) values)) == c  

{--
To test coinDiv we need to tell QuickCheck how to supply an integer
which is at least as large as the value of the (coin, int) value it
picked in the previous step. We create a new generator that will produce
an int which is larger than a given value.
--}
genBigger :: Int -> Gen Int
genBigger i = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> i)

{--
We use this generator to create the test using `do' syntax.
--}
prop_coinDiv :: Gen Bool
prop_coinDiv = do (c,i) <- elements values -- get an arbitrary (coin, int) pair
                  x <- genBigger i
                  let (cs, r) = coinDiv x (c,i)
                  return (((length cs)*i)+r == x)

{--
The last test is simpler but like the first one, needs positive integers.
--}
prop_makeChange :: Positive Int -> Bool
prop_makeChange (Positive x) =
  sum (map (fromJust . (\c -> lookup c values)) $ (evalState makeChange x)) == x  

tests :: [Test]
tests = [ testProperty "prop_getCoin" prop_getCoin
        , testProperty "prop_coinDiv" prop_coinDiv
        , testProperty "prop_makeChange" prop_makeChange
        ]
        
main :: IO ()
main = defaultMain tests


