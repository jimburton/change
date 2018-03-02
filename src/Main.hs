module Main where

import Data.Maybe (fromJust)

data Coin = Pound | Fifty | Twenty | Ten | Five | Two | Penny
               deriving (Eq, Ord, Enum, Show)

values :: [(Coin, Int)]
values = zip [Pound .. Penny] [100, 50, 20, 10, 5, 2, 1]

getCoin :: Int -> (Coin, Int)
getCoin i = head $ dropWhile ((>i) . snd) values

coinDiv :: Int -> (Coin, Int) -> ([Coin], Int)
coinDiv n (c,i) = let (d,m) = n `divMod` i in
                  (replicate d c, m)

makeChange :: Int -> [Coin]
makeChange 0 = []
makeChange n = let (cs, rem) = coinDiv n (getCoin n) in
               cs ++ (makeChange rem)

main :: IO ()
main = putStrLn "Hello, Haskell!"
