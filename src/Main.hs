module Main where

import Data.Maybe (fromJust)

data Coin = Pound | Fifty | Twenty | Ten | Five | Two | Penny
               deriving (Eq, Ord, Enum, Show)

{-makeChange :: Int -> Int -> Maybe (Int, [Change])
makeChange cost paid | cost > paid = Nothing
                     | cost `div` 100 > 0 = Just ()
-}

values :: [(Coin, Int)]
values = zip [Pound .. Penny] [100, 50, 20, 10, 5, 2, 1]

getCoin :: Int -> Coin
getCoin i = fst $ head $ dropWhile ((>i) . snd) values

coinDiv :: Int -> Coin -> ([Coin], Int)
coinDiv n c = let i     = fromJust $ lookup c values
                  (d,m) = n `divMod` i in
              if n < i then coinDiv n (succ c) else (replicate d c, m)

makeChange :: Int -> [Coin]
makeChange 0 = []
makeChange n = let c = getCoin n
                   (cs, rem) = coinDiv n c
                   cs' = makeChange rem in
               cs ++ cs'

main :: IO ()
main = putStrLn "Hello, Haskell!"
