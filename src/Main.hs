module Main where

import Control.Monad.State.Lazy

data Coin = Pound | Fifty | Twenty | Ten | Five | Two | Penny
               deriving (Eq, Ord, Enum, Show)

values :: [(Coin, Int)]
values = zip [Pound .. Penny] [100, 50, 20, 10, 5, 2, 1]

getCoin :: Int -> (Coin, Int)
getCoin i = head $ dropWhile ((>i) . snd) values

coinDiv :: Int -> (Coin, Int) -> ([Coin], Int)
coinDiv n (c,i) = let (d,m) = n `divMod` i in
                  (replicate d c, m)

makeChange :: State Int [Coin]
makeChange = do
  i <- get
  if i > 0 then do
      let (cs, rem) = coinDiv i (getCoin i)
      put rem
      rest <- makeChange
      return $ cs ++ rest
  else return []

main :: IO ()
main = do
  putStrLn "Enter a number and I'll count out the change"
  str <- getLine
  if null str then return ()
  else do let i = read str :: Int
              coins = evalState makeChange i
          putStrLn $ show coins
          main

