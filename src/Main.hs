module Main where

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
main = do
  putStrLn "Enter a number and I'll count out the change"
  str <- getLine
  if null str then return ()
  else do let coins = makeChange $ (read str::Int)
          putStrLn $ show coins
          main

