module Main where

import Control.Monad.State.Lazy
import Change

data Coin = Pound | Fifty | Twenty | Ten | Five | Two | Penny
               deriving (Eq, Ord, Enum, Show)

main :: IO ()
main = do
  putStrLn "Enter a number and I'll count out the change"
  str <- getLine
  if null str then return ()
  else do let i = read str :: Int
              coins = evalState makeChange i
          putStrLn $ show coins
          main

