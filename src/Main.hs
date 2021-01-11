module Main where

import Data.List (group
                 , intercalate)
import Data.Char (toLower)
import Change

prettyPrint :: [Coin] -> String
prettyPrint = intercalate ", " . map (prettyPair . (\ds -> (length ds, head ds))) . group 
  where prettyPair (i, c)     = show i ++ " " ++ prettyCoin (i, c)
        prettyCoin (i, Pound) = "pound coin" ++ plural i
        prettyCoin (_, Penny) = "pence"
        prettyCoin (i, c)     = let cStr = show c in
                                  toLower (head cStr) : tail cStr ++ " pence piece" ++
                                  plural i
        plural i              = if i>1 then "s" else ""

main :: IO ()
main = do
  putStrLn "Enter a number and I'll count out the change"
  str <- getLine
  if null str then return ()
  else do let coins = makeChange (read str::Int)
          putStrLn $ prettyPrint coins
          main

