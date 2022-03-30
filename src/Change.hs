module Change where

import Control.Monad.State.Lazy ( MonadState(put, get)
                                , State )

-- | Data type for coin values.
data Coin = Pound | Fifty | Twenty | Ten | Five | Two | Penny
               deriving (Eq, Ord, Enum, Show)

-- | Pairs each coin with its monetary value in pennies.  
values :: [(Coin, Int)]
values = zip [Pound .. Penny] [100, 50, 20, 10, 5, 2, 1]

-- | Finds the highest denomination coin that is smaller than the argument.
getCoin :: Int -> (Coin, Int)
getCoin i = head $ dropWhile ((>i) . snd) values

-- | The first argument, n, is a number of pennies, while the second argument, (c, i),
--    is a pair of a coin and its monetary value. The return value is a list of coins
--    where each coin is equal to c and the length of the list is equal to the largest
--    number of c coins that is less than or equal to n, paired with the remainder after
--    the value of these coins is subtracted from n.
coinDiv :: Int -> (Coin, Int) -> ([Coin], Int)
coinDiv n (c,i) = let (d,m) = n `divMod` i in
                  -- trace ("coinDiv: n: "++show n) (replicate d c, m)
                  (replicate d c, m)

-- | Takes a number of pennies and returns the shortest list of coins that make up
--    that value.
makeChange :: State Int [Coin]
makeChange = do
  i <- get
  if i > 0 then do
      let (cs, r) = coinDiv i (getCoin i)
      put r
      rest <- makeChange
      pure (cs ++ rest)
      -- to do the two lines above as a one-liner:
      -- makeChange >>= pure . (cs ++)
  else return []
