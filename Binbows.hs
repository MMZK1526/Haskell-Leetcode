import Control.Monad
import Data.Array
import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Ord

-- Given a price and a list of coin values, find a way to pay for the price
-- using the most number of coins.
--
-- Paying an amount more than the price is allowed, in which case you’d get
-- change. Find a way to pay that maximises the net decrease in the number of 
-- coins you have.
--
-- Assume change is always given using a given set of denominations, in a way
-- that uses the least number of coins, e.g. if there are ¥1 and ¥10
-- denominations available, and you need a ¥10 change, you would get a ¥10 coin
-- instead of ten ¥1 coins.
--
-- This is a problem from my friend @Sweeper777 who wishes to spend his
-- Japanese coins as much as possible.

-- | Original problem. Given the list of coins I have and the list of coin
-- denominations, find the minimum number of coins that may remain with me for
-- spending the given amount, as well as the list of coins to be spent. I am
-- allowed to spend more than that, but will then receive change with minimal
-- number of coins possible. When the remaining number of coins is the same,
-- less coins to be spent is preferred.
--
-- If it is not possible to spend the given amount, return @Nothing@.
--
-- Assume that the given amount is always less than the sum of the given coins,
-- and the coin denominations are always positive.
sweeper :: [Int] -> [Int] -> Int -> Maybe (Int, [Int])
sweeper myMoney coins amount =
  case maximumBy (comparing $ fmap (second (Down . sum))) choices of
      Nothing      -> Nothing
      Just (s, cs) -> Just (length myMoney - s, cs)
  where
    allMoney                = sum myMoney
    preciseSpendings        = drop amount $ maxSpending myMoney allMoney
    changes                 = change coins (allMoney - amount)
    choices                 = zipWith (liftM2 worker) preciseSpendings changes
    worker (s, cs) c        = (s - c, cs)

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate r f = array r [(i, f i) | i <- range r]

suffixes :: [a] -> [[a]]
suffixes []       = [[]]
suffixes (x : xs) = (x : xs) : suffixes xs

-- | Given the list of coin denominations and the max amount of money to be
-- changed, find list of the minimum number of coins needed to make the change
-- from 0 to the given max amount.
change :: [Int] -> Int -> [Maybe Int]
change coins amount = elems cache
  where
    cache  = tabulate (0, amount) worker
    worker x
      | x == 0    = Just 0
      | otherwise = case catMaybes [ fmap succ (cache ! (x - c))
                                   | c <- coins, x - c >= 0 ] of
        [] -> Nothing
        xs -> Just (minimum xs)

-- | Given the list of coins I have and the max amount of money to be spent,
-- find the maximum number of coins I can spend for exactly the given amount,
-- with the given amount ranging from 0 to the given max amount. Also returns
-- the list of coins to be spent.
maxSpending :: [Int] -> Int -> [Maybe (Int, [Int])]
maxSpending myMoney amount
  = snd <$> filter ((== len) . snd . fst) (assocs cache)
  where
    len         = length myMoney
    coinSuffArr = listArray (0, len - 1) $ suffixes myMoney
    cache       = tabulate ((0, 0), (amount, len)) $ uncurry worker
    worker x r
      | x == 0    = Just (0, [])
      | r == 0    = Nothing
      | x - c < 0 = cache ! (x, r - 1)
      | otherwise = max (bimap succ (c :) <$> cache ! (x - c, r - 1))
                        (cache ! (x, r - 1))
      where
        c = head $ coinSuffArr ! (len - r)
