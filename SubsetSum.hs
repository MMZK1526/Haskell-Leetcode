module SubsetSum where

-- Calculate the number of subsets of {1, 2, ..., 2000} of which the sum is a
-- multiple of 5.

import Data.Array

tabulate :: Ix i => (i, i) -> (i -> a) -> Array i a
tabulate (u, v) f = array (u, v) [ (i, f i) | i <- range (u, v)]

answer :: Integer
answer = subsetSum 2000 5

subsetSum :: Int -> Int -> Integer
subsetSum n m = table ! (n, 0)
  where
    table = tabulate ((0, 0), (n, m - 1)) go
    go (0, 0) = 1
    go (0, _) = 0
    go (x, r) = table ! (x - 1, r)
              + table ! (x - 1, (m + r - x `mod` m) `mod` m)
