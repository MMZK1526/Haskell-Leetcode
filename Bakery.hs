import           Control.Monad (forM_)
import           Control.Monad.ST (ST, runST)
import           Data.Array.ST (STUArray, readArray, writeArray)
import           Data.Array.Unboxed (UArray, array)
import           Data.Array.Unsafe (unsafeThaw)

-- A bakery can make a profit of P[k] on a cake made with k units of mixture 
-- (for any k ≥ 1). Write a program to compute the maximum profit achievable if 
-- the bakery has a total of n units of mixture at its disposal.
-- Here n and k are natural numbers and the total mixture is to be divided into 
-- whole number units.
-- Your code should use a bottom-up (non-recursive) dynamic programming style.

bakery :: [Int] -> Int
bakery ps = runST $ do
  let len = length ps
  let arr = array (0, len) $ zip [0..] $ 0 : ps :: UArray Int Int
  arrST <- unsafeThaw arr :: ST s (STUArray s Int Int)
  forM_ [1..len] $ \i -> do
    forM_ [1..i] $ \j -> do
      vi <- readArray arrST i
      vj <- readArray arrST j
      vs <- readArray arrST $ i - j
      writeArray arrST i $ max vi $ vj + vs
  readArray arrST len
