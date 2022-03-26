{-# LANGUAGE FlexibleContexts #-}

module RotateArray where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe

-- | A classical problem involving right-rotating an array by k units.
--
-- For example, rotating [1, 2, 3, 4, 5] by 3 results in [3, 4, 5, 1, 2].
rotateArr :: Array Int x -> Int -> Array Int x
rotateArr arr k = runST $ do
  arrST <- (thaw :: Array Int x -> ST s (STArray s Int x)) arr
  let len       = length arr
  let bp        = len - k `mod` len
  let swaps i j = when (i < j) $ do
        vi <- readArray arrST i
        vj <- readArray arrST j
        writeArray arrST i vj
        writeArray arrST j vi
        swaps (i + 1) (j - 1)
  swaps 0 (bp - 1)
  swaps bp (len - 1)
  swaps 0 (len - 1)
  unsafeFreeze arrST
