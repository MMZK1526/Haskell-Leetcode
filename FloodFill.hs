{-# LANGUAGE FlexibleContexts #-}

module FloodFill where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.Functor

-- Source: https://leetcode.com/problems/max-area-of-island/

-- You are given an m x n binary matrix grid. An island is a group of 1's 
-- (representing land) connected 4-directionally (horizontal or vertical.)
-- You may assume all four edges of the grid are surrounded by water.
--
-- The area of an island is the number of cells with a value 1 in the island.
--
-- Return the maximum area of an island in grid. If there is no island, return 0.

maxAreaOfIsland :: [[Int]] -> Int
maxAreaOfIsland list2D = runST $ do
  arrST        <- forM arr2D thaw' >>= thaw'
  touchST      <- forM ((False <$) <$> arr2D) unsafeThaw' >>= unsafeThaw'
  (rInf, rMax) <- getBounds arrST
  fmap (maximum . concat) $ forM [rInf..rMax] $ \i -> do
    arr          <- readArray arrST i
    (cInf, cMax) <- getBounds arr
    forM [cInf..cMax] $ \j -> do
      let count x y = if x < rInf || x > rMax || y < cInf || y > cMax
            then return 0
            else do
              touch <- readArray touchST x >>= flip readArray y
              value <- readArray arrST x >>= flip readArray y
              readArray touchST x >>= flip (`writeArray` y) True
              if touch || value == 0
                then return 0
                else foldM (fmap . (+)) 1 [ count (x - 1) y, count (x + 1) y
                                          , count x (y - 1), count x (y + 1) ]
      count i j
  where
    thaw'       = thaw :: Array Int a -> ST s (STArray s Int a)
    unsafeThaw' = unsafeThaw :: Array Int a -> ST s (STArray s Int a)
    arr2D       = listArray (0, length list2D - 1)
                $ map (\list -> listArray (0, length list - 1) list) list2D
