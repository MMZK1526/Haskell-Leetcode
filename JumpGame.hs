module JumpGame where

-- source: https://leetcode.com/problems/jump-game/

-- You are given an integer array. You are initially positioned at the array's
-- first index, and each element in the array represents your maximum jump
-- length at that position.
--
-- Return true if you can reach the last index, or false otherwise.

canJump :: [Int] -> Bool
canJump = (== 0) . foldr go 0 . zip [0..]
  where
    go (i, x) last
      | last <= i + x = i
      | otherwise     = last
