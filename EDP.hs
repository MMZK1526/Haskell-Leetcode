module EDP where

import Data.Bool
import Data.Array
import Data.List

-- The Edit-String Problem.
-- Calculate how many insertion/deletion/update does it take to transform one
-- string to another.

-- | A recursive approach.
-- The distance between "abc" and "wxyz" is the minimum of
-- 1) 1 + distance between "bc" and "wxyz";
-- 2) 1 + distance between "abc" and "xyz";
-- 3) distance between "bc" and "xyz" + if 'a' == 'w' then 0 else 1.
dist :: String -> String -> Int
dist "" str = length str
dist str "" = length str
dist xz@(x : xs) yz@(y : ys)
  = minimum [ 1 + dist xs yz
            , bool 1 0 (x == y) + dist xs ys
            , 1 + dist xz ys
            ]

-- | The same algorithm as dist, but using memoisation.
distT :: String -> String -> Int
distT xz yz = table ! (lx, ly)
  where
    xArr  = array (0, lx - 1) $ zip [0..] xz
    yArr  = array (0, ly - 1) $ zip [0..] yz
    lx    = length xz
    ly    = length yz
    f m 0 = m
    f 0 n = n
    f m n = minimum
      [ 1 + table ! (m - 1, n)
      , bool 1 0 (xArr ! (lx - m) == yArr ! (ly - n)) + table ! (m - 1, n - 1)
      , 1 + table ! (m, n - 1)
      ]
    table = array ((0, 0), (lx, ly))
      [((i, j), f i j) | i <- [0..lx], j <- [0..ly]]

-- | Also calculates the steps.
distStep :: String -> String -> (Int, [String])
distStep xz yz = let (d, cs) = table ! (lx, ly) in (d, run xz 0 0 cs)
  where
    run str _ _ []
      = [yz]
    run str i j (c : cs)
      = case c of
        Delete -> str : run (deleteAt i str) i j cs
        Insert -> str : run (insertAt i (yArr ! j) str) (i + 1) (j + 1) cs
        Modify -> str : run (replaceAt i (yArr ! j) str) (i + 1) (j + 1) cs
        None   -> run str (i + 1) (j + 1) cs
    xArr  = array (0, lx - 1) $ zip [0..] xz
    yArr  = array (0, ly - 1) $ zip [0..] yz
    lx    = length xz
    ly    = length yz
    f m 0 = (m, replicate m Delete)
    f 0 n = (n, replicate n Insert)
    f m n = minimumBy (\x y -> compare (fst x) (fst y))
      [ let (d, cs) = table ! (m - 1, n) in  (d + 1, Delete : cs)
      , let (d, cs) = table ! (m - 1, n - 1) in 
          if xArr ! (lx - m) == yArr ! (ly - n)
            then (d, None : cs)
            else (d + 1, Modify : cs)
      , let (d, cs) = table ! (m, n - 1) 
        in  (d + 1, Insert : cs)
      ]
    table = array ((0, 0), (lx, ly))
      [((i, j), f i j) | i <- [0..lx], j <- [0..ly]]

data ChangeType = Delete | Insert | Modify | None
  deriving (Eq, Show)

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = let (l, r) = splitAt i xs in l ++ x : r

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = let (l, _ : r) = splitAt i xs in l ++ r

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = let (l, _ : r) = splitAt i xs in l ++ x : r
