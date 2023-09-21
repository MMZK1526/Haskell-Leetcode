module LS where

-- This is a question from the NWERC 2021 Contest.

-- Given a list of ordered words and a fixed width, rearrange the words column-
-- first to minimise the height of the display while not exceeding the width
-- limit.

-- The full description of the problem can be viewed from here:
-- https://www.nwerc.eu/files/nwerc2021problems.pdf.

import Control.Monad (forM_, forM, when)
import Data.Array (Array, bounds, listArray, (!))
import Data.Maybe (fromJust, isNothing)
import Data.Foldable (minimumBy)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (( . f) . compare . f)

fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _, _) = a

-- | Given the width and the list of words, return the minimum height and a
--  list of indices where a new column should start immediately after them
--  (will be referred as the "split configuration").
--
-- Therefore, the number of columns is the length of that list plus one.
split :: Foldable t => Int -> [t a] -> (Int, [Int])
split w strs
  -- We first find the lower and upper bound of the height.
  -- 
  -- The trivial lower bound is 0; for the height bound, we know that it is
  -- possible to have a least c := w `div` l columns, where l is the length of 
  -- the longest word. Under this configuration, we can also split at wherever
  -- we like. Therefore it is guaranteed that we can display the words in
  -- count `div` c + 1 rows, where count is the number of the words.
  --
  -- Assume that for any height h, we can determine if a split that has at most
  -- h rows within the width limit is possible. Then since we already have the
  -- upper and lower bounds, we can conduct a binary search to find the least
  -- possible height, which is the job of the "rush" function.
  = rush 0 $
         max 2 $
             1 + len `div` (w `div` maximum [length str | str <- strs])
  where
    rush i j
      | j == i + 1    = let (_, h, _, _, ls) = fromJust (run j)
                        in  (h, reverse ls)
      | isNothing res = rush mid j
      | otherwise     = if   mid == i + 1
                        then let (_, h, _, _, ls) = fromJust res
                             in  (h, reverse ls)
                        else rush i mid
      where
        res = run mid -- determine if the split with "mid" rows is possible
        mid = (i + j) `div` 2
    len    = length strs
    aStrs  = listArray (0, len - 1) strs
    -- This function takes a height h and determines if we can split the words
    -- into at most h rows without exceeding the width limit.
    run h  = run' 0
      where
        -- We use a helper "run'" that outputs the split configuration with the
        -- minimum width to display the first (i + 1) words, or Nothing if
        -- such split does not exist since the minimum width exceeds w.
        --
        -- When the minimum width is larger than w, we terminates with Nothing
        -- since we cannot split the words into h rows. Otherwise we try with
        -- the next index. When we reached the last index and a split is still
        -- possible, we return the split.
        run' i
          | fst5 cur > w = Nothing
          | i == len - 1 = Just cur
          | otherwise    = run' $ i + 1
          where
            cur = table ! i
        -- The i-th index of table contains the split using h rows with the
        -- minimum number of width. More specifically, each entry is a 
        -- quintuple with i) the width, ii) the height, iii) the width of the 
        -- last column, iv) the height of the last column, and iv) the split 
        -- configuration.
        --
        -- The table is constructed with the "go" function that does the
        -- calculation, and its purpose is to optimise the recursive pattern
        -- with dynamic programming.
        table = listArray (0, len - 1) $ map go [0..]
        -- When i is 0, apparently we simply throw the first word onto the
        -- screen, thus the total width is the length of the word, height is 1,
        -- and there is no split.
        --
        -- Otherwise, we have two options: either append the (i + 1)-th word
        -- to the bottom of the last column, or group the last few words into
        -- a new column. The first option is encoded in 'append', while the
        -- second option (one can backtrack at most h words) is encoded in
        -- 'newCols'. We then search for the one with the least width from the
        -- options.
        go i
          | i == 0    = (curLen, 1, curLen, 1, [])
          | h' == h   = minimumOn fst5 newCols
          | otherwise = minimumOn fst5 $ append : newCols
          where
            append               = ( w' - k' + max k' curLen
                                   , max h' $ d' + 1
                                   , k'
                                   , d' + 1
                                   , l'
                                   )
            curLen               = length curStr
            curStr               = aStrs ! i
            (w', h', k', d', l') = table ! (i - 1)
            beginning            = max 0 (i - h)
            newCols              = do
              (j, m) <- zip [beginning..(i - 1)] $
                            scanr1 max [ length $ aStrs ! k 
                                       | k <- [(beginning + 1)..i]
                                       ]
              let (w', h', _, _, l') = table ! j
              let count              = i - j
              return (w' + m + 1, max h' count, m, count, j : l')

-- | Turns a list of words and a split configuration into columns.
--
-- Returns an array of heights of each column and an array of columns, where
-- each column is an array of words.
toColumns :: [String] -> [Int] -> (Array Int Int, Array Int (Array Int String))
toColumns strs ls
  = ( listArray (0, len - 1) ws
    , listArray (0, len - 1) $ map (\c -> listArray (0, length c - 1) c) cs
    )
  where
    (ws, cs)           = go (-1) strs ls
    len                = length ws
    go _ strs []       = ([maxLen strs], [strs])
    go c strs (l : ls) = let (strs', strs'') = splitAt (l - c) strs
                             (ws, cs)   = go l strs'' ls
                         in  (maxLen strs' : ws, strs' : cs)
    maxLen strs        = maximum [length str | str <- strs]

-- | Prints a column pair from "toColumns" in the desired format.
-- 
-- Also takes the height as an auxiliary argument despite it is not necessary.
outputLogic :: Int -> (Array Int Int, Array Int (Array Int String)) -> IO ()
outputLogic h (ws, cs) = do
  let (_, maxCol) = bounds ws
  forM_ [0..(h - 1)] $ \row -> do
    forM_ [0..maxCol] $ \col -> do
      let (_, maxRow) = bounds $ cs ! col
      if   row <= maxRow
      then do
        let word = cs ! col ! row
        putStr word
        putStr $ replicate (max 1 $ 1 + ws ! col - length word) ' '
      else putStr $ replicate (1 + ws ! col) ' '
    when (row < h) $ putStrLn ""

-- | A sample input from the competition.
exp1 :: (Int, [String])
exp1 = (15, ["pppp", "ppppp", "pq", "pqab", "xyzff"])

-- | A sample input from the competition.
exp2 :: (Int, [String])
exp2 = (30, [ "algorithm", "contest", "eindhoven", "icpc", "nwerc"
            , "programming", "regional", "reykjavik", "ru"
            ]
       )

-- | Use with "exp1" or "exp2".
runExample :: (Int, [String]) -> IO ()
runExample (width, words) = do
  let (depth, ls) = split width words
  let cols        = toColumns words ls
  outputLogic depth cols

problem :: IO ()
problem = do
  meta  <- getLine
  let [count, width] = (read :: String -> Int) <$> words meta
  words <- forM [1..count] $ const $ do
    getLine
  let (depth, ls) = split width words
  let cols        = toColumns words ls
  outputLogic depth cols
