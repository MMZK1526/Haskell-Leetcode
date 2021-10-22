import           Control.Monad
import           Data.Maybe
import           Text.Read

-- Integral Pyramid (competition problem)

-- Pascal’s triangle is a marvel of the combinatorical world, and what’s more you can easily build
-- one for yourself at home.
-- The lowest row has n numbers. The next row is staggered and has n  1 numbers, where the ith
-- is the sum of the ith and the i + 1th on the previous row.
-- You can choose any positive integers for the lowest row, but the single cell on the top row needs
-- to be equal to a given x. Is this possible?

-- Input: The only line contains the number of rows, n (1  n  20), and the value needed at the
-- top, x

-- Output: If a pyramid can be constructed, output all of the numbers on each row, starting from the top.
-- Every number must be greater than or equal to 1.
-- Otherwise, output impossible.

-- Given a row of numbers (list), generate the row above
-- Pre: the list is non-empty
genRow :: [Integer] -> [Integer]
genRow [x]
  = []
genRow (x : x' : xs)
  = (x + x') : genRow (x' : xs)
genRow _ 
  = undefined

-- Given the top number and the number of rows, generate the bottom numbers
-- set all but the first to 1; the first maybe < 1; will test validity later
genBtmRow :: Integer -> Int -> [Integer]
genBtmRow x n
  = (x - 2 ^ (n - 1) + 1) : replicate (n - 1) 1

-- Given the top number and the number of rows, generate the full pyramid
-- Pre: x, n > 0; these are tested during parsing, not in this function
genPyramid :: Integer -> Int -> Maybe [[Integer]]
genPyramid x n
  | head btm > 0 = Just $ gen [btm]
  | otherwise    = Nothing
  where
    btm = genBtmRow x n
    gen pyramid@([x'] : ps)
      | x == x'   = pyramid
      | otherwise = error "The top number is incorrect! Bad algorithm!"
    gen pyramid@(p : ps)
      = gen (genRow p : pyramid)
    gen _
      = undefined

parseIntTuple :: IO (Int, Integer)
parseIntTuple = do
  raw <- getLine
  let tuple = parse (span (/= ' ') raw)
  maybe parseIntTuple return tuple
  where
    -- Seems like here is the limit of the Haskell Compiler
    -- If I don't explicitly include the signature for filt,
    -- It cannot deduce it and will give me an error
    -- because it think filt :: MonadPlus m => m Int -> m Int
    filt :: (MonadPlus m, Ord a, Num a) => m a -> m a
    filt = mfilter (> 0)
    parse (s1, s2)
      = liftM2 (,) (filt (readMaybe s1)) (filt (readMaybe s2))

-- Takes input in the same format as the original competition problem
-- If input is invalid, will wait for another input
main :: IO ()
main = do
  print "Gimme n and x, separated by a space!"
  (n, x) <- parseIntTuple
  let result = genPyramid x n
  if isNothing result
    then print "impossible!"
    else forM_ (fromJust result) print
