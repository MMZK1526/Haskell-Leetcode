import Control.Monad
import Control.Monad.Trans.State
import Data.Map as M
import Data.Maybe
import qualified Text.Read as R

-- source: https://leetcode.com/problems/generate-parentheses/

-- Given n pairs of parentheses, write a function to generate all 
-- combinations of well-formed parentheses.

-- Outputs all well-formed n pairs of brackets.
brace :: Int -> [String]
brace n = evalState (braceS n) M.empty
  where
    braceS n
      | n < 0 = return []
    braceS n = do
      -- init
      forM_ [0..n] $ \i -> modify $ M.insert (i, 0) [replicate i ')']
      -- record
      forM_ [1..n] $ \j ->
        forM_ [0..n] $ \i -> do
          dict <- get
          case i of
            0 -> put $ M.insert (0, j) (('(' :) <$> dict M.! (1, j - 1)) dict
            i -> put $ M.insert (i, j)
                       ((('(' :) <$> dict M.! (i + 1, j - 1)) ++
                        ((')' :) <$> dict M.! (i - 1, j))) 
                       dict
      -- result
      gets (M.! (0, n))

-- Outputs the count of all well-formed n pairs of brackets.
braceLength :: Integer -> Integer
braceLength n = evalState (braceLengthS n) M.empty
  where
    braceLengthS n
      | n < 0 = return 0
    braceLengthS n = do
      -- init
      forM_ [0..n] $ \i -> modify $ M.insert (i, 0) i
      -- record
      forM_ [1..n] $ \j ->
        forM_ [0..n] $ \i -> do
          dict <- get
          case i of
            0 -> put $ M.insert (0, j) (dict M.! (1, j - 1)) dict
            i -> put $ M.insert (i, j)
                       (dict M.! (i + 1, j - 1) + dict M.! (i - 1, j))
                       dict
      -- result
      gets (M.! (0, n))

main :: IO ()
main = do
  i <- R.readMaybe <$> getLine
  case (i :: Maybe Int) of
    Nothing -> putStrLn "Please enter a positive integer!"
    Just n  -> print $ brace n
