-- A question I saw on HackerRank.

-- Given a list of positive and distinct integers, count the number of pairs
-- such that the first element is less than the second element, and the XOR of
-- the two elements is larger than the AND of the two elements.

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bits

dominatingXOR :: FiniteBits a => [a] -> Int
dominatingXOR [] = 0
dominatingXOR ns = runST $ do
  leadingZeroArrST <- newArray (0, finiteBitSize (head ns) - 1) 0 :: ST s (STUArray s Int Int)
  forM_ ns $ \n -> readArray leadingZeroArrST (countLeadingZeros n) >>= writeArray leadingZeroArrST (countLeadingZeros n) . succ
  leadingZeros <- getAssocs leadingZeroArrST
  pure $ sum [x * y | (i, x) <- leadingZeros, (_, y) <- drop (i + 1) leadingZeros]
