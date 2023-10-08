{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

-- Given a string, find the lexically smallest string that can be formed by
-- removing letters from the original string such that each distinct letters
-- appears exactly once in the result.
--
-- The string is consisted only of uppercase letters.

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Array.ST
import Data.Char
import Data.Word

solution :: String -> String
solution str = runST do
  smallIndexArrST <- newArray (0, 25) maxBound :: ST s (STUArray s Int Word32)
  largeIndexArrST <- newArray (0, 25) maxBound :: ST s (STUArray s Int Word32)
  forM_ (zip [0..] str) \(i, ch) -> do
    let chIndex = ord ch - ord 'A'
    smallIndex <- readArray smallIndexArrST chIndex
    when (smallIndex == maxBound) $ writeArray smallIndexArrST chIndex i
    writeArray largeIndexArrST chIndex i
  let go = do
        cutoff <- minimum <$> getElems largeIndexArrST
        mN     <- runMaybeT . msum $ flip map [0..25] \n -> do
          smallIndex <- lift $ readArray smallIndexArrST n
          guard (smallIndex <= cutoff)
          pure n
        if
          | cutoff /= maxBound, Just n <- mN -> do
              writeArray smallIndexArrST n maxBound
              writeArray largeIndexArrST n maxBound
              (chr (n + ord 'A') :) <$> go
          | otherwise                        -> pure ""
  go
