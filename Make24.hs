module Make24 where

-- The good old Make24 problem. Given a list of integers, return an expression
-- that evaluates to 24.

import Control.Monad
import Data.Ratio

data Exp = Int Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
  deriving Eq

instance Num Exp where
  (+) :: Exp -> Exp -> Exp
  (+) = Add

  (-) :: Exp -> Exp -> Exp
  (-) = Sub

  (*) :: Exp -> Exp -> Exp
  (*) = Mul

  fromInteger :: Integer -> Exp
  fromInteger = Int . fromInteger

  abs :: Exp -> Exp
  abs = undefined
  signum :: Exp -> Exp
  signum = undefined

instance Fractional Exp where
  (/) :: Exp -> Exp -> Exp
  (/) = Div

  fromRational :: Rational -> Exp
  fromRational = Int . fromInteger . numerator

instance Show Exp where
  showsPrec :: Int -> Exp -> ShowS
  showsPrec _ (Int n)     = shows n
  showsPrec d (Add e1 e2) = showParen (d > 6) 
                          $ showsPrec 6 e1 . showString " + " . showsPrec 6 e2
  showsPrec d (Sub e1 e2) = showParen (d > 6)
                          $ showsPrec 6 e1 . showString " - " . showsPrec 7 e2
  showsPrec d (Mul e1 e2) = showParen (d > 7)
                          $ showsPrec 7 e1 . showString " * " . showsPrec 7 e2
  showsPrec d (Div e1 e2) = showParen (d > 7)
                          $ showsPrec 7 e1 . showString " / " . showsPrec 8 e2

make24 :: [Int] -> Maybe Exp
make24 xs = go (map (\x -> (x % 1, Int x)) xs)
  where
    work (n1, exp1) (n2, exp2)
      = [(n1 + n2, exp1 + exp2), (n1 * n2, exp1 * exp2)]
     ++ (guard (n1 > n2) >> [(n1 - n2, exp1 - exp2)])
     ++ [(n1 / n2, exp1 / exp2) | n2 /= 0] 
    go [(24, exp)] = Just exp
    go [_]         = Nothing
    go xs          = msum $ do
      (x, xs')  <- pick1 xs
      (y, xs'') <- pick1 xs'
      pure $ msum [go $ choice : xs'' | choice <- work x y]

pick1 :: [a] -> [(a, [a])]
pick1 []       = []
pick1 (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- pick1 xs]

selfSymJoin :: Ord a => Int -> [a] -> [[a]]
selfSymJoin 0 xs = [[]]
selfSymJoin n xs
  = [x : y | x <- xs, y <- selfSymJoin (n - 1) [x' | x' <- xs, x' >= x]]

tabPrint :: IO ()
tabPrint = do
  forM_ (selfSymJoin 4 [1..10]) $ \nums -> do
    putStr $ show nums ++ ": "
    case make24 nums of
      Nothing  -> putStrLn "No solution"
      Just exp -> putStrLn $ show exp ++ " = 24"
