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
      = [(n1 + n2, Add exp1 exp2), (n1 * n2, Mul exp1 exp2)]
     ++ if n1 > n2
      then [(n1 - n2, Sub exp1 exp2)]
      else [(n1 - n2, Sub exp1 exp2)]
     ++ [(n1 / n2, Div exp1 exp2) | n2 /= 0] 
     ++ [(n2 / n1, Div exp2 exp1) | n1 /= 0]
    go []          = Nothing
    go [(24, exp)] = Just exp
    go [_]         = Nothing
    go xs          = msum $ do
      (x, xs')  <- pick1 xs
      (y, xs'') <- pick1 xs'
      pure $ msum [go $ choice : xs'' | choice <- work x y]

pick1 :: [a] -> [(a, [a])]
pick1 []       = []
pick1 (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- pick1 xs]
