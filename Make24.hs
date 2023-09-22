module Make24 where

import Control.Monad

data Exp = Const Int | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Div Exp Exp
  deriving (Eq, Show)

make24 :: [Int] -> Maybe Exp
make24 xs = go (map (\x -> (x, Const x)) xs)
  where
    work (n1, exp1) (n2, exp2)
      = [(n1 + n2, Add exp1 exp2), (n1 * n2, Mul exp1 exp2)]
     ++ if n1 > n2 then [(n1 - n2, Sub exp1 exp2)] else [(n1 - n2, Sub exp1 exp2)]
     ++ [(n1 `div` n2, Div exp1 exp2) | n2 /= 0, n1 `mod` n2 == 0] ++ [(n2 `div` n1, Div exp2 exp1) | n1 /= 0, n2 `mod` n1 == 0]
    go [] = Nothing
    go [(24, exp)] = Just exp
    go [_] = Nothing
    go xs = msum $ do
      (x, xs') <- pick1 xs
      (y, xs'') <- pick1 xs'
      let choices = work x y
      pure $ msum [go $ choice : xs'' | choice <- choices]

pick1 :: [a] -> [(a, [a])]
pick1 [] = []
pick1 (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- pick1 xs]
