{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import Data.List ( (!!), unfoldr )
import Data.List.NonEmpty ( unfold )

main :: IO ()
main = 
  do {
  print (approx_infinite_series exponential)
  }

factorial :: Float -> Float
factorial 0.0 = 1.0 
factorial 1.0 = 1.0
factorial n = n * factorial (n-1)
  
data Exp = Var 
  | Plus Exp Exp
  | Times Exp Exp 
  | Minus Exp Exp
  | Div Exp Exp 
  | Const Float
  | Pow Exp Exp
  | Factorial Exp

eval :: Exp -> Float -> Float
eval (Var) f = f
eval (Const f) g = f
eval (Plus e1 e2) f = (eval e1 f) + (eval e2 f)
eval (Times e1 e2) f = (eval e1 f) * (eval e2 f)
eval (Minus e1 e2) f = (eval e1 f) - (eval e2 f)
eval (Div e1 e2) f = (eval e1 f) / (eval e2 f) 
eval (Pow e1 e2) f = (eval e1 f) ** (eval e2 f)
eval (Factorial e) f = factorial (eval e f)

test :: Exp 
test = Plus Var (Times Var Var)

exponential :: Exp 
exponential = Div (Pow (Const 2.0) Var) (Factorial Var)

go :: Exp -> (Float, Float) -> Maybe (Float, (Float, Float))
go e (a, b) = Just (a, ((a+ eval e (b+1)), b+1))
  
approx_infinite_series :: Exp -> Float

approx_infinite_series e = unfoldr (go') (eval e 0.0, 0.0) !! 100
  where go' (a, b) = go e (a, b)
