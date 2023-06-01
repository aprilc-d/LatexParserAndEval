{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}

module Evaluator where 

    import Expression 
    import Data.List

    factorial :: Float -> Float
    factorial 0.0 = 1.0 
    factorial 1.0 = 1.0
    factorial n = n * factorial (n-1)

    eval :: Exp -> Float 
    eval e = 
        case e of 
            Plus e1 e2 -> (eval e1) + (eval e2)
            Times e1 e2 -> (eval e1) * (eval e2)
            Minus e1 e2 ->  (eval e1) - (eval e2)
            Div e1 e2 ->  (eval e1) / (eval e2)
            Const n -> n 
            Pow e1 e2 ->  (eval e1)**(eval e2)
            Factorial e1 -> factorial (eval e1)
            Dummy -> error "invalid arguments"
            Infinite_Sum e1 e2 e3 -> 0
            Infinite_Product e1 e2 e3 -> 0
