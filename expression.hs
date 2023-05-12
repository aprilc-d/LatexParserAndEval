{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expression where 

    data Exp = Var String 
        | Plus Exp Exp
        | Times Exp Exp
        | Minus Exp Exp
        | Div Exp Exp
        | Const Float
        | Pow Exp Exp
        | Factorial Exp
        | Dummy

    correct_args :: Exp -> Int
    correct_args e =
        case e of 
            Times _ _ -> 2 
            Plus _ _ -> 2
            Div _ _ -> 2 
            Minus _ _ -> 2 
            Const _ -> 0
            Pow _ _ -> 2
            Factorial _ -> 1
            _ -> 0 


        