{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expression where 

    import Control.Exception
    
    data Exp = Var String 
        | Plus Exp Exp
        | Times Exp Exp
        | Minus Exp Exp
        | Div Exp Exp
        | Const Float
        | Pow Exp Exp
        | Factorial Exp
        | Dummy
        | Infinite_Sum Exp
        | Infinite_Product Exp

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
            Infinite_Product _ -> 1
            Infinite_Sum _ -> 1
            _ -> error "invalid argument correct_args :: Exp -> Int"


    precedence :: Exp -> Int 
    precedence e = 
        case e of 
            Plus _ _  -> 0
            Minus _ _ -> 0
            Const _ -> 0
            Var _ -> 0
            Times _ _ -> 1
            Div _ _ -> 1
            Pow _ _ -> 2 
            Factorial _ -> 2
            Infinite_Product _ -> 3
            Infinite_Sum _ -> 3
            _ -> error "invalid arguement precedence :: Exp -> Int"

    find_corresponding :: String -> Exp
    find_corresponding s = 
        if 
    find_corresponding "*" = Times Dummy Dummy
    find_corresponding "+" = Plus Dummy Dummy
    find_corresponding "/" = Div Dummy Dummy
    find_corresponding "^" = Pow Dummy Dummy
    find_corresponding "!" = Factorial Dummy

