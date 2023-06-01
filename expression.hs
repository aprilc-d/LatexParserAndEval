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
        | Infinite_Sum Exp Exp Exp
        | Infinite_Product Exp Exp Exp

    correct_args :: Exp -> Int
    correct_args e =
        case e of
            Times {} -> 2
            Plus {} -> 2
            Div {} -> 2
            Minus {} -> 2
            Const _ -> 0
            Pow {} -> 2
            Factorial _ -> 1
            Infinite_Product {} -> 3
            Infinite_Sum {} -> 3
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
            Infinite_Product {} -> 3
            Infinite_Sum {} -> 3
            _ -> error "invalid arguement precedence :: Exp -> Int"