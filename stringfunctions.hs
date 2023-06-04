{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
module StringFunctions where

    -- imports --

    import Expression
    import Stack

    -- constants for parsering are string functions --

    special_chars :: [Char]
    special_chars = ['/', '+', ' ', '-', '*', '\\', '^', '_', '{', '}', '[', ']', '(', ')', '!']

    brackets :: [Char]
    brackets = ['{', '}', '[', ']', '(', ')']

    brackets_string :: [String]
    brackets_string = ["{", "}", "[", "]", "(", ")"]

    front_brackets :: [String]
    front_brackets = ["{", "[", "("]

    back_brackets :: [String]
    back_brackets = ["}", "]", ")"]

    operators :: [Char]
    operators = ['+', '-', '*', '/', '^', '!']

    string_operators :: [String]
    string_operators = ["\\frac", "+", "-", "/", "*", "\\times", "^", "!"]

    -- basic string operations --

    char_to_string :: Char -> String
    char_to_string c = c:[]

    append_char :: Char -> String -> String
    append_char c s = s ++ (char_to_string c)

    combine :: [String] -> String
    combine l =
        case l of
        [] -> ""
        x:xs -> x ++ (combine xs)

    string_equality :: String -> String -> Bool
    string_equality s1 s2 =
        if length s1 /= length s2 then False
        else (
        case s1 of
            [] -> (
                case s2 of
                [] -> True
                (x:xs) -> False
                )
            (x:xs) -> (
                case s2 of
                    [] -> True
                    (y:ys) ->
                        if y /= x then False
                        else string_equality xs ys
                )
        )

    check_possibilities :: String -> [String] -> Bool
    check_possibilities s l =
        case l of
            [] -> False
            (x:xs) ->
                if string_equality s x then True
                else check_possibilities s xs

    -- brackets functions --

    --checks if a front bracket pairs with a given back bracket
    same_set :: String -> String -> Bool
    same_set s1 s2 =
        if ((string_equality s1 "{") && (string_equality s2 "}")) ||
        ((string_equality s1 "(") && (string_equality s2 ")")) ||
        ((string_equality s1 "[") && (string_equality s2 "]"))
        then True

        else False

    --returns the corresponding back bracket given a front bracket
    reverse_bracket :: String -> String
    reverse_bracket s =
        if (string_equality s "{") then "}"
        else
            if (string_equality s "(") then ")"
            else
                if (string_equality s "[") then "]"
                --with any other input, throws an exception
                else error "invalid arguement"
   
    internal_brackets_skipper :: [String] -> [String]
    internal_brackets_skipper [] = []
    internal_brackets_skipper l = go l EmptyStack False 
        where 
            go :: [String] -> Stack String -> Bool -> [String] 
            go [] (S head mini) b = error "invalid brackets"
            go l EmptyStack True = l 
            go [] EmptyStack b = []
            go (x:xs) EmptyStack False = 
                if check_possibilities x front_brackets then 
                    go xs (S x EmptyStack) True
                else (
                    if check_possibilities x back_brackets then error "invalid brackets"
                    else go xs EmptyStack False
                )
            go (x:xs) (S head mini) True = 
                if check_possibilities x front_brackets then go xs (S x (S head mini)) True
                else (
                    if check_possibilities x back_brackets && same_set head x then go xs mini True
                    else (
                        if check_possibilities x back_brackets && not (same_set head x) then error "invalid brackets"
                        else 
                            go xs (S head mini) True
                    )
                )
            go _ _ _ = error "invalid arguments"

    --checks if a set of brackets contains a front bracket
    check_for_front_brk :: [String] -> Bool
    check_for_front_brk [] = False
    check_for_front_brk (x:xs) =
        if check_possibilities x front_brackets then True
        else (
            if check_possibilities x back_brackets then False 
            else check_for_front_brk xs
        )

    --accumulates all operators in a list and returns the operators
    accumulate_ops :: [String] -> [String]
    accumulate_ops [] = []
    accumulate_ops (x:xs) =
        if check_possibilities x string_operators then x:(accumulate_ops xs)
        else (
            if check_possibilities x front_brackets then accumulate_ops (internal_brackets_skipper (x:xs))
            else accumulate_ops xs
        )

    precedence_list :: [String] -> [Int]
    precedence_list l = 
        (map (\a -> precedence (find_corresponding a)) (accumulate_ops l)) 

    add_front_brks :: [String] -> [String]
    add_front_brks l = go l (length (accumulate_ops l))
        where
            go :: [String] -> Int -> [String]
            go l 0 = l
            go l n = go ("(":l) (n-1)

    add_back_brks_equal_prec :: [String] -> [String]
    add_back_brks_equal_prec input = go input (accumulate_ops input)
        where 
            go :: [String] -> [String] -> [String]
            go [] [] = [] 
            go [] _ = error "invalid argument"
            go input [] = input 
            go (x:xs) (op:ops) = 
                if (not (check_possibilities x string_operators)) 
                    then x:(go xs (op:ops))

                else (
                if string_equality x "+" then 
                    case xs of 
                        y:ys -> x:y:(")"):(go ys ops)
                        [] -> error "invalid argument"

                else (
                    if string_equality x "-" then 
                        case xs of 
                            y:ys -> x:y:(")"):(go ys ops)
                            [] -> error "invalid argument"

                    else (
                        if string_equality x "/" then 
                            case xs of 
                                y:ys -> x:y:(")"):(go ys ops)
                                [] -> error "invalid argument"

                        else (
                            if string_equality x "*" then 
                                case xs of 
                                    y:ys -> x:y:(")"):(go ys ops)
                                    [] -> error "invalid argument"

                            else (
                                if string_equality x "!" then 
                                    x:(")"):(go xs ops)

                                else (
                                    if string_equality x "^" then 
                                        case xs of 
                                            y:ys -> x:y:(")"):(go ys ops)
                                            [] -> error "invalid argument"

                                    else (
                                        if string_equality x "\\frac" then []
                                        else []

                )))))))

    find_corresponding :: String -> Exp 
    find_corresponding s = 
        if not (check_possibilities s string_operators) then error "invalid argument"
        else (
            if string_equality s "+" then Plus Dummy Dummy
            else (
                if string_equality s "-" then Minus Dummy Dummy
                else (
                    if string_equality s "/" then  Div Dummy Dummy
                    else (
                        if string_equality s "*" then Times Dummy Dummy
                        else (
                            if string_equality s "!" then Factorial Dummy
                            else (
                                if string_equality s "^" then Pow Dummy Dummy 
                                else (
                                    if string_equality s "\\frac" then Div Dummy Dummy 
                                    else Times Dummy Dummy
        )))))))