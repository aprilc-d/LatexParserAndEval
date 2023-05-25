{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use guards" #-}
module StringFunctions where

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