{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
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