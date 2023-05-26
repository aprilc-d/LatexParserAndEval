{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant if" #-}
module ListFunctions where
    import Data.List 

    uniform_list_helper :: [Int] -> Int -> Bool
    --if list is empty it is uniform
    uniform_list_helper [] _ = True
    --if head of list is n then, call on sublist, if not false
    uniform_list_helper (x:xs) n = 
        if x == n then uniform_list_helper xs x
        else False

    --returns true if a list contains all identical values
    uniform_list :: [Int] -> Bool
    uniform_list [] = False
    uniform_list l = uniform_list_helper l (head l)

    find_largest_helper :: [Int] -> Int -> Int
    find_largest_helper [] n = n 
    find_largest_helper (x:xs) n = 
        if x > n then find_largest_helper xs x
        else find_largest_helper xs n 

    find_largest :: [Int] -> Int
    find_largest [] = 0
    find_largest l = find_largest_helper l 0

    occurs :: [Int] -> Int -> Int 
    occurs [] n = 0
    occurs (x:xs) n = 
        if x == n then 1 + (occurs xs n) 
        else occurs xs n 
