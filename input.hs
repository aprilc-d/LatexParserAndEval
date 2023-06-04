{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

import System.Environment
import System.Exit
import System.IO
import Distribution.Simple.Command (OptDescr(BoolOpt))

import Parser
import Stack
import Expression
import Tree
import StringFunctions


main :: IO ()
main =
    do {
        args <- getArgs;
        if null args then print ("please put in an argument")
        else print (
            --order_of_operations (latex_to_list (combine args))
            --check_for_front_brk (latex_to_list (combine args))
            --uniform_list (map (\a -> precedence (find_corresponding a)) (accumulate_ops (latex_to_list (combine args)))) 
            --uniform_list (map (\a -> precedence (find_corresponding a)) (accumulate_ops (latex_to_list (combine args))))
            --accumulate_ops (latex_to_list (combine args))
            add_front_brks (latex_to_list (combine args))
    )}