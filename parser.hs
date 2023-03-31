{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use :" #-}
{-# LANGUAGE BlockArguments #-}

import Data.String
import Data.List
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main =
  do {
    args <- getArgs
    ; print args
    ; if length args == 0 then print ("please put in an argument")
    else print (latex_to_list (combine args) [])

  }

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

test :: String
test = "5+10\\times 6"

special_chars :: [Char]
special_chars =
  ['/', '+', ' ', '-', '*', '\\', '^', '_', '{', '}', '[', ']', '(', ')']

brackets :: [Char]
brackets =
  ['{', '}', '[', ']', '(', ')']

operators :: [Char]
operators = ['+', '-', '*', '/']

latex_to_list :: String -> [String] -> [String]

latex_to_list s l =
  case s of
  [] -> l
  (x:xs) ->
    if length l /= 0 && string_equality (last l) "" then (

      if x `elem` special_chars then (

      if (x `elem` operators || x `elem` brackets) then
        latex_to_list xs ((init l) ++ [append_char x (last l)]++[""])

      else
        if x == ' ' then latex_to_list xs l
        else latex_to_list xs ((init l) ++ [(append_char '\\' (last l))])
    )
    else
      latex_to_list xs ((init l) ++ [(append_char x (last l))])
    )

    else (
    if x `elem` special_chars then (

        if (x `elem` operators || x `elem` brackets) then
          latex_to_list xs (l ++ [char_to_string x]++[""])

        else (
          if x == ' ' then latex_to_list xs (l ++ [""])
          else latex_to_list xs (l ++ ["\\"])
        )
      )

      else (
        if length l == 0
          then latex_to_list xs ([(char_to_string x)])
          else (
            if string_equality (last l) "" then 
              latex_to_list xs (init l ++ [(char_to_string x)])
              else 
                latex_to_list xs (init l ++ [(append_char x (last l))])
          )
    )
    )

data Tree a = Empty | Node (Tree a) a (Tree a)


data Exp = Var
  | Plus Exp Exp
  | Times Exp Exp
  | Minus Exp Exp
  | Div Exp Exp
  | Const Float
  | Pow Exp Exp
  | Factorial Exp

check_possibilities :: String -> [String] -> Bool
check_possibilities s l =
  case l of
    [] -> False
    (x:xs) ->
      if string_equality s x then True
      else check_possibilities s xs

string_operators :: [String]
string_operators = ["\\frac", "+", "-", "/", "*", "\\times"]

list_to_tree :: [String] -> Tree String -> Tree String
list_to_tree l t = t