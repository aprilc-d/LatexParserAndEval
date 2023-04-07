{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use :" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use list literal" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Data.String
import Data.List
import System.Environment
import System.Exit
import System.IO
import Distribution.Simple.Command (OptDescr(BoolOpt))

main :: IO ()
main =
  do {
    args <- getArgs
    ; if length args == 0 then print ("please put in an argument")
    else print (brackets_valid (latex_to_list (combine args) [] ) EmptyStack)
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

data Exp = Var String
  | Plus Exp Exp
  | Times Exp Exp
  | Minus Exp Exp
  | Div Exp Exp
  | Const Float
  | Pow Exp Exp
  | Factorial Exp
  | Dummy

check_possibilities :: String -> [String] -> Bool
check_possibilities s l =
  case l of
    [] -> False
    (x:xs) ->
      if string_equality s x then True
      else check_possibilities s xs

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

data Stack = S String Stack | EmptyStack

push :: String -> Stack -> Stack 
push string stack = 
  case stack of 
    EmptyStack -> S string EmptyStack
    S _ _ -> S string stack 

pre_pop :: Stack -> Maybe (String)
pre_pop s =
  case s of 
    EmptyStack -> Nothing
    S head mini -> Just (head)

pop :: Stack -> Stack
pop s =
  case s of 
    EmptyStack -> EmptyStack
    S head mini -> mini

special_chars :: [Char]
special_chars =
  ['/', '+', ' ', '-', '*', '\\', '^', '_', '{', '}', '[', ']', '(', ')', '!']

brackets :: [Char]
brackets =
  ['{', '}', '[', ']', '(', ')']

brackets_string :: [String]
brackets_string =
  ["{", "}", "[", "]", "(", ")"]

back_brackets =
  ["}", "]", ")"]

same_set :: String -> String -> Bool
same_set s1 s2 = 
 if ((string_equality s1 "{") && (string_equality s2 "}")) ||  
  ((string_equality s1 "(") && (string_equality s2 ")")) ||
  ((string_equality s1 "[") && (string_equality s2 "]")) 
  then True 
  else False

brackets_valid :: [String] -> Stack -> Bool
brackets_valid strings stack = 
  case strings of 
    [] -> 
      case stack of 
        EmptyStack -> True
        S _  _-> False
    x:xs -> 
      if check_possibilities x brackets_string then 
        case pre_pop stack of 
          Nothing -> if check_possibilities x back_brackets then False else brackets_valid xs (push x stack)
          Just (head) -> 
            if same_set head x then
              brackets_valid (xs) (pop stack)  
            else (
              if not (check_possibilities x back_brackets) then brackets_valid (xs) (push x stack)
              else False
            )
        else 
          brackets_valid xs stack

find_corresponding :: String -> Exp
find_corresponding "*" = Times Dummy Dummy
find_corresponding "+" = Plus Dummy Dummy
find_corresponding "/" = Div Dummy Dummy
find_corresponding "^" = Pow Dummy Dummy
find_corresponding "!" = Factorial Dummy

string_operators :: [String]
string_operators = ["\\frac", "+", "-", "/", "*", "\\times", "^", "!"]

convert_to_Exp :: [String] -> Exp -> [Exp] -> [Exp] -> [Exp]
convert_to_Exp s op_cache input_cache sc = 
  case s of 
    [] -> sc 
    x:xs -> 
      if (check_possibilities x string_operators)
        then (
          if correct_args (find_corresponding x) == length input_cache then 
              sc else sc )
        else sc