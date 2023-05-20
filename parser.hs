{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use :" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use list literal" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Parser where 

  -- imports --

  import Data.String
  import Data.List

  import Stack
  import Expression
  import Tree
  import StringFunctions

  -- constants for parsing --

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

  -- helper functions --

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
                  else
                    Times Dummy Dummy
    )))))))
 

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

  --checks if a set of brackets contains a front bracket
  check_for_front_brk :: [String] -> Bool
  check_for_front_brk [] = False
  check_for_front_brk (x:xs) =
    if check_possibilities x front_brackets then True
    else check_for_front_brk xs

  --accumulates all operators in a list and returns the operators
  accumulate_ops :: [String] -> [String]
  accumulate_ops [] = []
  accumulate_ops (x:xs) =
    if check_possibilities x string_operators then x:(accumulate_ops xs)
    else accumulate_ops xs

  --helper function
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


  add_front_brks_helper :: [String] -> Int -> [String]
  add_front_brks_helper l 0 = l
  add_front_brks_helper (l) n = add_front_brks_helper ("(":l) (n-1)

  add_front_brks :: [String] -> [String]
  add_front_brks l = add_front_brks_helper l (length l)

  add_back_brks_equal_prec_helper :: [String] -> [String] -> [String]
  add_back_brks_equal_prec_helper [] [] = []
  add_back_brks_equal_prec_helper [] _ = error "invalid arguments"
  add_back_brks_equal_prec_helper input [] = input 
  add_back_brks_equal_prec_helper (x:xs) (op:ops) = 
    if not (check_possibilities x string_operators) then error "invalid argument"
    else (
      if string_equality x "+" then []
      else (
        if string_equality x "-" then []
        else (
          if string_equality x "/" then  []
          else (
            if string_equality x "*" then []
            else (
              if string_equality x "!" then []
              else (
                if string_equality x "^" then []
                else (
                  if string_equality x "\\frac" then []
                  else
                    []
    )))))))
 

  add_back_brks_equal_prec :: [String] -> [String]
  add_back_brks_equal_prec input = add_back_brks_equal_prec_helper input (accumulate_ops input)


  -- parsing techniques --

  latex_to_list_helper :: String -> [String] -> [String]
  latex_to_list_helper s l =
    case s of
      [] -> l
      (x:xs) ->
        if length l /= 0 && string_equality (last l) "" then (
          if x `elem` special_chars then (
            if (x `elem` operators || x `elem` brackets) then
        latex_to_list_helper xs ((init l) ++ [append_char x (last l)]++[""])

        else
          if x == ' ' then latex_to_list_helper xs l
          else latex_to_list_helper xs ((init l) ++ [(append_char '\\' (last l))])
          )
      else
        latex_to_list_helper xs ((init l) ++ [(append_char x (last l))])
        )

      else (
        if x `elem` special_chars then (

          if (x `elem` operators || x `elem` brackets) then
            latex_to_list_helper xs (l ++ [char_to_string x]++[""])

          else (
            if x == ' ' then latex_to_list_helper xs (l ++ [""])
            else latex_to_list_helper xs (l ++ ["\\"])
          )
        )

        else (
          if length l == 0
            then latex_to_list_helper xs ([(char_to_string x)])
          else (
            if string_equality (last l) "" then
              latex_to_list_helper xs (init l ++ [(char_to_string x)])
            else
              latex_to_list_helper xs (init l ++ [(append_char x (last l))])
          )
        )
        )

  latex_to_list :: String -> [String]
  latex_to_list s = latex_to_list_helper s []

  -- uses a stack to ensure that the order of brackets is valid, otherwise the expression cannot be evaluated
  brackets_valid :: [String] -> Stack String -> Bool
  brackets_valid strings stack =
    case strings of
      --empty list
      [] ->
        case stack of
          --if stack empty, valid
          EmptyStack -> True

          --if stack is not empty, invalid
          S _ _-> False

      --non empty list
      x:xs ->
        --if x is a bracket
        if check_possibilities x brackets_string then
          --check top of stack
          case pre_pop stack of
            --empty stack
            Nothing -> 
              --if x is a back bracket, invalid
              if check_possibilities x back_brackets then False 

              --otherwise push onto stack and move on
              else brackets_valid xs (push x stack)

            Just (head) -> 
                  -- if in the same set, valid arrangement
                  if same_set head x then
                    brackets_valid (xs) (pop stack)

                  else (
                    --if x is not a front bracket then invalid
                    if not (check_possibilities x back_brackets) then brackets_valid (xs) (push x stack)
                    else False
                  )

        --if not a bracket, continue with sublist          
        else brackets_valid xs stack

  order_of_operations_helper :: [String] -> String -> [String] -> [String]
  order_of_operations_helper [] current_brk ops = []
  order_of_operations_helper input current_brk ops =
    -- no internal brackets 
    if not (check_for_front_brk input) then
      -- if all have same precedence add brackets from left to right
        if
          uniform_list 
            (map (\a -> precedence (find_corresponding a)) (accumulate_ops input)) 

          then
            --adding front brackets equal to the number of operators 
              add_front_brks input 
        
        else []


    else []

  order_of_operations :: [String] -> [String]
  order_of_operations input = order_of_operations_helper input "" (accumulate_ops input)

  convert_to_Exp :: [String] -> Exp -> [Exp] -> [Exp] -> [Exp]
  convert_to_Exp s op_cache input_cache sc =
    case s of
      [] -> sc
      x:xs ->
        if (check_possibilities x string_operators)
          then (
            if correct_args (find_corresponding x) == length input_cache 
              then sc 
            else sc )
        else sc