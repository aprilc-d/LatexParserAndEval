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
  import ListFunctions 

  -- parsing --

  --coverts input from command line into a list of operators and operands
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
    if not (check_for_front_brk input) then (
      -- if all have same precedence add brackets from left to right
        if uniform_list (precedence_list input)
          then
            --adding front brackets equal to the number of operators 
              add_back_brks_equal_prec (add_front_brks input)
    
        -- the precedence of operators is not equal (one is largest)
        else (
          if (occurs (precedence_list input) (find_largest (precedence_list input))) == 1
            then []

          else []
        ))

    -- there are internal brackets 
    else []

  --add brackets which would be inferred by humans
  --so that multiplication happens before addition etc
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