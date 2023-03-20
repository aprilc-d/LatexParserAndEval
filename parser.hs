import Data.String
import Data.List

main :: IO ()
main = 
  do {
  print (check_possibilities "asdas" string_operators)
  }

char_to_string :: Char -> String
char_to_string c = c:[]

append_char :: Char -> String -> String
append_char c s = s ++ (char_to_string c)

test :: String
test = "5+10\\times 6"

special_chars :: [Char]
special_chars = 
  ['/', '+', ' ', '-', '*', '\\', '{', '}', '[', ']', '(', ')', '^', '_']

operators :: [Char]
operators = ['+', '-', '*', '/']

latex_to_list :: String -> [String] -> [String]

latex_to_list s l = 
  case s of 
  [] -> l 
  (x:xs) -> 
      if x `elem` special_chars then (

        if x `elem` operators then latex_to_list xs (l ++ [(char_to_string x)] ++ [""])
        else (
          if x == ' ' then latex_to_list xs (l ++ [""])
          else latex_to_list xs (l ++ ["\\"])
        )
      )

      else (
        if length l == 0 then latex_to_list xs ([append_char x ""])
          
        else latex_to_list xs ((init l) ++ [(append_char x (last l))])
      )


data Tree a = Empty | Node (Tree a) a (Tree a)

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
list_to_tree l t holder = 
  case l of 
    [] -> t 
    (x:xs) ->