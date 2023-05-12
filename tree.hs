{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Tree where 

    import Data.List

    data Tree a = Empty | Node (Tree a) a (Tree a)

    inorder :: Tree a -> [a] 
    inorder Empty = [] 
    inorder (Node left_subtree n right_subtree) = 
        (inorder left_subtree) ++ [n] ++ (inorder right_subtree)

    