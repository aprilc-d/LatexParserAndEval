{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Stack where 

    data Stack a = S a (Stack a) | EmptyStack

    push :: a -> (Stack a) -> (Stack a)
    push n EmptyStack = S n EmptyStack
    push n s = S n s 

    pre_pop :: Stack a -> Maybe (a)
    pre_pop EmptyStack = Nothing
    pre_pop (S head mini) = Just head
  
    pop :: Stack a -> Stack a
    pop EmptyStack = EmptyStack
    pop (S head mini) = mini