
module Day4 where 
    -- | Exercise 4.1 -> redefine even using components

    new_even :: Integral n => n -> Bool
    new_even = (0 ==) . (\x -> mod x 2)
    --       = (0 ==) . (flip mod 2)
    -- Or using flip
