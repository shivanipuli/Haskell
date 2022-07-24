greet :: String -> String
greet s =
    if s == "R" then
        "Oh"
    else 
        "Hello" ++ s

{-
greet s
    | s == "R" = "Oh"
    | otherwise = "Hello" ++ s
-}

(.) :: (b->c) -> (a -> b) -> a -> c
(.) f g a = \x -> (\y ->  (\z -> x (y z)))