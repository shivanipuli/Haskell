module Day5 where

    data D = A Int | B String | C Bool
    -- deriving (Show)

    {-
    class Show a where 
        show :: a -> String
    -}

    {- 
    x == y is the same as (==) x y
    -}
    instance Show D where 
    --    show :: D -> String 
        show (A i) = "A " ++ show i
        show (B s) = "B" ++ show s
        show (C b) = "C " ++ show b
    instance Eq D where 
    --    (==) :: a -> a -> Bool
        (==) (A i) (A j) = i == j
        (==) (B i) (B j) = i == j
        (==) (C i) (C j) = i == j
        (==) x y         = False
        
    --  (/=) :: a -> a -> Bool


