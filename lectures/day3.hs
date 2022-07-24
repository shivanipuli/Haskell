module Oct01 where 

--type Color = 
---- "aliases"

data Color
    = Grayscale Int
    | ColorName String
    | RGB Int Int Int
    | RGBtriple (Int, Int, Int)
        deriving (Show)