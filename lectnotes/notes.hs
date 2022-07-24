{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Exercise 2.1
--   sumf (sumf square) [[1,2],[3,4]]
--   = sumf square [1,2] + sumf (sumf square) [3,4]
--   = square 1 + sumf square 2:[] + sumf square 3 + sumf (sumf square) 4:[]
--   = 1 + square 2 + 0 + square 3 + sumf square 4 + 0
--   = 1 + 4 + 9 + 16 = 30

-- | Exercise 2.2

import Prelude hiding (product)

-- | Computes the product of numbers in a list

product :: Num n => [n] -> n
product [] = 1
product (x:xs) = x * product xs

-- product [1..10] = 3628800

-- | Exercise 2.3 / 2.4

-- | Returns true if num evenly divides by other

divisibleBy :: Integral n => n -> n -> Bool
divisibleBy n d = rem d n == 0


-- | Checks if list of conditions are true


allp :: [t -> Bool] -> t -> Bool
allp [] g = True
allp (p:ps) g = p g && allp ps g


-- | Combines filter and allP

filterAll :: (a -> Bool) -> [a] -> [a]
filterAll p [x] = filter p [x]
