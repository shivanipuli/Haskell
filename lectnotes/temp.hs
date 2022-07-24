module Temp where

findPrime :: [Int] -> [Int]
findPrime num = [x | x <- num, isPrime x]

isPrime :: Int -> Bool
isPrime x = (all (\y -> 0 /= mod x y) [2..(x-1)])