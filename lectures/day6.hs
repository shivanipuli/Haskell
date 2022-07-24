module Day6 where
    import Prelude hiding (foldr)
    foldr :: (a -> a -> a) -> a -> [a] -> a
    foldr f base [] = base
    foldr f base (x:xs) = f x (foldr f base xs)

    prod :: Num a => [a] -> a
    prod xs = foldr (*) 1 xs

    lengt :: Num a => [a] -> a
    lengt xs = foldr(\_ res -> 1+res) 0 xs