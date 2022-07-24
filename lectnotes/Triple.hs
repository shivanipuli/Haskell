module Triple where

    -- | Returns the first value of a triple

    fstof3 :: (a, b, c) -> a
    fstof3 (a,_,_) = a

    -- | Returns the second value of a triple

    sndof3 :: (a, b, c) -> b
    sndof3 (_,b,_) = b

    -- | Returns the third value of a triple

    thirdof3 :: (a, b, c) -> c
    thirdof3 (_,_,c) = c
    
    -- | Applies a function two three seperate values

    -- uncurry :: (a, b, c) -> (a -> b -> c)
    uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
    uncurry3 f (a,b,c) = f a b c

    -- | Applies a function to a triple
    curry3 :: ((a, b, c) -> t) -> a -> b -> c -> t
    curry3 f a b c = f (a, b, c)

-- | Exercise 3.1

-- | Writing the collatz function using guards

    collatz :: Integer -> Integer
    collatz n = if n == 1 
    then 1
    else if even n  then 1 + collatz (div n 2)
    else 1 + collatz (3 * n + 1)

    -- | Writing the collatz function using if else then

    collatz' :: Integer -> Integer
    collatz' n
        | n == 1 = 1
        | even n = 1 + collatz' (div n 2)
        | otherwise = 1 + collatz' (3*n+1)

    -- | Exercise 3.3 -> Explored different data types for Integral n => Integral (Maybe n)
    -- No code needed

    -- | Exercise 3.4 Lookup

    -- | Dictionary look up that throws error if value not found

    lookupWithError :: Eq a => a -> [(a, b)] -> b
    lookupWithError _ [] = error "key not found"
    lookupWithError a ((k,v):ps)
        | a == k = v
        | otherwise = lookupWithError a ps

    -- | Dictionary look up that throws error if value not found

    lookupWithDefault :: Eq a => a -> b -> [(a,b)] -> b
    lookupWithDefault _ b [] = b
    lookupWithDefault a b ((k,v):ps)
        | a == k = b
        | otherwise = lookupWithDefault a b ps

    -- | A dictionary with default value

    lookupInDictionary :: Eq a => a -> [(a,b)] -> b
    lookupInDictionary a ((k,v):ps)
        | a == k    =  v
        | otherwise = lookupInDictionary a ps

    -- | Exercise 3.5 


    -- | A rose tree.    
    data Tree a = Empty | Node a (Forest a)
    type Forest a = [Tree a]

    -- | Turns values in Trees into a list , couldn't figure out how to define 

    --preorder :: Tree a -> [a]
    --preorder Empty = []
    --preorder (Node n t) = concat([n, preorder t])

        


