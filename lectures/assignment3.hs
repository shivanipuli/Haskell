-- | Exercise 3.1

-- | Writing the collatz function using guards

collatz :: Integer -> Integer
collatz n
  | n == 1 = 1
  | even n  = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (3 * n + 1)

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

data Tree a = Node a (Forest a)
type Forest a = [Tree a]

-- | Turns values in Trees into a list

preorder :: Tree a -> [a]
preorder (Node a) (Forest b) = [a]
preorder a b = a ++ preorder b