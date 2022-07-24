{-
-- | 10.3
 There is no applicative instance for a tuple because a tuple can 
contain different data kinds which we can then use applicative instances for

-- | 10.5
The first calculation follows this process
[(+),(*)] <*> pure 2 <*> pure 3 = [(+),(*)] <*> [2] <*> [3]
= [2+3,2*3] = [5,6]. It turns the values into lists and then returns every 
possible combination of the list.
The second calculation follows this process

ZipList [(+),(*)] <*> pure 2 <*> pure 3 = ZipList [(+),(*)] <*> ZipList [2] <*> ZipList [3]
= [2+3,2*3] = [5,6]

-}