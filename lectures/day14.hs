module Day14 where

n = foldr (\n acc -> acc ++ [n]) [1..3]
-- n = [3,2,1]

{-
Semigroup (S, op : S x S -> S )
monoid (S, op, id E S )
V x , y , z E S 

S.t. (S, op) is a semigroup
V x E S. id op x = x = x op id 

-- set s , binary associative operation

-} 

class Semigroup_ a where 
-- forall x, y, z :: s, (x <> y) <> z == x <> (y <> z)
    (<>) :: s -> s -> s


class Semigroup_ m => Monoid_ m where
    -- forall x :: s, id `op` x == x == x `op` id
    -- "id" 
    mempty :: m -- takes no arguments

{- 
newtype SemigroupEmptyList a = SemigroupEmptyList [a]


instance Semigroup_ (SemigroupEmptyList a) where
    -- (<>) :: [a] -> [a] -> [a]
    SemigroupEmptyList list1 <> SemigroupEmptyList list2 = []

-}

instance Semigroup_ [a] where
    -- (<>) :: [a] -> [a] -> [a]
    (<>) list1 list2 = list1 ++ list2 
    -- (<>) = (++)

instance Monoid_ [a] where
 -- mempty :: [a]
    mempty = []

{- 
ground type 
*                    * -> *         * -> * -> *
Eq (Ord)            F (A (M))
(Show (read))       []
(Semi (Monoid))
-}