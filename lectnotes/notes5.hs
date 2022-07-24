module Notes5 where 
    import Data.Foldable
    
    data Pair a b = Pair a b 

    instance (Eq a, Eq b) => Eq (Pair a b) where
        Pair a1 b1 == Pair a2 b2 = a1 == a2 && b1 == b2

    instance (Ord a, Ord b) => Ord (Pair a b) where  
        compare (Pair a1 b1) (Pair a2 b2)
            |a1 == a2  = compare b1 b2
            |otherwise = compare a1 a2

    data BinaryTree a = EmptyTree| Node a (BinaryTree a) (BinaryTree a) -- value leftChild rightChild
        deriving (Show)
    
    instance Foldable BinaryTree where
    -- foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
        foldr combiner base EmptyTree = base
        foldr combiner base (Node a left right) = foldr combiner (combiner a (foldr combiner base right)) left

    reverseTree :: BinaryTree a -> BinaryTree a
    reverseTree EmptyTree = EmptyTree
    reverseTree (Node a left right) = Node a (reverseTree right) (reverseTree left)

    tree :: BinaryTree Integer 
    tree = Node 3 (Node 1 (Node 0 EmptyTree EmptyTree) (Node 2 EmptyTree EmptyTree)) (Node 5 (Node 4 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree))
    
    