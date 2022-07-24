module Notes9 where 

data BinaryTree a
        = EmptyTree
        | Node a (BinaryTree a) (BinaryTree a)
        deriving (Show)

instance Functor BinaryTree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a x y) = Node (f a) (fmap f x) (fmap f y)

{- Exercise 9.4
instance Functor (f -> g -> a) where
fmap f g a = (f . g) a

The type for fmap would be function f function g mapped onto a. 
-}
