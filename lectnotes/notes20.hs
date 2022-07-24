module Exercise20 where
import Data.Traversable
import Data.List

data BinaryTree a
  = Empty
  | Node (BinaryTree a) a (BinaryTree a)
  deriving Show

instance Functor BinaryTree where
        fmap _ Empty = Empty
        fmap f (Node left a right) =
            Node (fmap f left) (f a) (fmap f right)

instance Foldable BinaryTree where
        foldMap _ Empty = mempty
        foldMap f (Node left a right) =
            foldMap f left <> f a <> foldMap f right

instance Traversable BinaryTree where
        traverse _ Empty = pure Empty
        traverse f (Node left a right) =
             Node <$> traverse f left <*> f a <*> traverse f right

addLabels :: (Traversable t, Num b) => t a -> t (b,a)
addLabels tree = snd $ mapAccumL (\a b -> (a+1, (a,b) )) 1 tree









