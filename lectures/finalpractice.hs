module Final where 

import Data.Traversable
import Data.Foldable
import Control.Monad 
import Control.Applicative
import Data.Monoid
import Data.Semigroup

{-
newtype First a = First { getFirst :: Maybe a } deriving (Show)
newtype Last a  = Last  { getLast  :: Maybe a } deriving (Show)

instance Semigroup (First a) where
    First Nothing <> b = b 
    a <> _ = a

instance Monoid (First a) where
    mempty = First Nothing

instance Semigroup (Last a) where
    a <> Last Nothing = a
    _ <> b = b

instance Monoid (Last a) where
    mempty = Last Nothing

instance Monoid w => Monad (Writer w) where
    return a = Writer (a, mempty)
    Writer (a, w1) >>= f =
      let Writer (b, w2) = f a in
      Writer (b, w1 <> w2)
-}

data MultiTree a
    = Node [MultiTree a]
    | Leaf a

instance Eq a => Eq (MultiTree a) where
    Leaf a == Leaf b = a == b 
    Node a == Node b = a == b
    _ == _ = False

instance Ord a => (MultiTree a) where 
    (<=) (Leaf a) (Leaf b) = a <= b
    (<=) (Node a) (Node b) = a <= b
    (<=) (Leaf a) (Node b) = b
    (<=) (Node a) (Leaf b) = a

instance Functor MultiTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a) = Node (map (fmap f) a)

instance Applicative MultiTree where
    pure = Leaf 
    -- (<*>) :: MultiTree (a -> b) -> MultiTree a -> MultiTree b
    Leaf f <*> a = fmap f a
    Node f:fs <*> a = (Node fs) <*> (Node $ fmap f a) 

instance Monad MultiTree where
    -- (>>=) :: MultiTree a -> (a -> MultiTree b) -> MultiTree b
    Leaf a >>= f = f a
    Node a >>= f = Node $ fmap ( >>= f) a

instance Semigroup MultiTree where
    (<>) (Node a) (Node b) = Node $ a ++ b
    (<>) (Node a) (Leaf b) = Node $ a: Leaf b
    (<>) (Leaf a) (Node b) = Node $ Leaf a:b
    (<>) (Leaf a) (Leaf b) = Node $ [Leaf a, Leaf b]

instance Monoid a => Monoid (MultiTree a) where
    mempty = Node []
    mappend = (<>)

instance Alternative MultiTree where
    empty = mempty
    (<|>) = (<>)

instance MonadPlus MultiTree where

instance Foldable MultiTree where
    -- -- foldMap :: Monoid b => (a -> b) -> [a] -> b
    foldMap _ (Node []) = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node a:as) = foldMap f a <> foldMap f as

instance Traversable MultiTree where
    traverse _ (Node []) = mempty
    traverse f (Leaf a) = Leaf (f a)
    traverse f (Node a:as) = Node (traverse (traverse f) a)