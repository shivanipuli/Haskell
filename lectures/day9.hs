{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}


module Day9 where

-- squareList :: Num n => [n] -> [n]

squareMaybe :: Num n => Maybe n -> Maybe n
squareMaybe Nothing = Nothing 
squareMaybe (Just n) = Just (n ^ 2)

doubleMaybe :: Num n => Maybe n -> Maybe n
doubleMaybe Nothing = Nothing
doubleMaybe (Just n) = Just (2 * n)

-- map :: (a -> b) -> [a] -> [b]

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

class Eq_ (t :: *) where
    (==) :: t -> t -> Bool

class Functor_ (t :: * -> *) where 
    fmap :: (a -> b) -> t a -> t b

instance Functor_ Maybe where 
    --fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing  = Nothing
    fmap f (Just a) = Just (f a)
-- :kind Int gives you *


instance Functor_ ((,) a) where 
    --fmap :: (b -> b') -> (,) fst b -> (,) fst b'
    --fmap :: (b -> b') -> (fst,b) -> (fst,b')
    fmap f (fst,a) = (fst, f a)