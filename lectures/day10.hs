module Day10 where

import Control.Applicative


class Functor t => Applicative_ t where
    apply :: t (a -> b) -> t a -> t b
    pure :: a -> t a

instance Applicative_ Maybe where
--    apply :: Maybe (a -> b) -> Maybe a -> Maybe b
--    apply (Just f) (Just a) = Just $ f a
--    apply _ _ = Nothing
--    apply 
    apply (Just f) ma = f <$> ma
    apply Nothing ma = Nothing


    pure a = Just a
-- or you can do 
-- pure a = Nothing
--(<*>) = apply 

instance Applicative_ [] where
    pure a = [a]
    apply (f:fs) a = f <$> a :apply fs as
    apply f _ = []

--(++) <$> ["ap","bp"] <*> ["c","d"]
b = (++) <$> ZipList ["ap","bp"] <*> ZipList ["cp","dp"]
