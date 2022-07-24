module Day8 where

import Prelude hiding (bind)

type Person = String 

parent :: Person -> Maybe Person
parent "Will III" = Just "Will Jr"
parent "Will Jr" = Just "Will"
parent "Will" = Nothing
parent _ = undefined 

wills = ["Will III", "Will Jr", "Will"]

grandparent :: Person -> Maybe Person
grandparent p = 
    parent p >>== parent
    --andThen (Just p) parent
    --andThen (Just p :: Maybe Person)(parent :: Person -> Maybe Person)
    {-
    case parent p of 
        Nothing -> Nothing
        Just pp -> parent pp 
    -}

greatgrandparent :: Person -> Maybe Person
greatgrandparent p = 
    (parent p >>== parent) >>== parent
    {-
    case grandparent p of 
        Nothing -> Nothing
        Just ppp -> parent ppp 
    -}

andThen :: {- forall a b. -} Maybe a -> (a -> Maybe b) -> Maybe b
andThen ma funct = 
    case ma of 
        Nothing -> Nothing
        Just a -> funct a

(>>==) = andThen

nParent :: Int -> Person -> Maybe Person
nParent 0 p = Just p
nParent n p = parent p >>== nParent(n-1)