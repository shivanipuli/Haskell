module ParsingD where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.String 
import Text.ParserCombinators.ReadP
import Prelude hiding (sequence_)

data D = A | B 
    deriving (Show)

instance Read D where
    readsPrec _ = readP_to_S parseD

parseD :: ReadP D
parseD = (const A <$> char 'A') <|> (const B <$> char 'B')


data List = Nil | Cons Int List

instance Show List where
    show Nil = "[]"
    show (Cons i l) = "Cons " ++ show i ++ ":" ++ show l

parseList :: ReadP List
parseList = 
    parseNil <|> parseCons

parseNil = (\_ -> Nil) <$> string "[]"
    {-
    do
    string "[]"
    pure Nil
    -}

parseCons = liftA3 (\i c list -> Cons i list) parseInt (char ':') parseList
    {-
    do
    i <- parseInt 
    char ':'
    parseList
    -}

parseInt :: ReadP Int
parseInt = read <$> munch isDigit
    {-
    do
    s <- munch isDigit
    pure $ read s
    -}

instance Read List where
    readsPrec _ = readP_to_S parseList

(<*) fa fb = pure (\a _ -> a) <*> fa <*> fb

foo :: [Maybe a] -> Maybe [a]
foo [] = Just []
foo (ma:mas) = 
    case (ma, foo mas) of 
        (Just a, Just as) -> Just (a:as)
        _ -> Nothing


class Traversable_ t_outer where
    sequence_ :: t_outer (t_inner a) -> t_inner(t_outer a)

instance Traversable_ Maybe where
    sequence_ :: Monad t => Maybe (t a) -> t (Maybe a)
    sequence_ Nothing = pure Nothing 
    sequence_ (Just ta) = Just <$> ta
        {-
        do
            a <- ta
            pure $ Just a
        -}

instance Traversable_ List where
    sequence_ :: Applicative t => List (t a) -> t (List a)
    sequence_ [] = pure []
    sequence_ (t:ts) = pure (\a as -> a:as) <*> ta <*> sequence_ ts
        -- do 
        --     a <- t
        --     as <- sequence_ ts
        --     pure $ a:as