module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.String 

{-
type Parser s = String -> [(s,String)]

pairp :: Parser a -> Parser b -> Parser (a,b)
pairp ap bp s = ((a,b),u) where
    (a,t) = ap s
    (b,u) = bp t
-}


newtype Parser a = Parser { runParser :: String -> [(a,String)] }

{- primitive parsers -}

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
    [] -> []
    a:as -> [(a,as) | f a]

char :: Char -> Parser Char
char = satisfy . (==)

alpha, digit, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
space = satisfy isSpace

token :: String -> a -> Parser a
token s a = a <$ string s 

parseTrue, parseFalse :: Parser Bool
parseTrue = token "True" True
parseFalse = token "False" False

string :: String -> Parser String
string str =
    Parser $ \s ->
        [ (t,u)
        | let (t,u) = splitAt (length str) s
        , str == t
        ]

{- instance definitions for Parser -}

instance Functor Parser where
    fmap f p = Parser $ \s ->
        [ (f a,t)
        | (a,t) <- runParser p s
        ]
    
instance Applicative Parser where
    pure a = Parser $ \s -> [(a,s)]
    af <*> aa = Parser $ \s ->
        [ (f a,u) 
        | (f,t) <- runParser af s
        , (a,u) <- runParser aa t
        ]

instance Alternative Parser where
    empty = Parser $ \s -> []
    p1 <|> p2 = Parser $ (++) <$> runParser p1 <*> runParser p2
    
instance Monad Parser where
    ma >>= f = Parser $ \s ->
        [ (b,u) 
        | (a,t) <- runParser ma s
        , (b,u) <- runParser (f a) t
        ]

instance MonadPlus Parser

parseBool :: Parser Bool
parseBool = token "True" True <|> token "False" False
--parseBool = read <$> (string "True" <|> string "False")

data IntV = IntV Int deriving (Show)

parseInt = read <$> some digit
skipSpaces = const () <$> many space
parseIntV = liftA3 (\_ _ i -> IntV i) (string "IntV") skipSpaces parseInt

instance Read IntV where
    readsPrec _ = runParser parseIntV


parseWith :: Parser a -> String -> a
parseWith p s = case [a | (a,t) <- runParser p s, all isSpace t] of
    [a] -> a
    [] -> error "no parse"
    _ -> error "ambiguous parse"

data ComplexInt = ComplexInt Int Int
    deriving (Show)



parseComplexTuple :: Parser ComplexInt
parseComplexTuple = do
    char '('
    a <- parseInt
    char ','
    b <- parseInt
    char ')'
    pure (ComplexInt a b)
  

parseComplexNum :: Parser ComplexInt
parseComplexNum = Parser $ \s -> [(ComplexInt (parseWith parseInt s) 0, "")]

parseComplexInt = parseComplexTuple <|> parseComplexNum


instance Read ComplexInt where
    readsPrec _ = runParser parseComplexInt


-- parseComplexTuple = Parser $ \s0 -> 
--     [ (ComplexInt 0 0,s1)
--     | (_,s1) <- runParser (optional . char $ '(') s0 ]
--     , (a,s2) <- runParser parseInt s1
--     , (_,s3) <- runParser (char ',') s2
--     , (b,s4) <- runParser parseInt s3
--     , (_,s5) <- runParser (char ')') s4
--     ]


{-
parseComplexInt :: Parser ComplexInt
parseComplexInt = Parser $ \s -> 
    let (x,s0):xs = runParser (optional $ char '(') s in
        if x == Nothing then
            (a,s1) = runParser parseInt s0
            [(ComplexInt a 0, s1)]
        else 
            []
 -}
    {-
    Nothing -> let (x,s1) = parseInt in [(ComplexInt x 0, s1)]
    (_,s1):xs -> [ (ComplexInt a b,s4)
        | (a,s2) <- runParser parseInt s1
        , (_,s3) <- runParser (char ',') s2
        , (b,s4) <- runParser parseInt s3
        , (_,s5) <- runParser (char ')') s4
        ]
    -}

