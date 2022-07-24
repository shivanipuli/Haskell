module Lecture18 where



newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Monad Parser where 
    return a = Parser $ \s -> [(a,s)]
    pa >>= f = Parser $ \s0 ->
        [ (b,s2)
        | (a,s1) <- runParser pa s0
        , (b, s2) <- runParser (f a) s1
        ]

instance Functor Parser where {fmap f x = pure f <*> x}
instance Applicative Parser where {pure = return; (<*>) = op}

char :: Char -> Parser Char
char c = Parser $ \s ->
    case s of 
        [] -> []
        a:as -> if c == a then 
                [ (a,as) ]
            else
                []

string str = Parser $ \s ->
    let (pre,suf) = splitAt (length str) s in
        if str == pre then
            [(str,suf)]
        else 
            []

hello :: Parser String
hello = do
    char 'H'
    char 'e'
    char 'l'
    char 'l'
    char 'o'
    pure "Hello"

{-
hello = Parser $ \s0 -> 
    [ ([c1, c2, c3, c4, c5], s5)
    | (c1,s1) <- runParser (char 'H') s0
    , (c2,s2) <- runParser (char 'e') s1
    , (c3,s3) <- runParser (char 'l') s2
    , (c4,s4) <- runParser (char 'l') s3
    , (c5,s5) <- runParser (char 'o') s4
    ]
-}

parseTrue :: Parser Bool
parseTrue = const True <$> string "True" 

parseBool = parseTrue ?? parseFalse