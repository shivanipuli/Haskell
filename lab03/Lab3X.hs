module Lab3 where


import Debug.Trace
import Data.Char
import Data.List
import Data.Ratio

data ArithExp = 
    Plus ArithExp ArithExp
    |Mult ArithExp ArithExp
    |Div ArithExp ArithExp
    |Number Int
    deriving (Eq, Show)

data Fraction = Fract Int Int
    | Numb Int


eval :: ArithExp -> Rational
eval (Number a) =  a
eval (Plus a b) = eval a + eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a %  eval b

data Token = TInt Int 
            | TPlus
            | TParen [Token] -- or TLeft | TRight
            | TMult
            | TDiv 
    deriving (Eq, Show)

findIndexParen :: Int -> String -> Int
findIndexParen count (x:xs) 
    | count == -1 = 0
    | x == '('   = 1 + findIndexParen (count+1) xs
    | x == ')'   = 1 + findIndexParen (count-1) xs
    | otherwise  = 1 + findIndexParen count xs
findIndexParen _ _ = 0

tokenize :: String -> [Token]
tokenize (x:xs)
    | isDigit x = TInt (read $ takeWhile isDigit (x:xs)) : tokenize (dropWhile isDigit xs)
    | x == '-'  = TInt (-1 * read(takeWhile isDigit xs)) : tokenize (dropWhile isDigit xs)
    | x == '*'  = TMult : tokenize xs
    | x == '/'  = TDiv  : tokenize xs
    | x == '+'  = TPlus : tokenize xs
    | x == '('  = TParen (tokenize(take ind xs)) : tokenize (drop ind xs)
    | otherwise = tokenize xs -- accounts for case x = ' '
    where ind = findIndexParen 0 xs
tokenize _ = []

-- | Splits at operator -> Operator parse (everything before operator) parse(everything after) 
--   ignoring operator itself

parse :: [Token] -> ArithExp
parse [TInt a] = Number a
parse [TParen a] = parse a
parse exp
    | TPlus `elem` exp = Plus (parse $ takeWhile (TPlus /=) exp) (parse $ tail $ dropWhile (TPlus /=) exp)
    | TDiv `elem` exp  = Div  (parse $ takeWhile (TDiv /=) exp)  (parse $ tail $ dropWhile (TDiv /=) exp)  
    | TMult `elem` exp = Mult (parse $ takeWhile (TMult /=) exp) (parse $ tail $ dropWhile (TMult /=) exp)
    | otherwise = Number 10
