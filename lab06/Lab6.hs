module Lab6 (
  Name,
  Number,
  TopLevelExp(..),
  MathExp(..),
  parse
) where

import           Control.Applicative          hiding (many, optional)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name   = String
type Number = Int 

data TopLevelExp
    = MathTLE MathExp
    | LetTLE [Name] [MathExp] MathExp
    deriving (Eq, Show)

data MathExp
    = Number Number
    | Var    Name
    | Neg    MathExp
    | Plus   MathExp MathExp
    | Minus  MathExp MathExp
    | Mult   MathExp MathExp
    | Div    MathExp MathExp
    | Pow    MathExp MathExp
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = read <$> munch isDigit

parseNum :: ReadP MathExp
parseNum = do
    skipSpaces
    x <- parseInt 
    pure $ Number x

parseParen :: ReadP MathExp
parseParen = do
    x <- between (char '(') (char ')') parseTerm
    return x

parseName :: ReadP Name
parseName = do
    skipSpaces
    x <- satisfy isAlpha
    varName <- munch isAlphaNum
    skipSpaces
    pure $ (x:varName)

parseVar :: ReadP MathExp
parseVar = do
    x <- parseName
    pure $ Var x

parseVals, parseVal :: ReadP MathExp
parseVals = liftA2 (\x _ -> x) (parseVar <++ parseParen <++ parseNum) skipSpaces

parseVal = do
    skipSpaces
    unparsed <- look
    if head unparsed == '-'
        then do
            _ <- char '-'
            s <- parseVals
            return $ Neg s
        else do
            x <- parseVals
            return x

token :: String -> a -> ReadP a
token s a = a <$ string s 

parseMinus, parseMult, parsePow :: ReadP (MathExp -> MathExp -> MathExp)
parseMinus = token "-" Minus <|> token "+" Plus
parseMult = token "*" Mult <|> token "/" Div
parsePow = token "^" Pow


parseTerm = chainl1 parseExp parseMinus

parseExp = chainl1 parseExpo parseMult 

parseExpo = chainr1 parseVal parsePow 


parseMathTLE :: ReadP TopLevelExp
parseMathTLE = parseTerm >>= (\s -> pure $ MathTLE s)

parseList :: ReadP a -> ReadP [a]
parseList parser = do
    skipSpaces
    _ <- optional . char $ '(' 
    x <- sepBy parser (char ',')
    _ <- optional . char $ ')' 
    skipSpaces
    return x

parseLetTLE :: ReadP TopLevelExp
parseLetTLE = do
    skipSpaces
    string "let"
    names <- parseList parseName
    string "="
    nums <- parseList parseTerm
    string "in"
    exp <- parseTerm
    return $ LetTLE names nums exp


parseTLE :: ReadP TopLevelExp
parseTLE = do
    tle <- parseLetTLE +++ parseMathTLE
    skipSpaces
    return tle

parse :: String -> Either String TopLevelExp
parse str =
    case (nub completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Right result
        ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show completeParses
    where
        parses = readP_to_S parseTLE str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
