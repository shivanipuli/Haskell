module Main where

import Control.Monad.State
import Data.Map (Map,(!),singleton,unionsWith)
import System.Random
import Data.List
import Data.Ord

{- A module for producing "pretty" HTML. -}

--module Pretty where

--import Control.Monad.State

type Document = State (Int, String)

{- Append a String to the HTML document. -}

string :: String -> Document ()
string t = modify (\(i,s) -> (i,s ++ t))

{- Append a newline, indenting the subsequent text. -}

newline :: Document ()
newline = modify (\(i,s) -> (i, s ++ "\n" ++ concat(replicate i "    ")))

{- Render a Document as Text -}

render :: Document a -> String
render doc = (\(i,s) -> s) $ execState doc (0,"")

{- Increase the indentation level. -}

indent :: Document ()
indent = modify $ \(i,s) -> (i+1,s)

{- Decrease the indentation level. -}

exdent :: Document ()
exdent = modify $ \(i,s) -> (i-1,s)

{- An HTML tag which indents its content -}

type HTML = Document ()

tag :: String -> HTML -> HTML
tag t html = do
    newline
    inlineTag t $ do
        indent
        html
        exdent
        newline

{- An HTML tag which does not indent its content -}

inlineTag :: String -> HTML -> HTML
inlineTag t html = do
    string $ "<" ++ t ++ ">"
    void html
    string $ "</" ++ t ++ ">"

{- An HTML tag which presents its content on a new line -}

onelineTag :: String -> HTML -> HTML
onelineTag t html = newline >> inlineTag t html

{- Provide the functionality of the standard HTML tags -}

html, head, title, body, p, i, b, h1, h2, h3, h4, ol, ul, li, table, tr, th, td
    :: HTML -> HTML
html  = tag "html"
head  = tag "head"
title = onelineTag "title"
body  = tag "body"
p     = onelineTag "p"
i     = inlineTag "i"
b     = inlineTag "b"
h1    = onelineTag "h1"
h2    = onelineTag "h2"
h3    = onelineTag "h3"
h4    = onelineTag "h4"
ol    = tag "ol"
ul    = tag "ul"
li    = onelineTag  "li"
table = tag "table"
tr    = tag "tr"
th    = tag "th"
td    = tag "td"


type RandState = State StdGen
type Model = (String,Map String [Maybe String])

buildModel :: [String] -> Model
buildModel xs@(x:_) = (x,unionsWith (++) . transitions $ xs) where
    transitions (y:ys@(y':_)) = singleton y [Just y'] : transitions ys
    transitions [y] = [singleton y [Nothing]]
    transitions [] = error "Impossible error"
buildModel [] = error "Empty model"

runModel :: Model -> RandState [String]
runModel (start,wordmap) = iter start where
    iter word = (word:) <$> do
        maybeNext <- select $ wordmap ! word
        case maybeNext of
            Just nextWord -> iter nextWord
            Nothing -> pure []

roll :: Int -> RandState Int
roll n = state $ uniformR (1,n)

select :: [a] -> RandState a
select as = (as !!) . (subtract 1) <$> roll (length as)

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter current (nextWord:ys)
        | length current + length nextWord + 1 > n = current ++ "\n" ++ linefill n (nextWord:ys)
        | otherwise                   = iter (current ++ " " ++ nextWord) ys
    iter current [] = current ++ "\n"

helper :: Model -> RandState ([String],[String])
helper model = do
    output <- replicateM 1000 (runModel model)
    let sortedOutput = sortBy (comparing length) output
    let tup = (Prelude.head sortedOutput, last sortedOutput)
    pure tup

main :: IO ()
main = do
    input <- getContents
    gen <- getStdGen
    let model = buildModel (words input)
    let (short,long) = evalState (helper model) gen
    putStr . linefill 72 $ short
    putStrLn ""
    putStr . linefill 72 $ long

