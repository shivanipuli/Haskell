module HTML where

import Control.Monad.State

type Document = State String 

string :: String -> Document ()
string = modify . flip (++) -- modify (\s -> s ++ t)

render :: Document a -> String
render doc = execState doc ""

type HTML = Document () 

tag :: String -> HTML -> HTML
tag t html = do
    string $ "<" ++ t ++ ">"
    html
    string $ "</"++ t ++ ">"

html  = tag "html"
head  = tag "head"
title = tag "title"
body  = tag "body"
p     = tag "p"
i     = tag "i"
b     = tag "b"
h1    = tag "h1"
h2    = tag "h2"
h3    = tag "h3"
h4    = tag "h4"
ol    = tag "ol"
ul    = tag "ul"
table = tag "table"
tr    = tag "tr"
th    = tag "th"
td    = tag "td"

{-
doc :: HTML
doc =
    html $ do
        HTML.head $ do
            title $ string "Hello, world!"
        body $ do
            h1 $ string "Greetings"
            p $ string "Hello, world!"
-}
