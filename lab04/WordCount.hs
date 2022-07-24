module Main where

import qualified Data.Map.Strict as Map
import Data.Char

isWordChar :: Char -> Bool
isWordChar c = isLetter c || (==) '\'' c || (==) '-' c

splitIntoWords :: String -> [String]
splitIntoWords [] = []
splitIntoWords str = takeWhile isWordChar str: splitIntoWords (tail $ dropWhile isWordChar str)

tasks :: String -> String
tasks str =
    let 
     -- lowercases all chars in string >> splits string into words >> filters out any empty strings  
        wordList = filter (not . null) $ splitIntoWords $ map toLower str
     -- makes singletons per word with count 1 >> concats and adds counts >> sorts
        wordMap = Map.toAscList $ Map.unionsWith (+) $ map (\x -> Map.singleton x 1) wordList
        output = (++) "\n" (unlines $ map (\(x,y) -> x ++ " " ++ show y) wordMap)
    in 
        output

main :: IO()
main = interact tasks