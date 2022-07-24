module Main where

import qualified Data.Map.Strict as Map
import Data.Char
import Data.List

isWordChar :: Char -> Bool
isWordChar c = isLetter c || (==) '\'' c || (==) '-' c 

splitIntoWords :: String -> [String]
splitIntoWords [] = []
splitIntoWords str = takeWhile isWordChar str: splitIntoWords (tail $ dropWhile isWordChar str)

tasks :: String -> String 
tasks str =
    let 
     -- lowercases all chars in string >> splits string into words >> filters out any empty strings >> removes duplicates  
        wordList = nub $ filter (not . null) $ splitIntoWords $ map toLower str 
     -- makes singleton (sorted string, string) for each string value >> concats values with same key
     -- can match singletons by key because if sorted strings are equal, then strings are perms
        wordMap = Map.unionsWith (++) $ map (\x -> Map.singleton (sort x) [x]) wordList
     -- wordMap values >> sorts each group in [[String]] >> filters out one perm words >> sorts group order
        permList = sort . filter ((>1) .length) $ map sort $ Map.elems wordMap
        output = "\n" ++ (unlines $ map (intercalate ", " ) permList)
    in 
        output

main :: IO()
main = interact tasks