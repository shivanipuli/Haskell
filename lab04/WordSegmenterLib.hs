module WordSegmenterLib where

import Data.List
import Data.Ord
import Data.Function

unwrap :: Maybe Int -> Int
unwrap int = case int of 
    Nothing -> -1
    Just a -> a

findWordInd :: [String] -> String -> (Int,Int,[String])
findWordInd (x:xs) str
    | index == -1 = findWordInd xs str
    | otherwise   = (index , length x,x:xs)
    where index = unwrap $ findIndex (isPrefixOf x) (tails str)
findWordInd _ str = (0,length str,[])


splitWords :: [String] -> String -> [String]
splitWords olddict str 
    | str `elem` olddict = [str]
    | otherwise = 
        let 
         -- new dict only has words with length less than total string -> sorted from biggest to smallest word
            dict = takeWhile ((< length str) . length) olddict
            (ind,l,newdict) = findWordInd dict str -- l = wordLength
            words = splitWords newdict (take ind str) ++ [take l (drop ind str)] ++ splitWords newdict (drop (ind+l) str)
        in
            words