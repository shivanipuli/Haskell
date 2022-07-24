module Main where

import Data.List
import Data.Ord
import System.IO
import WordSegmenterLib

main = do
    handle   <- openFile "/usr/share/dict/words" ReadMode
    contents <- hGetContents handle
    let dict = words contents
    let sortedDict = reverse $ sortBy (comparing length) dict
    input <- getContents
    print (splitWords sortedDict input)
    hClose handle
