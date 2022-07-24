-- module MergeSort (someFunc) where

-- | MergeSort provides a polymorphic mergeSort function
    
module MergeSort where

-- | sort a list using the merge sort algorithm

mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined

-- | split a list into two lists of almost equal length

split :: Ord a => [a] -> ([a],[a])
split = undefined

-- | merge two sorted lists into a single sorted list

merge :: Ord a => [a] -> [a] -> [a]
merge = undefined


-- cabal init -i