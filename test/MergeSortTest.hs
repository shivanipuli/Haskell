module Main where 

-- module MergeSortTest where 

-- import Test.Hspec

pendingTests :: Spec
pendingTests = do
    it "needs some tests" $ do
        pending

mergeSortSpec :: Spec
mergeSortSpec = describe "mergeSort" $ do
    pendingTests

-- | split a list into two lists of almost equal length

splitSpec :: Spec
splitSpec = describe "split" $ do
    pendingTests

-- | merge two sorted lists into a single sorted list

mergeSpec :: Spec
mergeSpec = describe "merge" $ do
    pendingTests

main :: IO ()
main = hspec $ do
    mergeSortSpec
    splitSpec
    mergeSpec