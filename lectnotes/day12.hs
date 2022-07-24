    -- | A simple version of the UNIX "cat" program
    
    module Main where
    
    import System.Environment
    
    -- | Process a list of files, writing the contents of each to standard output.
    
    outputFile :: FilePath -> IO ()
    outputFile path = readFile path >>= putStr

    {- outputFiles :: [FilePath] -> IO ()
    outputFiles [] = pure ()
    outputFiles (f:fs) = do
        readFile f >>= putStr
        outputFiles fs
    -}
    {-
    outputFiles (f:fs) = do
        content <- readFile f
        putStr content
        outputFiles fs
    -}

    main :: IO ()
    main = 
        getArgs >>= mapM_ outputFile
        --getArgs >>= outputFiles
        {-
        do
        args <- getArgs
        outputFiles args
        -}

    {- 
    -- | The annoying "frog" program.

    module Main where
    import System.IO
    import Data.Functor
    import Control.Monad
    import Data.Function ()
    
    -- | Produce a String containing n lines, each of consisting of "frog."
    
    manyFrogs :: Int -> String
    manyFrogs = unlines . ( `replicate` "frog")
    
    helperenum :: Int -> String -> Int -> String
    helperenum ind cont total = concat (replicate (length(show total) - length(show ind)) " ")  ++ show ind ++ ". " ++ cont

    enumerate :: String -> String
    enumerate input = unlines $ zipWith (\x y -> helperenum x y (length $ lines input)) [1..] $ lines input

-- | Prompt for a line of input
    
    prompt :: String -> IO String
    prompt msg = putStr msg *> hFlush stdout *> getLine

    -- | The main act...

    main :: IO ()
    main = interact $ map toUpper
        --getContents <&> enumerate >>= putStr
        
        -- frogs <- manyFrogs . read <$> prompt "How many frogs? " >>= putStr

-}