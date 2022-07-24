module Main where
    
import System.Environment
import Data.Char ( toUpper )

upperName :: [Char] -> [Char]
upperName [] = []
upperName (x:xs) = Data.Char.toUpper x: xs

main :: IO ()
main = do
    putStrLn "Hello. I am a HAL 9000 series computer."
    name <- getEnv "USER"
    putStrLn $ "Good morning, " ++ upperName name  ++ "."
