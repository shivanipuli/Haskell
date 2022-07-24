import System.IO
import Lab3

main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     print $ eval . parse . tokenize $ line
     main
