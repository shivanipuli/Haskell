data Level
  = Undergraduate
  | Masters
  | PhD
  | Other
  deriving Show


name :: String
name = "Shivani Puli"

level :: Level
level = Undergraduate

major :: String
major = "Computer Science"

why :: String
why = "I was first introduced to computer science in freshmen year and I loved it since then! My favorite thing to do is taking games that I love to play like sudoku and sliding puzzles and coding them using AI."

distance :: Int -> Int -> Int
distance rate time = rate * time

main :: IO ()
main = do
    putStrLn "Hello, world!"
    putStrLn ""
    putStrLn ("My name is " ++ name ++ ".")
    putStrLn ("I am a " ++ show level ++ " student.")
    putStrLn ("I'm going into " ++ major ++ ".")
    putStrLn ("I'm in this class because " ++ why)
    putStrLn ""
    putStrLn ("If you travel for 15mph for 30 hours you will go " ++ show (distance 15 30) ++ " miles.")
