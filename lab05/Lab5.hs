import RandState
import Control.Monad.State
import System.Environment
import System.IO
import System.Random
import Data.List


-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
            suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
            suitStr Spades   = "♠"
            suitStr Clubs    = "♣"
            -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
            -- suitStr Diamonds = "D"
            -- suitStr Spades   = "S"
            -- suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

-- basic random functions

randR :: Random a => (a, a) -> RandState a
randR range = do
    gen <- RandState.get
    let (x, gen') = randomR range gen
    RandState.put gen'
    return x

rollTwoDice :: RandState Int
rollTwoDice = do
    roll1 <- randR (1,6)
    roll2 <- randR (1,6)
    return $ roll1+roll2


removeCard :: Deck -> RandState (PlayingCard,Deck)
removeCard deck = do
    index <- randR (0, (length deck)-1) -- <- :: RandState Int -> Int
    let card = deck !! index -- let :: Int -> PlayingCard
    let newdeck = delete card deck -- let :: PlayingCard ->  Deck
    return (card,newdeck) -- pure/return :: (PlayingCard,Deck) -> RandState (PlayingCard,Deck)

--collaborated with Emi about logic behind shuffling a deck
shuffleDeck :: Deck -> RandState Deck 
shuffleDeck [] = pure []
shuffleDeck deck = do
    (card,newdeck) <- removeCard deck
    restOfDeck <- shuffleDeck newdeck
    let shuffledDeck = card : restOfDeck
    return shuffledDeck

shuffleADeck :: RandState Deck
shuffleADeck = shuffleDeck fullCardDeck

-- Omar suggested using helper function and replicateM
helper :: (Show a) => RandState a -> Int -> StdGen -> RandState [String]
helper funct nTimes gen = do
    output <- Control.Monad.State.replicateM nTimes funct
    let toReturn = map show output
    pure toReturn

shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen = do
        let shuffledDeck = runRandom (helper shuffleADeck nTimes gen) gen
        putStrLn $ unlines shuffledDeck

    
rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen = do
    let diceRolls = runRandom (helper rollTwoDice nTimes gen) gen
    putStrLn $ unlines diceRolls

usage :: String
usage =
    "Lab 5: Randomizer\n" ++
    "\n" ++
    "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
    "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
    "\n"

main :: IO ()
main = do
    gen  <- newStdGen
    args <- getArgs
    case args of
        ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
        ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
        _                       -> putStrLn usage
