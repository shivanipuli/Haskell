import RandState
import Control.Monad.State

piHelper :: RandState Double 
piHelper = do 
    x <- rand
    y <- rand
    return (x*x+y*y)
    
-- Succeed if randomly chosen point from square is inside circumscribed circle
piTrial :: RandState Bool
piTrial = do 
    dist <- piHelper
    let isTrue = piHelper < 1.0
    return isTrue

-- Perform n trials of the RandState function provided as the second argument,
--  and give back the number of successful trials
-- Hint: To perform the n trials, you can either use sequence from
--       Control.Monad, or you can use recursion
bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials nTimes funct = do
    output <- Control.Monad.State.replicateM nTimes funct
    let count = length $ filter (==True) output 
    return count

-- Approximate pi using n randomly chosen points
-- Hint: You will probably need to use the fromIntegral function to
--       convert Int into Double.
approxPi :: Int -> RandState Double
approxPi nTimes = do
    count <- bernoulliTrials nTimes piTrial
    let ratio = (fromIntegral (4 * count)) / (fromIntegral nTimes)
    return ratio


main :: IO()
main = do
    input <- getLine
    let nTimes = read input
    print $ show $ approxPi nTimes