module RandState where

import System.Random

newtype RandState a = RandState {
  runRandState :: StdGen -> (a, StdGen)
}

instance Functor RandState where
--fmap :: (a -> b) -> RandState a -> RandState b 
  fmap f genA = RandState $ (\(a,gen') -> (f a, gen')) . runRandState genA 

instance Applicative RandState where
-- pure :: a -> RandState a 
  pure a = RandState $ \s -> (a,s)
-- (<*>) :: Randstate (a -> b) -> Randstate a -> Randstate b
  af <*> genA = RandState $ \gen' ->
    let (f,t) = runRandState af gen'
        (a,u) = runRandState genA t 
    in (f a, u)

instance Monad RandState where
-- Code given by Ben in lab
-- (>>=) :: RandState a -> (a -> RandState b) -> RandState b 
  (>>=) (RandState sToAS) aToRSB = 
    RandState (\gen ->
      let (a, gen')        = sToAS gen in 
      let (RandState soToBS)  = aToRSB a in
    soToBS gen'
    )

{- primitive manipulation functions -}

get :: RandState StdGen
get = RandState $ \gen -> (gen, gen)

put :: StdGen -> RandState ()
put gen' = RandState $ \gen -> ((), gen')

-- runRandom runs a RandState monad, given an initial random number generator
-- runRandom is equivalent to evalState
runRandom :: RandState a -> StdGen -> a
runRandom (RandState f) s = fst $ f s

-- rand is a helper function that generates a random instance of any
--  type in the Random class, using the RandState monad.
rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x
