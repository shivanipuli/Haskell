module Day17 where

import Control.Monad.State.Lazy

instance Monad (State s) where

{-
        ma >>= f = State $ \s ->
            let (a,t) = runState ma s
                mb = f a
                (b,u) = runState mb t
            in (b,u)
-- | Transform (b,u) = runState mb t
        ma >>= f = State $ \s ->
            let (a,t) = runState ma s
                (b,u) = runState (f a) t
            in (b,u) 

        ma >>= f = State $ \s ->
            let (a,t) = runState ma s
                in runState (f a) t
-}
-- | t just carries through while f is applied to a so
        ma >>= f = State $ \s ->
            (\(a,t) -> runState (f a) t . runState ma) 

-- | replace t with s
        ma >>= f = State $ \(a,s) -> runState (f a ) s . runState ma  
 --       ma >>= f = State $
 --           (\(a,s) -> runState (f a) s) . runState ma
