module Exercise16 where

-- Exercise 16.3.

    -- Drawing in separate submitted document


-- Exercise16.4.

-- If we follow simple substitution and deriving from the fact that:

--     mb = f a and (b,u) = runState mb t, we can transform our action to:

--         runState (f a) t

--     With this form in mind, and with bind type constructor in mind:
--     (>>=) :: m a -> (a -> m b) -> m b, it makes it obviously simple
--     that our function should look like:

--         (\(a,t) -> runState (f a t) . m ma) where our m is our function,
            -- which in this case is runState, finally getting:

            -- (\(a,t) -> runState (f a t) . runState ma)

            -- In its complete form:
            -- instance Monad (State t) where
            --         ma >>= f = State $
            --             (\(a,t) -> runState (f a) t) . runState ma




