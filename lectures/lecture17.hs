module Lecture17 where

import Control.Monad.State 
import System.Random

type Stack a = [a]


type StackFunc a b = Stack a -> (b,Stack a)

--type StatefulFunc state a = state -> (a,state)

newtype StatefulFunc state a = StatefulFunc {getStatefulFunc :: state -> (a,state)}

instance Monad (StatefulFunc state) where
-- return :: a -> StatefulFunc state a 
return a = StatefulFunch $ \state -> (a, state)

-- (>>=) :: StatefulFunc state a -> (a -> StatefulFunc state b) -> StatefulFunc state b
StatefulFunc f >>= g = StatefulFunc $  \state0 ->
    let (a,fState) = f state0
        StatefulFunc h = g a
    -- h::state -> (b,state)
    in
        h fState 
-- f :: state -> (a, state)
-- g :: (a -> StatefulFunc state b)

instance Functor (StatefulFunc s) where {fmap f x = pure f <*> x}
instance Applicative (StatefulFunc s) where {pure = return; (<*>) = ap}

push :: a -> StatefulFunc ((),Stack a)
push a = StatefulFunc $ \stack -> ((),a:stack)

pop :: {-forall a. -} StatefulFunc (Stack a) a 
pop = Stateful $ \(a:as) -> (a,as)

get :: StatefulFunc state state 
get = StatefulFunc $ \s -> (s,s)

put :: state -> StatefulFunc state ()
put s' = StatefulFunc $ \s -> ((), s')


incrementEachElement :: StatefulFunc (Stack Int) ()
incrementEachElement = StatefulFunc $ \s -> ((), map (+1) s)

popIfNonEmpty :: StatefulFunc (Stack a) ()
popIfNonEmpty = do
    currentStack <- case currentStack of
        [] -> pure ()
        _ -> do {
            pop
            pure()
        }

testStack s0 = 
    let blah = do
        {-
        --blah :: StatefulFunc (Stack Int) ()
        push 1 >>= \_ ->
        push 2 >>= \_ ->
        push 3 >>= \_ ->
        pop >>= \a ->
        pop >>= \b ->
        push $ a + b
        -}
        _ <- push 1
        _ <- push 2
        _ <- push 3
        a <- pop
        b <- pop
        push $ a + b
    in
        getStateFunc blah s0
