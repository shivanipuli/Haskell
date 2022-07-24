module Day15 where

import Control.Applicative

type Eval = Identity

applyBinary :: (Num n,Show n) => (n -> n -> n) -> Expr n -> Expr n -> Eval n
    applyBinary op e1 e2 = eval e1 `op` eval e2 Prelude.>>= result

applyUnary :: (Num n,Show n) => (n -> n) -> Expr n -> Eval n
    applyUnary op expr = eval expr Prelude.>>= result 

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
fmap f (Reader a) = Reader (\x -> f (a x))
-- fmap f reader = Reader (\x -> f ((runReader reader) x))

instance Applicative (Reader e) where
pure x = Reader $ const x
Reader f <*> Reader g = Reader (\n -> f n (g n))


instance Monad (Reader e) where
Reader x >>= f = Reader (\n -> 
    let Reader g = f (x n) in
        g n)