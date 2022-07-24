module Homework19 where

import Data.Functor
import Text.ParserCombinators.ReadP
import Control.Monad.State

greedy :: ReadP a -> ReadP [a]
greedy a = greedy1 a <++ return []

greedy1 :: ReadP a -> ReadP [a]
greedy1 p = liftM2 (:) (p) (greedy p)