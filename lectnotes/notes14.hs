module Notes14 where

import Data.List
import Data.Monoid
import Control.Applicative

-- 14.3 For Maybe a, the type of a doesn't actually matter for the Monoid class
-- to be implemented. For Either s t, both types s and t have to be monoids too
-- for the <> and mempty functions to be used.

altconcat :: Alternative f => [f a] -> f a
altconcat = foldr (<|>) empty

{-
x :: Maybe Int
x = altconcat [Nothing, Just 2]
--let y = altconcat [Just 1, Nothing]
--let z = altconcat [Nothing, Just 2]

temp :: Maybe Int
temp = getFirst . mconcat . map First $ [Just 3, Just 1]
-}