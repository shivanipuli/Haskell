-- | 21.1 The (.) function has a similar type except the first two arguments
--   are flipped

{-
(a -> B) -> (B -> y) |- (a -> y)

|(a -> B) -> (B -> y)
_____________________
| a -> B
| B -> y
| | a
| _________
| | B
| | | B
| | ________
| | | y
| a -> y
So (a -> B) -> (B -> y) |- (a -> y), 
therefore (a -> B) -> (B -> y) -> (a -> y)

(p -> q), (q -> r), p |- p -> r hypo
(p -> q), (q -> r), p |- q -> r hypo
(p -> q), (q -> r), p |- p      hypo 
(p -> q), (q -> r), p |- q      modus ponens (1,3)
(p -> q), (q -> r), p |- r      modus ponens (2,4)

(p -> q), (q -> r), p |- r                               
(p -> q), (q -> r)    |- p -> r                          deduction thm
(p -> q)              |- (q -> r) -> p -> r              deduction thm     
                      |- (p -> q) -> (q -> r) -> p -> r  deduction thm
                        

-}

-- | 21.2 The haskell function either is most similar to the formula. It
-- takes two functions that turn (a -> c) -> (b -> c) -> Either a b into a c .


{- 
-- | 21.3 (a V B) -> ¬ a -> B

Q [a := T] = (T V B) -> ¬ T -> B
           = T -> ⊥ -> B 
           = ⊥ -> B
           = T 

Q [a := ⊥] = (⊥ V B) -> ¬ ⊥ -> B
           = B -> T -> B 
           = B -> B
           = T

-}