module Tautcheck where 

data Proposition
    = Var String
    | Boolean Bool
    | Not Proposition
    | And Proposition Proposition
    | Or Proposition Proposition
    | Implies Proposition Proposition

abstractEval :: (Applicative m) 
                => (String -> m b)    -- ^ Var
                -> (Bool -> m b)      -- ^ Boolean
                -> (b -> b)           -- ^ Not
                -> (b -> b -> b)      -- ^ And
                -> (b -> b -> b)      -- ^ Or
                -> (b -> b -> b)      -- ^ Implies
                -> Proposition -> m b
                
abstractEval varf boolf notf andf orf impliesf = eval where
    eval (Var a)       = varf a
    eval (Boolean b)   = boolf b
    eval (Not p)       = pure notf  <*> eval p
    eval (And p q)     = pure andf <*> eval p <*> eval q
    eval (Or p q)      = pure orf  <*> eval p <*> eval q
    eval (Implies p q) = pure impliesf <*> eval p <*> eval q