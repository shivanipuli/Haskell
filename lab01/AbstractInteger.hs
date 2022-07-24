{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AbstractInteger where
import GHC.Exception (ratioZeroDenomException)
import Distribution.Simple.Utils (xargs)

-- Here are some definations for AbstractNatural.
-- You will probably define your AbstractInteger based on
-- AbstractNatural.

data AbstractNatural = Zero | S AbstractNatural
    deriving (Show)

--collabed with Omar, Van, Anuraag, & Kelly in study group and decided to base add, difference
-- and multiply off of addNat and multNat
addNat :: AbstractNatural -> AbstractNatural -> AbstractNatural -- used notes definition
addNat Zero y  = y
addNat (S x) y = S (addNat x y)

multNat :: AbstractNatural -> AbstractNatural -> AbstractNatural
multNat Zero y  = Zero
multNat x Zero  = Zero 
multNat (S x) y = addNat y (multNat x y)

-- Once we tell Haskell that AbstractNatural can do equality
-- comparisons and how AbstractNatural is totally ordered, we
-- get other functions for free, like /= and >= and > and <
--
-- You may not need these so I've left them commented out, but
-- you should understand why they work.
--
instance Eq AbstractNatural where
   Zero == Zero = True
   Zero == S _  = False
   S _  == Zero = False
   S x  == S y  = x == y
--
instance Ord AbstractNatural where
    Zero <= Zero = True
    Zero <= S _  = True
    S _  <= Zero = False
    S x  <= S y  = x <= y
--
-- successorNat :: AbstractNatural -> AbstractNatural
-- successorNat = S

-- predecessorNat :: AbstractNatural -> AbstractNatural
-- predecessorNat Zero  = Zero
-- predecessorNat (S x) = x


-- Figure out how you will define integers...
data AbstractInteger = Neg AbstractNatural | Pos AbstractNatural
    deriving (Show)

-- ...then fill out the functions below for your AbstractInteger type.

-- successor and predecessor based off of discussion in lab
successor :: AbstractInteger -> AbstractInteger
successor (Pos Zero)  = Pos (S Zero)
successor (Pos a)     = Pos (S a)
successor (Neg Zero)  = Pos (S Zero)
successor (Neg (S a)) = Neg a


predecessor :: AbstractInteger -> AbstractInteger
predecessor (Pos Zero)  = Neg (S Zero)
predecessor (Pos (S a)) = Pos a
predecessor (Neg Zero)  = Neg (S Zero)
predecessor (Neg a)     = Neg (S a)

-- Be sure to add type declarations to all these functions too.
negator :: AbstractInteger -> AbstractInteger
negator (Pos Zero) = Pos Zero
negator (Neg Zero) = Pos Zero
negator (Pos a)    = Neg a
negator (Neg a)    = Pos a

absolute :: AbstractInteger -> AbstractInteger
absolute (Pos Zero) = Pos Zero
absolute (Neg Zero) = Pos Zero
absolute (Pos a)    = Pos a
absolute (Neg a)    = Pos a

add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add (Pos Zero) y    = y
add x (Pos Zero)    = x
add (Neg Zero) y    = y
add x (Neg Zero)    = x
add (Neg x) (Pos y) = add (successor (Neg x)) (predecessor (Pos y))
add (Pos x) (Neg y) = add (predecessor (Pos x)) (successor (Neg y)) --add (Neg y) (Pos x)
add (Neg x) (Neg y) = Neg (addNat x y)
add (Pos x) (Pos y) = Pos (addNat x y)

difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference x y = add x (negator y)

multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger
multiply (Pos x) (Pos y) = Pos (multNat x y)
multiply (Pos x) (Neg y) = Neg (multNat x y)
multiply (Neg x) (Pos y) = Neg (multNat x y)
multiply (Neg x) (Neg y) = Pos (multNat x y)

-- To define division and modulo, you will probably need
-- comparison functions: == <= < > >=.
--
-- If you just provide == and <= below, Haskell will give
-- you the rest for free.
instance Eq AbstractInteger where
    (Pos Zero) == (Neg Zero) = True
    (Neg Zero) == (Pos Zero) = True
    (Pos x) == (Pos y)       = x == y
    (Neg x) == (Neg y)       = x == y
    (Pos x) == (Neg y)       = False
    (Neg x) == (Pos y)       = False

instance Ord AbstractInteger where
    (Pos Zero) <= (Neg Zero) = True 
    (Neg x) <= (Pos y)       = True 
    (Pos x) <= (Neg y)       = False
    (Pos x) <= (Pos y)       = x <= y
    (Neg x) <= (Neg y)       = y <= x

divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide (Pos n) (Pos d)  -- n as in numerator d as in denominator
    | Pos Zero <= Pos n && Pos n < Pos d = Pos Zero
    | otherwise =  successor (divide (difference (Pos n) (Pos d)) (Pos d))
divide (Pos n) (Neg d)
    | Pos Zero <= Pos n && Pos n < Pos d = Neg Zero
    | otherwise = predecessor (divide (add (Pos n) (Neg d)) (Neg d))
divide (Neg n) (Pos d)
    | Pos Zero <= Neg n && Neg n < Pos d = Neg Zero
    | otherwise = predecessor (divide (add (Neg n) (Pos d)) (Pos d))
divide (Neg n) (Neg d)
    | Pos Zero <= Neg n && Neg n < Pos d = Pos Zero
    | otherwise = successor( divide (difference (Neg n)(Neg d)) (Neg d))

modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo (Pos n) (Pos d)
    | Pos Zero <= Pos n && Pos n < Pos d = Pos n
    | otherwise = modulo (difference (Pos n) (Pos d)) (Pos d)
modulo (Pos n) (Neg d)
    | Pos Zero <= Pos n && Pos n < Pos d = Pos n
    | otherwise = modulo (add (Pos n) (Neg d)) (Neg d)
modulo (Neg n) (Pos d)
    | Pos Zero <= Neg n && Neg n < Pos d = Pos n
    | otherwise = modulo (add (Neg n) (Pos d)) (Pos d)
modulo (Neg n) (Neg d)
    | Pos Zero <= Neg n && Neg n < Pos d = Pos n
    | otherwise = modulo (difference (Neg n)(Neg d)) (Neg d)

toAbstract :: Integer -> AbstractInteger
toAbstract 0 = Pos Zero
toAbstract x
    | x > 0     = successor (toAbstract (x-1))
    | otherwise = negator(toAbstract(-1*x))

fromAbstract :: AbstractInteger -> Integer
fromAbstract (Pos Zero) = 0
fromAbstract (Neg Zero) = 0
fromAbstract (Pos x)    = 1 + fromAbstract (predecessor (Pos x))
fromAbstract (Neg x)    = -1 * fromAbstract(Pos x)

-- Take a list of strings, calculate, and return a string result.
-- You should not need to modify this, but you may eta-reduce it if you like.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
-- You will need to provide more cases.
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
    case (stack, inputList) of
        ( x:_,           [] )            -> x -- No more input, return top of stack.
        ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
        ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
        ( y:x:stackRest, "-":inputRest ) -> evalRPNStack (difference x y : stackRest) inputRest
        ( y:x:stackRest, "/":inputRest ) -> evalRPNStack (divide x y     : stackRest) inputRest
        ( y:x:stackRest, "%":inputRest ) -> evalRPNStack (modulo x y     : stackRest) inputRest
        ( x:stackRest  , "||":inputRest) -> evalRPNStack (absolute x     : stackRest) inputRest

        -- This last case handles numeric inputs, "0" "-2" "34" etc...
        ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
-- Define zero after you've written your definition of AbstractInteger.
-- Once you define zero you should get the rest for free.
zero  = Pos Zero
one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
