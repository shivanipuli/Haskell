module Chatty where

doubleSquareDouble :: Int -> Int
doubleSquareDouble n = 
    let 
        n1 = (*2) n
        n2 = (^2) n1
        n3 = (*2) n2
    in 
        n3

data NamedFunc a b = NamedFunc String (a -> b)

data Chatty a = Chatty (a,[String])
    deriving (Show)


instance Monad Chatty where
--  return :: a -> Chatty a
    return a = Chatty (a, [])

-- (>>=) :: Chatty a -> (a -> Chatty b) -> Chatty b
    Chatty (a,list1) >>= f = 
        let 
            Chatty (b, list2) = f a 
        in
            Chatty (b, list1 ++ list2)


applyNamedFunc :: (Show a, Show b) => NamedFunc a b -> a -> Chatty b
applyNamedFunc (NamedFunc name f) a = 
    let 
        b = f a
        s = name ++ " " ++ show a ++ " = " ++ show b
    in
        Chatty (b,[s])

chattyDoubleSquareDouble :: Int -> Chatty Int
chattyDoubleSquareDouble n =
    let 
        Chatty (n1,str1) = applyNamedFunc (NamedFunc "double" (*2)) n
        Chatty (n2,str2) = applyNamedFunc (NamedFunc "square" (^2)) n1
        Chatty (n3,str3) = applyNamedFunc (NamedFunc "double" (*2)) n2
    in 
        pure n3