module Day11 where

example1 = do
    [(),()]
    x <- [1,2,3]
    pure x

example1_ = 
   --[(),()] >>= (\_ -> [1,2,3] >>= \x -> pure x)
   --[(),()] >>= (\_ -> [1,2,3] >>= pure)
   --[(),()] >> ( [1,2,3] >>= pure)
   [(),()] >> [1,2,3]
{-
example2 =
    pure 1 >>= (\x ->
        pure (x + x) >>= \(x ->
            pure (x * 2) >>= (\x ->
            pure x)))
-}

{-}
example3 = do
        x <- pure $ 1
        x <- pure $ x + x
        x <- pure $ x * 2
        return x
-}
examplelist = 
    flip concatMap [(),()] (\_ ->
    flip concatMap [1,2,3] (\x ->
            [x]))

example4 = 
    flip concatMap [1,2,3] (\x ->
    flip concatMap [()] (\_ ->
            [x]))
    
guard :: Bool -> Maybe ()
guard True = Just ()
guard False = Nothing

example5 = do
        x <- pure $ 1
        x <- pure $ x + x
        x <- pure $ x * 2
        return x



{-}
example7 = 
    --do { () <- [(),()]; x <- [1,2,3]; pure x}
    --[(),()] >>= (\()-> do { x <- [1,2,3]; pure x})
    [(),()] >>= [1,2,3] >>=  \(x -> pure x)
-}

example8 = flip concatMap [(),()] (\() -> flip concatMap [1,2,3] (\x -> pure x))
-- >>=  =  flip concatMap