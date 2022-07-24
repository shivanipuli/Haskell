module Tests where

import Lab3

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message

-- Test a function that takes one argument.
-- Usage: expect1 "myFunc" myFunc arg expectedOutput
expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            "\n  to be   " ++ show expectedOutput ++
            "\n  but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input


-- This is where you add new test cases.
tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (Number (-3))
        (-3)
    , expect1 "eval" eval
        (Mult (Plus (Number 2) (Number (-6))) (Plus (Number 3) (Number 2)))
        (-20)
    , expect1 "eval" eval
        (Div (Number 13) (Number 6))
        2
    , expect1 "eval" eval
        (Plus (Mult (Number 2) (Number 3)) (Div (Number 13) (Number (-6))))
        4
    , expect1 "tokenize" tokenize "1+2"
        [TInt 1, TPlus, TInt 2]
     , expect1 "tokenize" tokenize
         "1 + 20"
        [TInt 1, TPlus, TInt 20]
     , expect1 "tokenize" tokenize
         "1 * -2"
        [TInt 1, TMult, TInt (-2)]
     , expect1 "tokenize" tokenize
         "1 + 2 * 3 + 4"
         [TInt 1, TPlus, TInt 2,TMult, TInt 3, TPlus, TInt 4]
     , expect1 "tokenize" tokenize
         "1 * 2 + 3 + 4"
         [TInt 1, TMult, TInt 2,TPlus, TInt 3, TPlus, TInt 4]
     , expect1 "tokenize" tokenize
         "(1+2)"
        [TParen [TInt 1, TPlus, TInt 2]]
     , expect1 "tokenize" tokenize
         " (1 + 2 )"
         [TParen[TInt 1, TPlus, TInt 2]]
     , expect1 "tokenize" tokenize
         "(1 * 2 + 3)"
         [TParen [TInt 1, TMult, TInt 2,TPlus,TInt 3]]
     , expect1 "tokenize" tokenize
         "(-10 + 2) * 5"
         [TParen [TInt (-10), TPlus, TInt 2],TMult,TInt 5]
     , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3)"
         [TInt 5, TMult, TParen[TInt (-10), TPlus, TParen[TInt 2, TPlus, TInt  4], TMult, TInt 3]]
     , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
        [TInt 5, TMult, TParen[TInt (-10), TPlus, TParen[TInt 2, TPlus, TInt  4], TMult, TInt 3], TMult, TParen [TInt 3, TPlus, TInt 2]]
    , expect1 "parse" parse 
        [TInt 1, TPlus, TInt 20]
        (Plus (Number 1) (Number 20))
    , expect1 "parse" parse 
        [TInt 1, TMult, TInt (-2)]
        (Mult (Number 1) (Number (-2)))
    , expect1 "parse" parse 
        [TInt 1, TPlus, TInt 2,TMult, TInt 3, TPlus, TInt 4]
        (Plus (Number 1) ( Plus (Mult (Number 2) (Number 3)) (Number 4)))
    , expect1 "parse" parse 
        [TInt 7, TDiv, TParen [TInt 2, TPlus, TInt (-5)]]
        (Div (Number 7) (Plus (Number 2) (Number (-5))))
    , expect1 "parse" parse
        [TInt 2, TMult, TInt 3, TPlus, TInt 13, TDiv, TInt (-6)]
        (Plus (Mult (Number 2) (Number 3)) (Div (Number 13) (Number (-6))))
    , expect1 " tokenize -> parse -> eval" (eval . parse . tokenize)
        "1 + 2 * 3 + 4"
        11
    , expect1 " tokenize -> parse -> eval" (eval . parse . tokenize)
        "5 * (-10 + (2 + 4) * 3)"
        40
    , expect1 " tokenize -> parse -> eval" (eval . parse . tokenize)
        "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
        200
    , expect1 " tokenize -> parse -> eval" (eval . parse . tokenize)
        "-2 * (5 * 6 + -32) + 8"
        12
    , expect1 " tokenize -> parse -> eval" (eval . parse . tokenize)
        "7 / (2 + -5) + -2 * 3"
        (-8)
    , expect1 " tokenize -> parse -> eval" (eval . parse . tokenize)
        "3 * 2 / 3"
        2
    ]


-- Inspect the below in GHCi.

-- DO NOT MODIFY BELOW THIS LINE IN YOUR SUBMISSION --

successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
