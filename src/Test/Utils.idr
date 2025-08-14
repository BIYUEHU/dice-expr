module Test.Utils

public export
TestResult: Type
TestResult = Maybe String

export
assertEq : Show a => Eq a => a -> a -> TestResult
assertEq x y = if x == y then Nothing else Just $ "Error: Expected " ++ show x ++ " but got " ++ show y

export
assert : Bool ->  TestResult
assert True = Nothing
assert False = Just "Error: Assertion failed"

export
assertCond : Show a => Eq a => a -> (a -> Bool) ->  TestResult
assertCond x p = if p x then Nothing else Just $ "Error: Expected " ++ show x ++ " to satisfy the condition"

export
testGroup : String -> List (TestResult) -> IO ()
testGroup name tests = f tests 1 0
  where
    p : String
    p = "\x1b[1mTest group >> " ++ name

    f : List (TestResult) -> Int -> Int -> IO ()
    f [] i e = if e == 0 then putStrLn $ "\x1b[32m" ++ p ++ " >> All tests passed\x1b[0m" else putStrLn $ "\x1b[31m" ++ p ++ " >> " ++ show e ++ " Tests failed\x1b[0m"
    f (Nothing :: xs) i e = f xs (i+1) e
    f (Just err :: xs) i e = do
      putStrLn $ "\x1b[31m" ++ p ++ " >> Test " ++ show i ++ " failed: " ++ err ++ "\x1b[0m"
      f xs (i+1) (e+1)

