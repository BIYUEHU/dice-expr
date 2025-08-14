module Test.Main

import Test.Parser
import Test.Evaluator

export
testMain : IO ()
testMain = do
  putStrLn "Testing Parser..."
  testParser
  putStrLn "Testing Evaluator..."
  testEvaluator