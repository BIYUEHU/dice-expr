module Main

import Parser
import Ast
import Evaluator
import Value
import Utils
import Test.Main

isOperation : String -> String -> Bool
isOperation t s = t == s || t ++ "\r\n" == s || t ++ "\n" == s

loop : Bool -> IO ()
loop onlyAst = do
  putStr "> "
  input <- getLine
  if isOperation "q" input then pure ()
    else if isOperation "s" input then loop $ not onlyAst
    else if isOperation "t" input then testMain
    else do
      if isOperation "" input then pure () else do
      case parse input of
        Left err => putStrLn $ "Parse error: " ++ err
        Right ast => if onlyAst then putStrLn $ "AST: " ++ show ast else do
          result <- evaluate ast
          case result of
            Left err => putStrLn $ "Evaluation error: " ++ err
            Right val => putStrLn $ "Result: " ++ show val
      loop onlyAst

main : IO ()
main = do
  putStrLn "Welcome to Dice Expressions!"
  putStrLn "Type 'q' to exit."
  putStrLn "Type 's' to switch to Parser and Evaluator mode."
  loop False
