module Test.Evaluator

import Evaluator
import Value
import Ast
import Utils
import Test.Utils

-- Test values and expressions
val42 : Value
val42 = VNum 42.0

valTrue : Value
valTrue = VBool True

valFalse : Value
valFalse = VBool False

valArray123 : Value
valArray123 = VArray [VNum 1.0, VNum 2.0, VNum 3.0]

valEmptyArray : Value
valEmptyArray = VArray []

valLambda : Value
valLambda = VLambda ["x"] (DIdent "x")

-- Test expressions
expr42 : DExpr
expr42 = DDLiteral (DNumber 42.0)

exprTrue : DExpr
exprTrue = DDLiteral (DBool True)

exprFalse : DExpr
exprFalse = DDLiteral (DBool False)

exprArray : DExpr
exprArray = DDLiteral (DArray [DDLiteral (DNumber 1.0), DDLiteral (DNumber 2.0), DDLiteral (DNumber 3.0)])

exprEmptyArray : DExpr
exprEmptyArray = DDLiteral (DArray [])

exprLambda : DExpr
exprLambda = DLambda ["x"] (DIdent "x")

-- Helper function to test evaluation results
testEval : String -> DExpr -> Value -> IO TestResult
testEval name expr expected = do
  result <- evaluate expr
  pure $ case result of
    Right actual => assertEq actual expected
    Left err => assert False -- Should not fail

testEvalError : String -> DExpr -> IO TestResult
testEvalError name expr = do
  result <- evaluate expr
  pure $ case result of
    Left _ => assert True -- Should fail
    Right _ => assert False -- Should not succeed

testGroupIO : String -> List (IO TestResult) -> IO ()
testGroupIO name tests = do
  results <- sequence tests
  testGroup name results

-- Test literals
testLiterals : IO ()
testLiterals = testGroupIO "Literals" [
    testEval "Number literal" expr42 val42,
    testEval "Boolean true literal" exprTrue valTrue,
    testEval "Boolean false literal" exprFalse valFalse,
    testEval "Array literal" exprArray valArray123,
    testEval "Empty array literal" exprEmptyArray valEmptyArray,
    testEval "Lambda literal" exprLambda valLambda
  ]

-- Test arithmetic operations
testArithmetic : IO ()
testArithmetic = testGroupIO "Arithmetic operations" [
  testEval "Addition" (DInfix (DDLiteral (DNumber 2.0)) DAdd (DDLiteral (DNumber 3.0))) (VNum 5.0),
  testEval "Subtraction" (DInfix (DDLiteral (DNumber 5.0)) DSub (DDLiteral (DNumber 3.0))) (VNum 2.0),
  testEval "Multiplication" (DInfix (DDLiteral (DNumber 4.0)) DMul (DDLiteral (DNumber 3.0))) (VNum 12.0),
  testEval "Division" (DInfix (DDLiteral (DNumber 12.0)) DDiv (DDLiteral (DNumber 3.0))) (VNum 4.0),
  testEval "Modulo" (DInfix (DDLiteral (DNumber 10.0)) DMod (DDLiteral (DNumber 3.0))) (VNum 1.0),
  testEval "Power" (DInfix (DDLiteral (DNumber 2.0)) DPow (DDLiteral (DNumber 3.0))) (VNum 8.0),
  testEvalError "Division by zero" (DInfix (DDLiteral (DNumber 5.0)) DDiv (DDLiteral (DNumber 0.0)))
]

-- Test comparison operations
testComparison : IO ()
testComparison = testGroupIO "Comparison operations" [
  testEval "Equal true" (DInfix (DDLiteral (DNumber 5.0)) DEq (DDLiteral (DNumber 5.0))) valTrue,
  testEval "Equal false" (DInfix (DDLiteral (DNumber 5.0)) DEq (DDLiteral (DNumber 3.0))) valFalse,
  testEval "Not equal true" (DInfix (DDLiteral (DNumber 5.0)) DNeq (DDLiteral (DNumber 3.0))) valTrue,
  testEval "Not equal false" (DInfix (DDLiteral (DNumber 5.0)) DNeq (DDLiteral (DNumber 5.0))) valFalse,
  testEval "Greater than true" (DInfix (DDLiteral (DNumber 5.0)) DGt (DDLiteral (DNumber 3.0))) valTrue,
  testEval "Greater than false" (DInfix (DDLiteral (DNumber 3.0)) DGt (DDLiteral (DNumber 5.0))) valFalse,
  testEval "Greater equal true" (DInfix (DDLiteral (DNumber 5.0)) DGte (DDLiteral (DNumber 5.0))) valTrue,
  testEval "Greater equal false" (DInfix (DDLiteral (DNumber 3.0)) DGte (DDLiteral (DNumber 5.0))) valFalse,
  testEval "Less than true" (DInfix (DDLiteral (DNumber 3.0)) DLt (DDLiteral (DNumber 5.0))) valTrue,
  testEval "Less than false" (DInfix (DDLiteral (DNumber 5.0)) DLt (DDLiteral (DNumber 3.0))) valFalse,
  testEval "Less equal true" (DInfix (DDLiteral (DNumber 5.0)) DLte (DDLiteral (DNumber 5.0))) valTrue,
  testEval "Less equal false" (DInfix (DDLiteral (DNumber 5.0)) DLte (DDLiteral (DNumber 3.0))) valFalse,
  testEval "Less than true with sum of array and number" (DInfix exprArray DLt expr42) valTrue
]

-- Test logical operations
testLogical : IO ()
testLogical = testGroupIO "Logical operations" [
  testEval "AND true true" (DInfix exprTrue DAnd exprTrue) valTrue,
  testEval "AND true false" (DInfix exprTrue DAnd exprFalse) valFalse,
  testEval "AND false true" (DInfix exprFalse DAnd exprTrue) valFalse,
  testEval "AND false false" (DInfix exprFalse DAnd exprFalse) valFalse,
  testEval "OR true true" (DInfix exprTrue DOr exprTrue) valTrue,
  testEval "OR true false" (DInfix exprTrue DOr exprFalse) valTrue,
  testEval "OR false true" (DInfix exprFalse DOr exprTrue) valTrue,
  testEval "OR false false" (DInfix exprFalse DOr exprFalse) valFalse
]

-- Test prefix operations
testPrefix : IO ()
testPrefix = testGroupIO "Prefix operations" [
  testEval "Negation positive" (DPrefix DNeg (DDLiteral (DNumber 5.0))) (VNum (-5.0)),
  testEval "Negation negative" (DPrefix DNeg (DDLiteral (DNumber (-3.0)))) (VNum 3.0),
  testEval "Negation zero" (DPrefix DNeg (DDLiteral (DNumber 0.0))) (VNum 0.0),
  testEval "NOT true" (DPrefix DNot exprTrue) valFalse,
  testEval "NOT false" (DPrefix DNot exprFalse) valTrue
]

-- Test range operations
testRange : IO ()
testRange = testGroupIO "Range operations" [
  testEval "Range 1 to 3" (DInfix (DDLiteral (DNumber 1.0)) DRange (DDLiteral (DNumber 3.0))) (VArray [VNum 1.0, VNum 2.0, VNum 3.0]),
  testEval "Range 5 to 5" (DInfix (DDLiteral (DNumber 5.0)) DRange (DDLiteral (DNumber 5.0)))(VArray [VNum 5.0]),
  testEval "Range empty" (DInfix (DDLiteral (DNumber 3.0)) DRange (DDLiteral (DNumber 1.0))) (VArray [])
]

-- Test array concatenation
testConcat : IO ()
testConcat = testGroupIO "Array concatenation" [
  testEval "Concat two arrays"
    (DInfix (DDLiteral (DArray [DDLiteral (DNumber 1.0), DDLiteral (DNumber 2.0)])) DConcat (DDLiteral (DArray [DDLiteral (DNumber 3.0), DDLiteral (DNumber 4.0)]))) (VArray [VNum 1.0, VNum 2.0, VNum 3.0, VNum 4.0]),
  testEval "Concat with empty array"
    (DInfix (DDLiteral (DArray [DDLiteral (DNumber 1.0)])) DConcat (DDLiteral (DArray []))) (VArray [VNum 1.0]),
  testEval "Concat empty arrays"
    (DInfix (DDLiteral (DArray [])) DConcat (DDLiteral (DArray []))) (VArray [])
]

-- Test parentheses
testParentheses : IO ()
testParentheses = testGroupIO "Parentheses" [
  testEval "Parenthesized number"
    (DParen expr42) val42,
  testEval "Parenthesized boolean"
    (DParen exprTrue) valTrue,
  testEval "Parenthesized expression"
    (DParen (DInfix (DDLiteral (DNumber 2.0)) DAdd (DDLiteral (DNumber 3.0)))) (VNum 5.0)
]

-- Test lambda expressions and function calls
testLambdaAndCalls : IO ()
testLambdaAndCalls = testGroupIO "Lambda and function calls" [
  testEval "Identity lambda call"
    (DCall (DLambdaHead ["x"] (DIdent "x")) [DDLiteral (DNumber 42.0)])val42,
  testEval "Add lambda call"
    (DCall (DLambdaHead ["x", "y"] (DInfix (DIdent "x") DAdd (DIdent "y"))) [DDLiteral (DNumber 3.0), DDLiteral (DNumber 4.0)]) (VNum 7.0),
  testEvalError "Lambda wrong arity"
    (DCall (DLambdaHead ["x"] (DIdent "x")) [DDLiteral (DNumber 1.0), DDLiteral (DNumber 2.0)]),
  testEvalError "Call unknown identifier"
    (DCall (DIdentHead "unknownFunction") [DDLiteral (DNumber 42.0)])
]

-- Test error cases
testErrors : IO ()
testErrors = testGroupIO "Error cases" [
  testEvalError "Undefined identifier" (DIdent "undefined"),
  testEvalError "Type mismatch in arithmetic" (DInfix exprTrue DAdd expr42),
  testEvalError "Type mismatch in logical" (DInfix expr42 DAnd exprTrue),
  testEvalError "NOT on number" (DPrefix DNot expr42),
  testEvalError "Negation on boolean" (DPrefix DNeg exprTrue),
  testEvalError "Concat non-arrays" (DInfix expr42 DConcat exprTrue),
  testEvalError "Range non-numbers" (DInfix exprTrue DRange exprFalse)
]

-- Test complex expressions
testComplexExpressions : IO ()
testComplexExpressions = testGroupIO "Complex expressions" [
  testEval "Nested arithmetic"
    (DInfix (DInfix (DDLiteral (DNumber 2.0)) DAdd (DDLiteral (DNumber 3.0))) DMul (DInfix (DDLiteral (DNumber 4.0)) DSub (DDLiteral (DNumber 1.0)))) (VNum 15.0),
  testEval "Boolean logic chain"
    (DInfix (DInfix exprTrue DAnd exprFalse) DOr (DInfix exprTrue DAnd exprTrue)) valTrue,
  testEval "Range in array"
    (DDLiteral (DArray [DInfix (DDLiteral (DNumber 1.0)) DRange (DDLiteral (DNumber 2.0))])) (VArray [VArray [VNum 1.0, VNum 2.0]])
]

export
testEvaluator : IO ()
testEvaluator = do
  testLiterals
  testArithmetic
  testComparison
  testLogical
  testPrefix
  testRange
  testConcat
  testParentheses
  testLambdaAndCalls
  testErrors
  testComplexExpressions
