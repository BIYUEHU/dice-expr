module Test.Parser

import Ast
import Parser
import Utils
import Test.Utils

allPrefixOps : List DPrefixOp
allPrefixOps = [DNeg, DNot]

allInfixOps : List DInfixOp
allInfixOps =
  [ DAdd, DSub, DMul, DDiv, DMod, DPow
  , DEq, DNeq, DGt, DGte, DLt, DLte
  , DAnd, DOr, DDice, DRandom, DRange, DConcat
  ]

num42 : DExpr
num42 = DDLiteral (DNumber 42.0)

boolTrue : DExpr
boolTrue = DDLiteral (DBool True)

identX : DExpr
identX = DIdent "x"

lambdaX : DExpr
lambdaX = DLambda ["x"] identX

callF : DExpr
callF = DCall (DIdentHead "f") [num42, identX]

paren42 : DExpr
paren42 = DParen num42

arrayExpr : DExpr
arrayExpr = DDLiteral (DArray [num42, boolTrue, identX])

testPrefixOps : IO ()
testPrefixOps = testGroup "Prefix operators" $ map (\op => assertEq (parse (prefixStr op ++ "42")) (Right (DPrefix op num42))) allPrefixOps
  where
    prefixStr : DPrefixOp -> String
    prefixStr DNeg = "-"
    prefixStr DNot = "!"


testInfixOps : IO ()
testInfixOps = testGroup "Infix operators" $ map (\op => assertEq (parse ("42 " ++ infixStr op ++ " 42")) (Right (DInfix num42 op num42))) allInfixOps
  where
    infixStr : DInfixOp -> String
    infixStr DAdd = "+"
    infixStr DSub = "-"
    infixStr DMul = "*"
    infixStr DDiv = "/"
    infixStr DMod = "%"
    infixStr DPow = "^"
    infixStr DEq = "=="
    infixStr DNeq = "!="
    infixStr DGt = ">"
    infixStr DGte = ">="
    infixStr DLt = "<"
    infixStr DLte = "<="
    infixStr DAnd = "&&"
    infixStr DOr = "||"
    infixStr DDice = "d"
    infixStr DRandom = "~"
    infixStr DRange = ".."
    infixStr DConcat = "++"
    infixStr DColon = ":"

testDExprConstructors : IO ()
testDExprConstructors = testGroup "DExpr constructors" $ map (\(s, e) => assertEq (parse s) (Right e)) [
  ("42", num42)
  ,("T", boolTrue)
  ,("x", identX)
  ,("\\x -> x", lambdaX)
  ,("f(42, x)", callF)
  ,("(42)", paren42)
  ,("[42, T, x]", arrayExpr)
]

testErrorCases : IO ()
testErrorCases = testGroup "Error cases" $ map (\s => assertCond (parse s) (\res => case res of
  Left _ => True
  Right _ => False)) [ "42 42" ,"" ,"(" ,")" ]

export
testParser : IO ()
testParser = do
  testPrefixOps
  testInfixOps
  testDExprConstructors
  testErrorCases
