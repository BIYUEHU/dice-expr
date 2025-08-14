module Parser

import Lexer
import Data.List
import Token
import Ast
import Utils

export
record ParseState where
  constructor MkParseState
  lexState : LexState
  current : Token

export
initParseState : String -> OpResult ParseState
initParseState input = do
  let lexState = initLexState input
  (firstToken, newLexState) <- nextToken lexState
  Right (MkParseState newLexState firstToken)

advance : ParseState -> OpResult ParseState
advance state = do
  (token, newLexState) <- nextToken state.lexState
  Right (MkParseState newLexState token)

expect : Token -> ParseState -> OpResult ParseState
expect expected state =
  if state.current == expected
  then advance state
  else Left ("Expected " ++ show expected ++ " but got " ++ show state.current)

tokenToInfixOp : Token -> OpResult DInfixOp
tokenToInfixOp TPlus = Right DAdd
tokenToInfixOp TMinus = Right DSub
tokenToInfixOp TMul = Right DMul
tokenToInfixOp TDiv = Right DDiv
tokenToInfixOp TMod = Right DMod
tokenToInfixOp TPow = Right DPow
tokenToInfixOp TEq = Right DEq
tokenToInfixOp TNeq = Right DNeq
tokenToInfixOp TGt = Right DGt
tokenToInfixOp TGte = Right DGte
tokenToInfixOp TLt = Right DLt
tokenToInfixOp TLte = Right DLte
tokenToInfixOp TAnd = Right DAnd
tokenToInfixOp TOr = Right DOr
tokenToInfixOp TDice = Right DDice
tokenToInfixOp TRandom = Right DRandom
tokenToInfixOp TRange = Right DRange
tokenToInfixOp TConcat = Right DConcat
tokenToInfixOp TColon = Right DColon
tokenToInfixOp t = Left $ "Not an infix operator: " ++ show t


mutual
  parseExpr : ParseState -> OpResult (DExpr, ParseState)
  parseExpr = parseInfixExpr 0

  parseInfixExpr : Nat -> ParseState -> OpResult (DExpr, ParseState)
  parseInfixExpr minPrec state = do
    (left, state') <- parsePrimaryExpr state
    parseInfixExprHelper minPrec left state'

  parseInfixExprHelper : Nat -> DExpr -> ParseState -> OpResult (DExpr, ParseState)
  parseInfixExprHelper minPrec left state =
    if isInfixOp state.current then
      let prec = precedence state.current
      in if prec >= minPrec then do
        op <- tokenToInfixOp state.current
        state' <- advance state
        (right, state'') <- parseInfixExpr (prec + 1) state'
        let newExpr = DInfix left op right
        parseInfixExprHelper minPrec newExpr state''
      else
        Right (left, state)
    else
      Right (left, state)

  parsePrimaryExpr : ParseState -> OpResult (DExpr, ParseState)
  parsePrimaryExpr state = do
    (baseExpr, state') <- parseAtomicExpr state
    parseCallChain baseExpr state'
  
  -- 新增：解析原子表达式（不包括函数调用）
  parseAtomicExpr : ParseState -> OpResult (DExpr, ParseState)
  parseAtomicExpr state = case state.current of
    TLambda => do
      state' <- advance state
      case state'.current of
        TIdent name => do
          (params, state'') <- parseParamList state'
          state''' <- expect TArrow state''
          (body, state'''') <- parseExpr state'''
          Right (DLambda params body, state'''')
        _ => Left $ "Expected parameter after \\ but got: " ++ show state'.current

    TMinus => do
      state' <- advance state
      (expr, state'') <- parsePrimaryExpr state'
      Right (DPrefix DNeg expr, state'')

    TNot => do
      state' <- advance state
      (expr, state'') <- parsePrimaryExpr state'
      Right (DPrefix DNot expr, state'')

    TLParen => do
      state' <- advance state
      (expr, state'') <- parseExpr state'
      state''' <- expect TRParen state''
      Right (DParen expr, state''')

    TLBracket => do
      state' <- advance state
      (exprs, state'') <- parseExprList state'
      state''' <- expect TRBracket state''
      Right (DDLiteral $ DArray exprs, state''')

    TNumber n => do
      state' <- advance state
      Right (DDLiteral $ DNumber n, state')

    TBool b => do
      state' <- advance state
      Right (DDLiteral $ DBool b, state')

    TIdent name => do
      state' <- advance state
      Right (DIdent name, state')

    _ => Left $ "Unexpected token in primary expression: " ++ show state.current

  parseCallChain : DExpr -> ParseState -> OpResult (DExpr, ParseState)
  parseCallChain expr state = case state.current of
    TLParen => do
      state' <- advance state
      (args, state'') <- parseExprList state'
      state''' <- expect TRParen state''
      callExpr <- case expr of
        DIdent name => Right $ DCall (DIdentHead name) args
        DLambda params body => Right $ DCall (DLambdaHead params body) args
        DParen innerExpr => case innerExpr of
          DLambda params body => Right $ DCall (DLambdaHead params body) args
          DIdent name => Right $ DCall (DIdentHead name) args
          _ => Left "Cannot call this expression type"
        _ => Left "Cannot call this expression type"
      parseCallChain callExpr state'''
    _ => Right (expr, state)

  parseParamList : ParseState -> OpResult (List String, ParseState)
  parseParamList state = case state.current of
    TIdent name => do
      state' <- advance state
      case state'.current of
        TComma => do
          state'' <- advance state'
          (rest, state''') <- parseParamList state''
          Right ( name :: rest, state''')
        _ => Right ( [name], state')
    _ => Left $ "Expected parameter name but got: " ++ show state.current

  parseExprList : ParseState -> OpResult (List DExpr, ParseState)
  parseExprList state = case state.current of
    TRBracket => Right ([], state)
    TRParen => Right ([], state)
    _ => do
      (first, state') <- parseExpr state
      parseExprListHelper [first] state'
    where
      parseExprListHelper : List DExpr -> ParseState -> OpResult (List DExpr, ParseState)
      parseExprListHelper acc state = case state.current of
        TComma => do
          state' <- advance state
          (expr, state'') <- parseExpr state'
          parseExprListHelper (expr :: acc) state''
        _ => Right (reverse acc, state)

export
parse : String -> OpResult DExpr
parse input = do
  state <- initParseState input
  (expr, finalState) <- parseExpr state
  case finalState.current of
    TEOF => Right expr
    _ => Left $ "Unexpected token after expression: " ++ show finalState.current
