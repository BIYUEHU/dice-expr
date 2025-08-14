module Evaluator

import Data.List
import Value
import Ast
import Utils
import Random
import Internal

range : Int -> Int -> List Int
range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

subst : Env -> DExpr -> DExpr
subst env (DIdent name) = case lookup name env of
  Just val => v2e val
  Nothing => DIdent name
  where
    v2e : Value -> DExpr
    v2e v = case v of
      VNum n => DDLiteral $ DNumber n
      VBool b => DDLiteral $ DBool b
      VArray a => DDLiteral $ DArray $ map v2e a
      VLambda args body => DLambda args body
subst env (DInfix l op r) = DInfix (subst env l) op (subst env r)
subst env (DPrefix op e) = DPrefix op $ subst env e
subst env (DCall head args) = DCall head $ map (subst env) args
subst env (DLambda args body) = DLambda args $ subst env body
subst env (DParen e) = DParen $ subst env e
subst env lit@(DDLiteral _) = lit

mutual
  evalLiteral : DLiteral -> IO $ OpResult Value
  evalLiteral (DBool b) = pure $ Right $ VBool b
  evalLiteral (DNumber d) = pure $ Right $ VNum d
  evalLiteral (DArray elems) = do
    results <- traverse evaluate elems
    pure $ do
      vals <- sequence results
      Right (VArray vals)

  evalPrefix : DPrefixOp -> Value -> OpResult Value
  evalPrefix DNeg v =
    case extractNumber v of
      Left e => Left e
      Right n => Right $ VNum $ - n
  evalPrefix DNot v =
    case extractBool v of
      Left e => Left e
      Right b => Right $ VBool $ not b

  evalInfix : DInfixOp -> Value -> Value -> IO $ OpResult Value
  evalInfix DAdd a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VNum $ x + y

  evalInfix DSub a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VNum $ x - y

  evalInfix DMul a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VNum $ x * y

  evalInfix DDiv a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    if y == 0.0 then Left "Division by zero"
      else Right $ VNum $ x / y

  evalInfix DMod a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    if y == 0.0 then Left "Modulo by zero"
      else
      if isIntegralDouble x && isIntegralDouble y then
        let xi = doubleToInt x
            yi = doubleToInt y in
        Right $ VNum $ cast $ xi `mod` yi
      else
        Left "Modulo requires integer operands"

  evalInfix DPow a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VNum $ pow x y

  evalInfix DEq a b = pure $ Right $ VBool $ a == b
  evalInfix DNeq a b = pure $ Right $ VBool $ not $ a == b

  evalInfix DGt a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VBool $ x > y

  evalInfix DGte a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VBool $ x >= y

  evalInfix DLt a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VBool $ x < y

  evalInfix DLte a b = pure $ do
    x <- extractNumber a
    y <- extractNumber b
    Right $ VBool $ x <= y

  evalInfix DAnd a b = pure $ do
    x <- extractBool a
    y <- extractBool b
    Right $ VBool $ x && y

  evalInfix DOr a b = pure $ do
    x <- extractBool a
    y <- extractBool b
    Right $ VBool $ x || y

  evalInfix DDice a b = do
    case (extractNumber a, extractNumber b) of
      (Left ea, _) => pure $ Left ea
      (_, Left eb) => pure $ Left eb
      (Right xa, Right yb) =>
        if not (isIntegralDouble xa) || xa < 0 then pure $ Left "Left operand of dice must be a natural number"
        else if not (isIntegralDouble yb) || yb <= 1 then pure $ Left "Right operand of dice must be an integer > 1"
        else
          let
              xi = cast xa
              yi = doubleToInt yb in
          if xi == 0 then pure $ Right (VArray [])
          else do
            nums <- traverse (\_ => randomInt 1 yi) (replicate (cast xi) ())
            pure $ Right $  VArray $ map (VNum . cast) nums

  evalInfix DRange a b = do
    case (extractNumber a, extractNumber b) of
      (Left ea, _) => pure $ Left ea
      (_, Left eb) => pure $ Left eb
      (Right xa, Right yb) =>
        if not (isIntegralDouble xa) || not (isIntegralDouble yb) then pure $ Left "Both operands of range must be integers"
        else
          let xi = doubleToInt xa
              yi = doubleToInt yb in
          if yi < xi then pure $ Right (VArray [])
          else
            let ints = map (\n => VNum (cast n)) (range xi yi)
            in pure $ Right $ VArray ints

  evalInfix DRandom a b = do
    case (extractNumber a, extractNumber b) of
      (Left ea, _) => pure $ Left ea
      (_, Left eb) => pure $ Left eb
      (Right xa, Right yb) =>
        if not (isIntegralDouble xa) || not (isIntegralDouble yb) then pure $ Left "Both operands of random must be integer"
        else
          let xi = doubleToInt xa
              yi = doubleToInt yb in
          if xi >= yi then pure $ Left "Left operand of random must be strictly less than right operand"
          else do
            val <- randomInt xi $ yi - 1
            pure $ Right $ VNum $ cast val

  evalInfix DConcat a b =
    case (a, b) of
      (VArray as, VArray bs) => pure $ Right $ VArray $ as ++ bs
      _ => pure $ Left $ "Concat requires both operands to be arrays but got: " ++ show a ++ " and " ++ show b

  evalInfix DColon a b = do
    case b of
      VArray bs => pure $ Right $ VArray $ a :: bs
      _ => pure $ Left $ "Colon requires right operand to be an arra but got: " ++ show b

  -- evalInfix _ _ _ = pure $ Left "Unsupported infix operation or wrong operand types."

  evalCall : DCallHead -> List Value -> IO $ OpResult Value
  evalCall (DIdentHead name) args = case lookup name builtinFunctions of
    Just f => f args
    Nothing => case lookup name builtinWithLambdaFunctions of
      Just f => f evalBuiltinWithLambda args
      Nothing => pure $ Left $ "Undefined function: " ++ name
  evalCall (DLambdaHead formal_args body) args = if length formal_args == length args
    then
      evaluate $ subst (zip formal_args args) body
    else
      pure $ Left $ "Wrong number of arguments for lambda: expected " ++ show (length formal_args) ++ " but got " ++ show (length args)

  evalBuiltinWithLambda : Value -> List Value -> IO $ OpResult Value
  evalBuiltinWithLambda (VLambda args body) args' = evalCall (DLambdaHead args body) args'
  evalBuiltinWithLambda v _ = pure $ Left $ "Expected a lambda but got: " ++ show v

  export
  evaluate : DExpr -> IO $ OpResult Value
  evaluate (DDLiteral lit) = evalLiteral lit
  evaluate (DParen e) = evaluate e
  evaluate (DPrefix op e) = do
    r <- evaluate e
    pure $ r >>= evalPrefix op
  evaluate (DInfix l op r) = do
    rl <- evaluate l
    rr <- evaluate r
    case (rl, rr) of
      (Left err, _) => pure $ Left err
      (_, Left err) => pure $ Left err
      (Right lv, Right rv) => evalInfix op lv rv
  evaluate (DLambda args body) = pure $ Right $ VLambda args body
  evaluate (DCall head args) = do
    args <- traverse evaluate args
    let sequenced = sequence args
    case sequenced of
      Left err => pure $ Left err
      Right vals => evalCall head vals
  evaluate (DIdent _) = pure $ Left "Invalid identifier"
