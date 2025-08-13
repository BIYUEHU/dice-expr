module Evaluator

import Data.List
import Data.Either
import Ast
import Utils
import Prelude
import Random

public export
data Value = VBool Bool | VNum Double | VArray (List Value)

partial
export
implementation Show Value where
  show (VBool b) = show b
  show (VNum d) = show d
  show (VArray xs) = "[" ++ joined "," (map show xs) ++ "]"

partial
export
implementation Eq Value where
  (VBool a) == (VBool b) = a == b
  (VNum a) == (VNum b) = a == b
  (VArray as) == (VArray bs) = as == bs
  _ == _ = False

isNonEmptyPureNumArraySum : Value -> Maybe Double
isNonEmptyPureNumArraySum (VArray []) = Nothing
isNonEmptyPureNumArraySum (VArray lst) =
  let
    go : List Value -> Maybe Double
    go [] = Just 0.0
    go (VNum x :: xs) =
      case go xs of
        Just s => Just (s + x)
        Nothing => Nothing
    go (_ :: _) = Nothing
  in
    case go lst of
      Just x => Just x
      Nothing => Nothing
isNonEmptyPureNumArraySum _ = Nothing

isIntegralDouble : Double -> Bool
isIntegralDouble d = floor d == d

doubleToInt : Double -> Int
doubleToInt d = cast {to = Int} (floor d)

valueToNumberOrErr : Value -> Either String Double
valueToNumberOrErr (VNum d) = Right d
valueToNumberOrErr v@(VArray _) =
  case isNonEmptyPureNumArraySum v of
    Just s => Right s
    Nothing => Left $ "Expected non-empty numeric array or number for arithmetic but got: " ++ show v
valueToNumberOrErr v = Left $ "Expected number for arithmetic but got: " ++ show v

valueToBoolOrErr : Value -> Either String Bool
valueToBoolOrErr (VBool b) = Right b
valueToBoolOrErr v = Left $ "Expected boolean but got: " ++ show v

range : Int -> Int -> List Int
range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

mutual
  evalLiteral : DLiteral -> IO (Either String Value)
  evalLiteral (DBool b) = pure $ Right (VBool b)
  evalLiteral (DNumber d) = pure $ Right (VNum d)
  evalLiteral (DArray elems) = f [] elems
    where
      f : List Value -> List DExpr -> IO (Either String Value)
      f vs [] = pure $ Right (VArray vs)
      f vs (x :: xs) = do
        r <- evaluate x
        case r of
          Left err => pure $ Left err
          Right v => f (vs ++ [v]) xs

  evalPrefix : DPrefixOp -> Value -> Either String Value
  evalPrefix DNeg v =
    case valueToNumberOrErr v of
      Left e => Left e
      Right n => Right $ VNum ( - n )
  evalPrefix DNot v =
    case valueToBoolOrErr v of
      Left e => Left e
      Right b => Right $ VBool (not b)

  evalInfix : DInfixOp -> Value -> Value -> IO (Either String Value)
  evalInfix DAdd a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VNum $ x + y

  evalInfix DSub a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VNum $ x - y

  evalInfix DMul a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VNum $ x * y

  evalInfix DDiv a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    if y == 0.0 then Left "Division by zero."
      else Right $ VNum $ x / y

  evalInfix DMod a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    if y == 0.0 then Left "Modulo by zero."
      else
      if isIntegralDouble x && isIntegralDouble y then
        let xi = doubleToInt x
            yi = doubleToInt y in
        Right $ VNum $ cast (xi `mod` yi)
      else
        Left "Modulo requires integer operands."

  evalInfix DPow a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VNum $ pow x y

  evalInfix DEq a b = pure $ Right $ VBool (a == b)
  evalInfix DNeq a b = pure $ Right $ VBool (not (a == b))

  -- 数值比较（不做隐式数组求和；若想支持可改成使用 valueToNumberOrErr）
  evalInfix DGt a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VBool $ x > y

  evalInfix DGte a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VBool $ x >= y

  evalInfix DLt a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VBool $ x < y

  evalInfix DLte a b = pure $ do
    x <- valueToNumberOrErr a
    y <- valueToNumberOrErr b
    Right $ VBool $ x <= y

  evalInfix DAnd a b = pure $ do
    x <- valueToBoolOrErr a
    y <- valueToBoolOrErr b
    Right $ VBool $ x && y

  evalInfix DOr a b = pure $ do
    x <- valueToBoolOrErr a
    y <- valueToBoolOrErr b
    Right $ VBool $ x || y

  evalInfix DDice a b = do
    case (valueToNumberOrErr a, valueToNumberOrErr b) of
      (Left ea, _) => pure $ Left ea
      (_, Left eb) => pure $ Left eb
      (Right xa, Right yb) =>
        if not (isIntegralDouble xa) then pure $ Left "Left operand of DDice must be an integer (natural)."
        else if xa < 0 then pure $ Left "Left operand of DDice must be non-negative."
        else if not (isIntegralDouble yb) then pure $ Left "Right operand of DDice must be an integer > 1."
        else
          let
              xi = cast xa
              yi = doubleToInt yb in
          if yi <= 1 then pure $ Left "Right operand of DDice must be an integer greater than 1."
          else if xi == 0 then pure $ Right (VArray [])
          else do
            nums <- traverse (\_ => randomInt 1 yi) (replicate (cast xi) ())
            pure $ Right $  VArray $ map (VNum . cast) nums

  evalInfix DRange a b = do
    case (valueToNumberOrErr a, valueToNumberOrErr b) of
      (Left ea, _) => pure $ Left ea
      (_, Left eb) => pure $ Left eb
      (Right xa, Right yb) =>
        if not (isIntegralDouble xa) || not (isIntegralDouble yb) then pure $ Left "Both operands of DRange must be integers."
        else
          let xi = doubleToInt xa
              yi = doubleToInt yb in
          if yi < xi then pure $ Right (VArray [])
          else
            let ints = map (\n => VNum (cast n)) (range xi yi)
            in pure $ Right $ VArray ints

  evalInfix DRandom a b = do
    case (valueToNumberOrErr a, valueToNumberOrErr b) of
      (Left ea, _) => pure $ Left ea
      (_, Left eb) => pure $ Left eb
      (Right xa, Right yb) =>
        if not (isIntegralDouble xa) || not (isIntegralDouble yb) then pure $ Left "Both operands of DRandom must be integers."
        else
          let xi = doubleToInt xa
              yi = doubleToInt yb in
          if xi >= yi then pure $ Left "For DRandom, left operand must be strictly less than right operand."
          else do
            val <- randomInt xi $ yi - 1
            pure $ Right $ VNum $ cast val

  evalInfix DConcat a b =
    case (a, b) of
      (VArray as, VArray bs) => pure $ Right $ VArray $ as ++ bs
      _ => pure $ Left $ "DConcat requires both operands to be arrays. Got: " ++ show a ++ " and " ++ show b

  evalInfix DColon a b = do
    case b of
      VArray bs => pure $ Right $ VArray $ a :: bs
      _ => pure $ Left $ "DColon requires right operand to be an array. Got: " ++ show b

  -- evalInfix _ _ _ = pure $ Left "Unsupported infix operation or wrong operand types."

  export
  evaluate : DExpr -> IO (Either String Value)
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
  evaluate (DLambda _ _ ) = pure $ Left "Lambda values not supported at runtime in this DSL evaluator."
  evaluate (DCall _ _) = pure $ Left "Function calls not supported in this DSL evaluator."
  evaluate (DIdent name) = pure $ Left $ "Unbound identifier: " ++ name
