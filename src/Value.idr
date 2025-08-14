module Value

import Ast
import Utils

public export
data Value = VBool Bool | VNum Double | VArray (List Value) | VLambda (List String) DExpr

export
partial
implementation Show Value where
  show (VBool b) = show b
  show (VNum d) = show d
  show (VArray xs) = "[" ++ joined "," (map show xs) ++ "]"
  show (VLambda args body) = "λ" ++ joined "." args ++ "." ++ show body

public export
BuiltinFunction : Type
BuiltinFunction = List Value -> IO $ OpResult Value

public export
BuiltinWithLambdaFunction : Type
BuiltinWithLambdaFunction = (Value -> List Value -> IO $ OpResult Value) -> List Value -> IO $ OpResult Value

export
partial
implementation Eq Value where
  (VBool a) == (VBool b) = a == b
  (VNum a) == (VNum b) = a == b
  (VArray as) == (VArray bs) = as == bs
  (VLambda a b) == (VLambda c d) = a == c && b == d
  _ == _ = False

export
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

export
isIntegralDouble : Double -> Bool
isIntegralDouble d = floor d == d

export
doubleToInt : Double -> Int
doubleToInt d = cast {to = Int} (floor d)

export
extractNumber : Value -> OpResult Double
extractNumber (VNum d) = Right d
extractNumber v@(VArray _) =
  case isNonEmptyPureNumArraySum v of
    Just s => Right s
    Nothing => Left $ "Expects non-empty numeric array or number for arithmetic but got: " ++ show v
extractNumber v = Left $ "Expects number for arithmetic but got: " ++ show v

export
extractBool : Value -> OpResult Bool
extractBool (VBool b) = Right b
extractBool v = Left $ "Expects boolean but got: " ++ show v

export
extractArray : Value -> OpResult $ List Value
extractArray (VArray xs) = Right xs
extractArray v = Left $ "Expects array but got " ++ show v

public export
Env : Type
Env = List (String, Value)
