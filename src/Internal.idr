module Internal

import Data.List
import Ast
import Utils
import Value

typeCheckAll : (Value -> OpResult a) -> List Value -> OpResult (List a)
typeCheckAll f xs = traverse f xs

adjacentPairs : List a -> List (a, a)
adjacentPairs [] = []
adjacentPairs [_] = []
adjacentPairs (x::y::rest) = (x,y) :: adjacentPairs (y::rest)

Iadd, Imul : BuiltinFunction
Iadd args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right nums => pure $ Right $ VNum $ sum nums

Imul args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right nums => pure $ Right $ VNum $ product nums

Isub, Idiv, Ipow : BuiltinFunction
Isub args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right (x::xs) => pure $ Right $ VNum $ foldl (-) x xs
  Right [] => pure $ Left "Expects at least one number"

Idiv args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right (x::xs) => pure $ Right $ VNum $ foldl (/) x xs
  Right [] => pure $ Left "Expects at least one number"

Ipow args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right (x::xs) => pure $ Right $ VNum $ foldl pow x xs
  Right [] => pure $ Left "Expects at least one number"

Isin, Icos, Itan, Icot, Isec, Icsc, Iasin, Iacos, Iatan,
Isinh, Icosh, Itanh, Icoth, Isech, Icsch, Ilog, Iexp : BuiltinFunction

singleNum : (Double -> Double) -> List Value -> IO $ OpResult Value
singleNum f [VNum x] = pure $ Right $ VNum (f x)
singleNum f [x] = pure $ Left $ "Expects a number but got " ++ show x
singleNum f xs = pure $ Left $ "Expects one number but got " ++ show (length xs)

Isin = singleNum sin
Icos = singleNum cos
Itan = singleNum tan
Icot = singleNum (\x => 1 / tan x)
Isec = singleNum (\x => 1 / cos x)
Icsc = singleNum (\x => 1 / sin x)
Iasin = singleNum asin
Iacos = singleNum acos
Iatan = singleNum atan
Isinh = singleNum sinh
Icosh = singleNum cosh
Itanh = singleNum tanh
Icoth = singleNum (\x => 1 / tanh x)
Isech = singleNum (\x => 1 / cosh x)
Icsch = singleNum (\x => 1 / sinh x)
Ilog = singleNum log
Iexp = singleNum exp

Iand, Ior : BuiltinFunction
Iand args = case typeCheckAll extractBool args of
  Left err => pure $ Left err
  Right bs => pure $ Right $ VBool (all id bs)

Ior args = case typeCheckAll extractBool args of
  Left err => pure $ Left err
  Right bs => pure $ Right $ VBool (any id bs)

Inot : BuiltinFunction
Inot [VBool b] = pure $ Right $ VBool (not b)
Inot [x] = pure $ Left $ "Expects a booleanbut got " ++ show x
Inot xs = pure $ Left $ "Expects one booleanbut got " ++ show (length xs)

Ieq, Ineq, Ilt, Igt : BuiltinFunction
Ieq args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right (x::xs) => pure $ Right $ VBool (all (== x) xs)

Ineq args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right (x::xs) => pure $ Right $ VBool (all (/= x) xs)

Ilt args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right [_] => pure $ Right $ VBool True  -- 单元素直接 True
  Right xs => pure $ Right $ VBool (all (\(a,b) => a < b) (adjacentPairs xs))

Igt args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right [_] => pure $ Right $ VBool True  -- 单元素直接 True
  Right xs => pure $ Right $ VBool (all (\(a,b) => a > b) (adjacentPairs xs))

Iif : BuiltinFunction
Iif [VBool cond, x, y] = pure $ Right $ if cond then x else y
Iif [cond, x, y] = pure $ Left $ "Expects a boolean and two valuesbut got " ++ show cond ++ ", " ++ show x ++ " and " ++ show y
Iif xs = pure $ Left $ "Expects a boolean and two valuesbut got " ++ show (length xs) ++ " arguments"

Iarray : BuiltinFunction
Iarray xs = pure $ Right $ VArray xs

Ilength : BuiltinFunction
Ilength [VArray xs] = pure $ Right $ VNum (cast (length xs))
Ilength _ = pure $ Left "Expects an array"

Ifill : BuiltinFunction
Ifill [VArray xs, val] = pure $ Right $ VArray (map (const val) xs)
Ifill _ = pure $ Left "Expects an array and a value"

Islice : BuiltinFunction
Islice [VArray xs, VNum start, VNum count] =
  let start = cast start
      count = cast count
  in pure $ Right $  VArray (take count (drop start xs))
Islice _ = pure $ Left "Expects an array and two numbers"

Isum : BuiltinFunction
Isum [VArray xs] = case typeCheckAll extractNumber xs of
    Left e => pure $ Left e
    Right nums => pure $ Right $ VNum (sum nums)
Isum _ = pure $ Left "Expects an array"

public export
builtinFunctions: HashMap String BuiltinFunction
builtinFunctions = [
    ("add", Iadd)
    , ("mul", Imul)
    , ("sub", Isub)
    , ("per", Idiv)
    , ("pow", Ipow)
    , ("sin", Isin)
    , ("cos", Icos)
    , ("tan", Itan)
    , ("cot", Icot)
    , ("sec", Isec)
    , ("csc", Icsc)
    , ("asin", Iasin)
    , ("acos", Iacos)
    , ("atan", Iatan)
    , ("sinh", Isinh)
    , ("cosh", Icosh)
    , ("tanh", Itanh)
    , ("coth", Icoth)
    , ("sech", Isech)
    , ("csch", Icsch)
    , ("log", Ilog)
    , ("exp", Iexp)
    , ("and", Iand)
    , ("or", Ior)
    , ("not", Inot)
    , ("eq", Ieq)
    , ("neq", Ineq)
    , ("lt", Ilt)
    , ("gt", Igt)
    , ("if", Iif)
    , ("array", Iarray)
    , ("length", Ilength)
    , ("fill", Ifill)
    , ("slice", Islice)
    , ("sum", Isum)
]

extractArrayWithLambda : List Value -> OpResult (List Value, Value)
extractArrayWithLambda [arr, lambda] = do
  arr <- extractArray arr
  pure $ (arr, lambda)
extractArrayWithLambda _ = Left "Expects an array and a lambda"

Ievery : BuiltinWithLambdaFunction
Ievery evalFn args = case extractArrayWithLambda args of
  Left e => pure $ Left e
  Right (xs, lambda) => do
    results <- traverse (\x => evalFn lambda [x]) xs
    let sequenced = sequence results
    case sequenced of
      Left e => pure $ Left e
      Right vals => Iand vals

Isome : BuiltinWithLambdaFunction
Isome evalFn args = case extractArrayWithLambda args of
  Left e => pure $ Left e
  Right (xs, lambda) => do
    results <- traverse (\x => evalFn lambda [x]) xs
    let sequenced = sequence results
    case sequenced of
      Left e => pure $ Left e
      Right vals => pure $ Right $ VBool $ any (\v => case v of VBool b => b; _ => False) vals

Imap : BuiltinWithLambdaFunction
Imap evalFn args = case extractArrayWithLambda args of
  Left e => pure $ Left e
  Right (xs, lambda) => do
    results <- traverse (\x => evalFn lambda [x]) xs
    pure $ map VArray (sequence results)

Ifilter : BuiltinWithLambdaFunction
Ifilter evalFn args = case extractArrayWithLambda args of
  Left e => pure $ Left e
  Right (xs, lambda) => do
    results <- traverse (\x => evalFn lambda [x]) xs
    let sequenced = sequence results
    case sequenced of
      Left e => pure $ Left e
      Right vals => pure $ Right $ VArray $
        map fst $ filter (\(v, r) => case r of VBool True => True; _ => False) (zip xs vals)

IflatMap : BuiltinWithLambdaFunction
IflatMap evalFn args = case extractArrayWithLambda args of
  Left e => pure $ Left e
  Right (xs, lambda) => do
    mapped <- Imap evalFn [VArray xs, lambda]
    case mapped of
      Left e => pure $ Left e
      Right (VArray xss) =>
        pure $ Right $ VArray (concatMap (\v => case v of VArray ys => ys; _ => [v]) xss)
      Right _ => pure $ Left "Expects an array"

Ireduce : BuiltinWithLambdaFunction
Ireduce evalFn args = case args of
  [VArray xs, initAcc, lambda] =>
    let
      step : OpResult Value -> Value -> IO (OpResult Value)
      step (Left err) _ = pure $ Left err
      step (Right accVal) x = evalFn lambda [accVal, x]
    in foldlM step (Right initAcc) xs
  _ => pure $ Left "Expects an array, an initial value, and a lambda"

public export
builtinWithLambdaFunctions : HashMap String BuiltinWithLambdaFunction
builtinWithLambdaFunctions = [
  ("every", Ievery)
  , ("some", Isome)
  , ("map", Imap)
  , ("filter", Ifilter)
  , ("flatMap", IflatMap)
  , ("reduce", Ireduce)
]