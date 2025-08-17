module Internal

import Data.List
import Ast
import Utils
import Value
import Random

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

Isin, Icos, Itan, Icot, Isec, Icsc, Iasin, Iacos, Iatan, Iacosh, Iasinh, Iatanh,
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
Iacosh = singleNum (\x => log (x + sqrt (x * x - 1.0)))
Iasinh = singleNum (\x => log (x + sqrt (x * x + 1.0)))
Iatanh = singleNum (\x => 0.5 * log ((1.0 + x) / (1.0 - x)))
Icoth = singleNum (\x => 1 / tanh x)
Isech = singleNum (\x => 1 / cosh x)
Icsch = singleNum (\x => 1 / sinh x)
Ilog = singleNum log
Iexp = singleNum exp
Iabs = singleNum abs
Icbrt = singleNum (\x => pow x (1.0/3.0))
Iceil = singleNum ceiling
Isqrt = singleNum sqrt
Ifloor = singleNum floor
Itrunc = singleNum (\x => cast {to=Double} (cast {to=Integer} x))
Iround = singleNum (\x => cast {to=Double} (cast {to=Integer} (x + 0.5)))
Ifround = singleNum (\x => cast {to=Double} (cast {to=Double} x))

Imax : BuiltinFunction
Imax args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right (x::xs) => pure $ Right $ VNum (foldl max x xs)

Imin : BuiltinFunction
Imin args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right (x::xs) => pure $ Right $ VNum (foldl min x xs)

Ihypot : BuiltinFunction
Ihypot args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right xs => pure $ Right $ VNum (sqrt (sum (map (\x => x * x) xs)))

Iimul : BuiltinFunction
Iimul [VNum x, VNum y] =
  let ix = cast {to=Integer} x
      iy = cast {to=Integer} y
  in pure $ Right $ VNum (cast (ix * iy))
Iimul [x, y] = pure $ Left $ "Expects two numbers but got " ++ show x ++ " and " ++ show y
Iimul xs = pure $ Left $ "Expects two numbers but got " ++ show (length xs) ++ " arguments"

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
  Right [_] => pure $ Right $ VBool True
  Right xs => pure $ Right $ VBool (all (\(a,b) => a < b) (adjacentPairs xs))

Igt args = case typeCheckAll extractNumber args of
  Left err => pure $ Left err
  Right [] => pure $ Left "Expects at least one number"
  Right [_] => pure $ Right $ VBool True
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

Iunion : BuiltinFunction
Iunion [VArray xs, VArray ys] =
  pure $ Right $ VArray (xs ++ filter (\y => not (elem y xs)) ys)
Iunion [x, y] = pure $ Left $ "Expects two arrays but got " ++ show x ++ " and " ++ show y
Iunion xs = pure $ Left $ "Expects two arrays but got " ++ show (length xs) ++ " arguments"

Idifference : BuiltinFunction
Idifference [VArray xs, VArray ys] =
  pure $ Right $ VArray (filter (\x => not (elem x ys)) xs)
Idifference [x, y] = pure $ Left $ "Expects two arrays but got " ++ show x ++ " and " ++ show y
Idifference xs = pure $ Left $ "Expects two arrays but got " ++ show (length xs) ++ " arguments"

Iintersection : BuiltinFunction
Iintersection [VArray xs, VArray ys] =
  pure $ Right $ VArray (filter (\x => elem x ys) xs)
Iintersection [x, y] = pure $ Left $ "Expects two arrays but got " ++ show x ++ " and " ++ show y
Iintersection xs = pure $ Left $ "Expects two arrays but got " ++ show (length xs) ++ " arguments"

Icontain : BuiltinFunction
Icontain [VArray xs, VArray ys] =
  pure $ Right $ VBool (all (\y => elem y xs) ys)
Icontain [x, y] = pure $ Left $ "Expects two arrays but got " ++ show x ++ " and " ++ show y
Icontain xs = pure $ Left $ "Expects two arrays but got " ++ show (length xs) ++ " arguments"

Iinclude : BuiltinFunction
Iinclude [VArray xs, el] = pure $ Right $ VBool (elem el xs)
Iinclude [x, _] = pure $ Left $ "Expects an array and an element but got " ++ show x
Iinclude xs = pure $ Left $ "Expects an array and an element but got " ++ show (length xs) ++ " arguments"

Ishuffle : BuiltinFunction
Ishuffle [VArray xs] = do
  shuffled <- shuffleList xs
  pure $ Right $ VArray shuffled
  where
    shuffleList : List Value -> IO (List Value)
    shuffleList [] = pure []
    shuffleList [x] = pure [x]
    shuffleList xs = do
      rand <- prim__random
      let len = cast (length xs)
          idx = cast {to=Integer} (rand * len)
          (before, after) = splitAt (cast idx) xs
      case after of
        [] => shuffleList xs
        (y::ys) => do
          rest <- shuffleList (before ++ ys)
          pure (y :: rest)
Ishuffle [x] = pure $ Left $ "Expects an array but got " ++ show x
Ishuffle xs = pure $ Left $ "Expects an array but got " ++ show (length xs) ++ " arguments"

Ipick : BuiltinFunction
Ipick [VArray xs, VNum count] = do
  let n = cast count
  if n <= 0 then pure $ Right $ VArray []
    else if n >= length xs then pure $ Right $ VArray xs
    else do
      picked <- pickRandom (cast n) xs []
      pure $ Right $ VArray picked
  where
    pickRandom : Integer -> List Value -> List Value -> IO (List Value)
    pickRandom 0 _ acc = pure acc
    pickRandom _ [] acc = pure acc
    pickRandom n xs acc = do
      rand <- prim__random
      let len = cast (length xs)
          idx = cast {to=Integer} (rand * len)
          (before, after) = splitAt (cast idx) xs
      case after of
        [] => pickRandom n xs acc -- 重试
        (y::ys) => pickRandom (n-1) (before ++ ys) (y::acc)
Ipick [x, y] = pure $ Left $ "Expects an array and a number but got " ++ show x ++ " and " ++ show y
Ipick xs = pure $ Left $ "Expects an array and a number but got " ++ show (length xs) ++ " arguments"

Iint : BuiltinFunction
Iint [VNum start, VNum end] = do
  let s = cast {to=Integer} start
      e = cast {to=Integer} end
  if s >= e then pure $ Left "Start must be less than end"
    else do
      rand <- prim__random
      let result = s + cast {to=Integer} (rand * cast (e - s))
      pure $ Right $ VNum (cast result)
Iint [x, y] = pure $ Left $ "Expects two numbers but got " ++ show x ++ " and " ++ show y
Iint xs = pure $ Left $ "Expects two numbers but got " ++ show (length xs) ++ " arguments"

Ireal : BuiltinFunction
Ireal [VNum start, VNum end] = do
  if start >= end then pure $ Left "Start must be less than end"
    else do
      rand <- prim__random
      let result = start + rand * (end - start)
      pure $ Right $ VNum result
Ireal [x, y] = pure $ Left $ "Expects one or two numbers but got " ++ show x ++ " and " ++ show y
Ireal xs = pure $ Left $ "Expects two numbers but got " ++ show (length xs) ++ " arguments"

Ibool : BuiltinFunction
Ibool [VNum prob] = do
  if prob < 0.0 || prob > 1.0
    then pure $ Left "Probability must be between 0 and 1"
    else do
      rand <- prim__random
      pure $ Right $ VBool (rand < prob)
Ibool [x] = pure $ Left $ "Expects a number between 0 and 1 but got " ++ show x
Ibool xs = pure $ Left $ "Expects a probability number but got " ++ show (length xs) ++ " arguments"

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
    , ("acosh", Iacosh)
    , ("asinh", Iasinh)
    , ("atanh", Iatanh)
    , ("cbrt", Icbrt)
    , ("sqrt", Isqrt)
    , ("ceil", Iceil)
    , ("floor", Ifloor)
    , ("round", Iround)
    , ("abs", Iabs)
    , ("trunc", Itrunc)
    , ("fround", Ifround)
    , ("hypot", Ihypot)
    , ("imul", Imul)
    , ("max", Imax)
    , ("min", Imin)
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
    , ("union", Iunion)
    , ("intersection", Iintersection)
    , ("difference", Idifference)
    , ("contain", Icontain)
    , ("include", Iinclude)
    , ("shuffle", Ishuffle)
    , ("pick", Ipick)
    , ("int", Iint)
    , ("real", Ireal)
    , ("bool", Ibool)
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
