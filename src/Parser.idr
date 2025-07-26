module Parser

import Combinator

public export
data DPrefixOp = DNeg | DAANot

public export
data DInfixOp
  = DAdd
  | DSub
  | DMul
  | DDiv
  | DMod
  | DPow
  | DEq
  | DNeq
  | DGt
  | DGte
  | DLt
  | DLte
  | DAnd
  | DOr
  | DDice
  | DRandom
  | DRange
  | DConcat

mutual
  public export
  data DExpr : Type where
    DLambda : List HString -> DExpr -> DExpr
    DPrefix : DPrefixOp -> DExpr -> DExpr
    DInfix : DExpr -> DInfixOp -> DExpr -> DExpr
    DCall : DCallHead -> List DExpr -> DExpr
    DDLiteral : DLiteral -> DExpr
    DIdent : HString -> DExpr
    DParen : DExpr -> DExpr

  public export
  data DCallHead : Type where
    DLambdaHead : List HString -> DExpr -> DCallHead
    DIdentHead : HString -> DCallHead


  public export
  data DLiteral : Type where
    DBool : Bool -> DLiteral
    DNumber : Double -> DLiteral
    DArray : List DExpr -> DLiteral

public export
implementation Show DPrefixOp where
  show DNeg = "-"
  show DAANot = "!"

public export
implementation Show DInfixOp where
  show DAdd = "+"
  show DSub = "-"
  show DMul = "*"
  show DDiv = "/"
  show DMod = "%"
  show DPow = "^"
  show DEq = "=="
  show DNeq = "!="
  show DGt = ">"
  show DGte = ">="
  show DLt = "<"
  show DLte = "<="
  show DAnd = "&&"
  show DOr = "||"
  show DDice = "d"
  show DRandom = "~"
  show DRange = ".."
  show DConcat = "++"

joined : String -> List String -> String
joined _ [] = ""
joined _ [x] = x
joined sep (x :: xs) = x ++ sep ++ joined sep xs

mutual
  partial
  public export
  implementation Show DLiteral where
    show (DBool b) = show b
    show (DNumber n) = show n
    show (DArray xs) = "[" ++ joined ", " (map show xs) ++ "]"

  partial
  public export
  implementation Show DCallHead where
    show (DLambdaHead args body) = "(\\" ++ joined ", " (map pack args) ++ " -> " ++ show body ++ ")"
    show (DIdentHead name) = pack name


  partial
  public export
  implementation Show DExpr where
    show (DLambda args body) = "(\\" ++ joined ", " (map pack args) ++ " -> " ++ show body ++ ")"
    show (DPrefix op e) = show op ++ show e
    show (DInfix l op r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show (DCall f args) = show f ++ "(" ++ joined ", " (map show args) ++ ")"
    show (DDLiteral lit) = show lit
    show (DIdent name) = pack name
    show (DParen e) = "(" ++ show e ++ ")"

ssymbol : String -> Parser HString
ssymbol = symbol . unpack

sepBy1 : Parser a -> Parser b -> Parser (List a)
sepBy1 p sep = p <**> many (sep *> p)

sepBy : Parser a -> Parser b -> Parser (List a)
sepBy p sep = sepBy1 p sep <|> empty

chainl1 : Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p <*> many (op <*> p) <&> \(x, pairs) => foldl (\acc, (f, y) => f acc y) x pairs

ident : Parser HString
ident = some (satisfy isAlpha) <* spaces

bool : Parser Bool
bool = (ssymbol "true" &> True) <|> 
       (ssymbol "false" &> False)

paramList : Parser (List HString)
paramList = ident `sepBy1` ssymbol ","

prefixOp : Parser DPrefixOp
prefixOp = (ssymbol "-" &> DNeg) <|> (ssymbol "!" &> DAANot)

infixOp : Parser DInfixOp
infixOp = 
  let x = (ssymbol "++" &> DConcat) <|>
          (ssymbol "&&" &> DAnd) <|>
          (ssymbol "||" &> DOr) <|>
          (ssymbol "==" &> DEq) in
  let x = x <|>
          (ssymbol "!=" &> DNeq) <|>
          (ssymbol ">=" &> DGte) <|>
          (ssymbol "<=" &> DLte) in
  let x = x <|>
          (ssymbol ">" &> DGt) <|>
          (ssymbol "<" &> DLt) <|>
          (ssymbol "+" &> DAdd) in
  let x = x <|>
          (ssymbol "-" &> DSub) <|>
          (ssymbol "*" &> DMul) <|>
          (ssymbol "/" &> DDiv) in
  let x = x <|>
          (ssymbol "%" &> DMod) <|>
          (ssymbol "^" &> DPow) <|>
          (ssymbol "d" &> DDice) in
  x <|>
  (ssymbol "~" &> DRandom) <|>
  (ssymbol ".." &> DRange)

mutual
  expr : Parser DExpr
  expr =
    let x = lambda <|> infixExpr <|> callExpr in
    let x = x <|> (literal <&> DDLiteral)
    in x

  literal : Parser DLiteral
  literal = (bool <&> DBool) <|>
            (float <&> DNumber) <|>
            (integer <&> (DNumber . cast)) <|>
            arrayLit
    where
      arrayLit : Parser DLiteral
      arrayLit =
        empty <|> filled
        where 
          empty = ssymbol "[" *> ssymbol "]" &> DArray []
          filled = (ssymbol "[" *>
                   (expr `sepBy1` ssymbol ",") <*
                   ssymbol "]" <&> DArray)

  lambda : Parser DExpr  
  lambda = 
    let x = ssymbol "\\" *> paramList <* ssymbol "->" in
    x <*> expr <&> \(params, body) => DLambda params body
  
  infixExpr : Parser DExpr
  infixExpr = chainl1 prefixExpr (infixOp <&> \op => \l, r => DInfix l op r)
  
  prefixExpr : Parser DExpr
  prefixExpr = (prefixOp <*> atom <&> \(op, e) => DPrefix op e) <|> callExpr
  
  callExpr : Parser DExpr
  callExpr = 
    let x = many (ssymbol "(" *> (expr `sepBy` ssymbol ",") <* ssymbol ")") in
    let x = callHead <*> x <&>
             \(head, argsList) => case argsList of
               [] => case head of
                 DLambdaHead params body => DLambda params body
                 DIdentHead name => DIdent name
               (args :: _) => DCall head args
    in x
  
  callHead : Parser DCallHead
  callHead = lambdaHead <|> identHead
    where
      lambdaHead : Parser DCallHead
      lambdaHead = 
        let x = ssymbol "\\" *> paramList <* ssymbol "->" in
        x <*> expr <&> \(params, body) => DLambdaHead params body
      
      identHead : Parser DCallHead
      identHead = ident <&> DIdentHead
  
  atom : Parser DExpr
  atom = literalExpr <|> identExpr <|> parenExpr
    where
      literalExpr : Parser DExpr
      literalExpr = literal <&> DDLiteral
      
      identExpr : Parser DExpr
      identExpr = ident <&> DIdent
      
      parenExpr : Parser DExpr
      parenExpr = ssymbol "(" *> expr <* ssymbol ")" <&> DParen

public export
program : Parser DExpr
program = spaces *> expr

public export
parseDice : String -> Either String DExpr
parseDice input = case expr $ unpack input of
  Left err => Left err
  Right (result, []) => Right result
  Right (_, remaining) => Left $ "Unexpected remaining input"
