module ParserCom

import Combinator
import Ast
import Utils

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
prefixOp = (ssymbol "-" &> DNeg) <|> (ssymbol "!" &> DNot)

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
    x <*> expr <&> \(params, body) => DLambda (map pack params) body

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
        x <*> expr <&> \(params, body) => DLambdaHead (map pack params) body

      identHead : Parser DCallHead
      identHead = ?mapident Combinator.(<&>) DIdentHead
        where
          x22 = ident

  atom : Parser DExpr
  atom = literalExpr <|> identExpr <|> parenExpr
    where
      literalExpr : Parser DExpr
      literalExpr = literal <&> DDLiteral

      identExpr : Parser DExpr
      identExpr = (ident <&> pack) <&> DIdent

      parenExpr : Parser DExpr
      parenExpr = ssymbol "(" *> expr <* ssymbol ")" <&> DParen

public export
program : Parser DExpr
program = spaces *> expr

public export
parseDice : String -> OpResult DExpr
parseDice input = case expr $ unpack input of
  Left err => Left err
  Right (result, []) => Right result
  Right (_, remaining) => Left $ "Unexpected remaining input"
