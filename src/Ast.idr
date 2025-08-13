module Ast

import Utils

public export
data DPrefixOp = DNeg | DNot

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
  | DColon

mutual
  public export
  data DExpr : Type where
    DLambda : List String -> DExpr -> DExpr
    DPrefix : DPrefixOp -> DExpr -> DExpr
    DInfix : DExpr -> DInfixOp -> DExpr -> DExpr
    DCall : DCallHead -> List DExpr -> DExpr
    DDLiteral : DLiteral -> DExpr
    DIdent : String -> DExpr
    DParen : DExpr -> DExpr

  public export
  data DCallHead : Type where
    DLambdaHead : List String -> DExpr -> DCallHead
    DIdentHead : String -> DCallHead


  public export
  data DLiteral : Type where
    DBool : Bool -> DLiteral
    DNumber : Double -> DLiteral
    DArray : List DExpr -> DLiteral

public export
implementation Show DPrefixOp where
  show DNeg = "-"
  show DNot = "!"

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
  show DColon = ":"

public export
implementation Eq DPrefixOp where
  (==) DNeg DNeg = True
  (==) DNot DNot = True
  (==) _ _ = False

public export
implementation Eq DInfixOp where
  (==) DAdd DAdd = True
  (==) DSub DSub = True
  (==) DMul DMul = True
  (==) DDiv DDiv = True
  (==) DMod DMod = True
  (==) DPow DPow = True
  (==) DEq DEq = True
  (==) DNeq DNeq = True
  (==) DGt DGt = True
  (==) DGte DGte = True
  (==) DLt DLt = True
  (==) DLte DLte = True
  (==) DAnd DAnd = True
  (==) DOr DOr = True
  (==) DDice DDice = True
  (==) DRandom DRandom = True
  (==) DRange DRange = True
  (==) DConcat DConcat = True
  (==) _ _ = False


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
    show (DLambdaHead args body) = "(\\" ++ joined ", " args ++ " -> " ++ show body ++ ")"
    show (DIdentHead name) = name

  partial
  public export
  implementation Show DExpr where
    show (DLambda args body) = "(\\" ++ joined ", " args ++ " -> " ++ show body ++ ")"
    show (DPrefix op e) = show op ++ show e
    show (DInfix l op r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show (DCall f args) = show f ++ "(" ++ joined ", " (map show args) ++ ")"
    show (DDLiteral lit) = show lit
    show (DIdent name) = name
    show (DParen e) = "(" ++ show e ++ ")"

  partial
  public export
  implementation Eq DLiteral where
    (==) (DBool b1) (DBool b2) = b1 == b2
    (==) (DNumber n1) (DNumber n2) = n1 == n2
    (==) (DArray xs1) (DArray xs2) = xs1 == xs2

  partial
  public export
  implementation Eq DCallHead where
    (==) (DLambdaHead args1 body1) (DLambdaHead args2 body2) = args1 == args2 && body1 == body2
    (==) (DIdentHead name1) (DIdentHead name2) = name1 == name2
    (==) _ _ = False

  partial
  public export
  implementation Eq DExpr where
    (==) (DLambda args1 body1) (DLambda args2 body2) = args1 == args2 && body1 == body2
    (==) (DPrefix op1 e1) (DPrefix op2 e2) = op1 == op2 && e1 == e2
    (==) (DInfix l1 op1 r1) (DInfix l2 op2 r2) = op1 == op2 && l1 == l2 && r1 == r2
    (==) (DCall f1 args1) (DCall f2 args2) = f1 == f2 && args1 == args2
    (==) (DDLiteral lit1) (DDLiteral lit2) = lit1 == lit2
    (==) (DIdent name1) (DIdent name2) = name1 == name2
    (==) (DParen e1) (DParen e2) = e1 == e2
    (==) _ _ = False
