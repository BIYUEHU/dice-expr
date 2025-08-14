module Token

public export
data Token
  = TLambda
  | TArrow
  | TComma
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TIdent String
  | TNumber Double
  | TBool Bool
  | TPlus
  | TMinus
  | TMul
  | TDiv
  | TMod
  | TPow
  | TEq
  | TNeq
  | TGt
  | TGte
  | TLt
  | TLte
  | TAnd
  | TOr
  | TDice
  | TRandom
  | TRange
  | TConcat
  | TColon
  | TNot
  | TEOF

public export
implementation Show Token where
  show TLambda = "\\"
  show TArrow = "->"
  show TComma = ","
  show TLParen = "("
  show TRParen = ")"
  show TLBracket = "["
  show TRBracket = "]"
  show (TIdent name) = name
  show (TNumber num) = show num
  show (TBool b) = show b
  show TPlus = "+"
  show TMinus = "-"
  show TMul = "*"
  show TDiv = "/"
  show TMod = "%"
  show TPow = "^"
  show TEq = "=="
  show TNeq = "!="
  show TGt = ">"
  show TGte = ">="
  show TLt = "<"
  show TLte = "<="
  show TAnd = "&&"
  show TOr = "||"
  show TDice = "d"
  show TRandom = "~"
  show TRange = ".."
  show TConcat = "++"
  show TColon = ":"
  show TNot = "!"
  show TEOF = "EOF"

public export
implementation Eq Token where
  TLambda == TLambda = True
  TArrow == TArrow = True
  TComma == TComma = True
  TLParen == TLParen = True
  TRParen == TRParen = True
  TLBracket == TLBracket = True
  TRBracket == TRBracket = True
  (TIdent a) == (TIdent b) = a == b
  (TNumber a) == (TNumber b) = a == b
  (TBool a) == (TBool b) = a == b
  TPlus == TPlus = True
  TMinus == TMinus = True
  TMul == TMul = True
  TDiv == TDiv = True
  TMod == TMod = True
  TPow == TPow = True
  TEq == TEq = True
  TNeq == TNeq = True
  TGt == TGt = True
  TGte == TGte = True
  TLt == TLt = True
  TLte == TLte = True
  TAnd == TAnd = True
  TOr == TOr = True
  TDice == TDice = True
  TRandom == TRandom = True
  TRange == TRange = True
  TConcat == TConcat = True
  TColon == TColon = True
  TNot == TNot = True
  TEOF == TEOF = True
  _ == _ = False

public export
precedence : Token -> Nat
precedence TOr = 1
precedence TAnd = 2
precedence TEq = 3
precedence TNeq = 3
precedence TGt = 4
precedence TGte = 4
precedence TLt = 4
precedence TLte = 4
precedence TConcat = 5
precedence TColon = 5
precedence TPlus = 6
precedence TMinus = 6
precedence TMul = 7
precedence TDiv = 7
precedence TMod = 7
precedence TRange = 8
precedence TDice = 8
precedence TRandom = 8
precedence TPow = 9
precedence _ = 0

public export
isInfixOp : Token -> Bool
isInfixOp TPlus = True
isInfixOp TMinus = True
isInfixOp TMul = True
isInfixOp TDiv = True
isInfixOp TMod = True
isInfixOp TPow = True
isInfixOp TEq = True
isInfixOp TNeq = True
isInfixOp TGt = True
isInfixOp TGte = True
isInfixOp TLt = True
isInfixOp TLte = True
isInfixOp TAnd = True
isInfixOp TOr = True
isInfixOp TDice = True
isInfixOp TRandom = True
isInfixOp TRange = True
isInfixOp TConcat = True
isInfixOp TColon = True
isInfixOp _ = False
