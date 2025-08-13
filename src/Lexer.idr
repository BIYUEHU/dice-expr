module Lexer

import Data.String
import Token
import Utils

export
record LexState where
  constructor MkLexState
  input : HString
  pos : Nat

export
initLexState : String -> LexState
initLexState str = MkLexState (unpack str) 0

skipWhitespace : LexState -> LexState
skipWhitespace state = case state.input of
  [] => state
  (c :: cs) => if isSpace c
              then skipWhitespace (MkLexState cs (state.pos + 1))
              else state

readNumber : HString -> (Double, HString)
readNumber chars =
  let (intPart, rest) = span isDigit chars
      intStr = pack intPart
  in case rest of
    ('.' :: cs) =>
      let (fracPart, rest') = span isDigit cs
          fracStr = pack fracPart
      in case parseDouble (intStr ++ "." ++ fracStr) of
        Just d => (d, rest')
        Nothing => (0.0, chars)
    _ => case parseDouble intStr of
      Just d => (d, rest)
      Nothing => (0.0, chars)

readIdent : HString -> (String, HString)
readIdent chars =
  let (identChars, rest) = span isAlpha chars
  in (pack identChars, rest)

export
nextToken : LexState -> Either String (Token, LexState)
nextToken state =
  let state' = skipWhitespace state
  in case state'.input of
    [] => Right (TEOF, state')
    ('\\' :: cs) => Right (TLambda, MkLexState cs (state'.pos + 1))
    (',' :: cs) => Right (TComma, MkLexState cs (state'.pos + 1))
    ('(' :: cs) => Right (TLParen, MkLexState cs (state'.pos + 1))
    (')' :: cs) => Right (TRParen, MkLexState cs (state'.pos + 1))
    ('[' :: cs) => Right (TLBracket, MkLexState cs (state'.pos + 1))
    (']' :: cs) => Right (TRBracket, MkLexState cs (state'.pos + 1))

    ('-' :: '>' :: cs) => Right (TArrow, MkLexState cs (state'.pos + 2))
    ('+' :: '+' :: cs) => Right (TConcat, MkLexState cs (state'.pos + 2))
    ('=' :: '=' :: cs) => Right (TEq, MkLexState cs (state'.pos + 2))
    ('!' :: '=' :: cs) => Right (TNeq, MkLexState cs (state'.pos + 2))
    ('>' :: '=' :: cs) => Right (TGte, MkLexState cs (state'.pos + 2))
    ('<' :: '=' :: cs) => Right (TLte, MkLexState cs (state'.pos + 2))
    ('&' :: '&' :: cs) => Right (TAnd, MkLexState cs (state'.pos + 2))
    ('|' :: '|' :: cs) => Right (TOr, MkLexState cs (state'.pos + 2))
    ('.' :: '.' :: cs) => Right (TRange, MkLexState cs (state'.pos + 2))

    ('+' :: cs) => Right (TPlus, MkLexState cs (state'.pos + 1))
    ('-' :: cs) => Right (TMinus, MkLexState cs (state'.pos + 1))
    ('*' :: cs) => Right (TMul, MkLexState cs (state'.pos + 1))
    ('/' :: cs) => Right (TDiv, MkLexState cs (state'.pos + 1))
    ('%' :: cs) => Right (TMod, MkLexState cs (state'.pos + 1))
    ('^' :: cs) => Right (TPow, MkLexState cs (state'.pos + 1))
    ('>' :: cs) => Right (TGt, MkLexState cs (state'.pos + 1))
    ('<' :: cs) => Right (TLt, MkLexState cs (state'.pos + 1))
    ('d' :: cs) => Right (TDice, MkLexState cs (state'.pos + 1))
    ('~' :: cs) => Right (TRandom, MkLexState cs (state'.pos + 1))
    (':' :: cs) => Right (TColon, MkLexState cs (state'.pos + 1))
    ('!' :: cs) => Right (TNot, MkLexState cs (state'.pos + 1))

    (c :: cs) => if isDigit c then
      let (num, rest) = readNumber $ c :: cs
          consumed = (length $ c::cs) `minus` (length rest)
      in Right (TNumber num, MkLexState rest (state'.pos + consumed))
      else if isAlpha c then
      let (ident, rest) = readIdent $ c :: cs
          consumed = length ident
      in case ident of
        "T" => Right (TBool True, MkLexState rest (state'.pos + consumed))
        "F" => Right (TBool False, MkLexState rest (state'.pos + consumed))
        "e" => Right (TNumber 2.718281828, MkLexState rest (state'.pos + consumed))
        "p" => Right (TNumber 3.141592654, MkLexState rest (state'.pos + consumed))
        _ => Right (TIdent ident, MkLexState rest (state'.pos + consumed))
      else
      Left $ "Unexpected character: " ++ show c ++ " at position " ++ show state'.pos

