module Combinator

import Utils

public export
Parser : Type -> Type
Parser a = HString -> Either String (a, HString)

export
empty : Parser (List a)
empty s = Right ([], s)


public export
(<&>) : Parser a -> (a -> b) -> Parser b
p <&> f = \s => case p s of
  Left err => Left err
  Right (x, rest) => Right (f x, rest)

infixl 4 &>
public export
(&>) : Parser a -> b -> Parser b
p &> x = p <&> \_ => x

public export
(<*>) : Parser a -> Parser b -> Parser (a, b)
p1 <*> p2 = \s =>
  case p1 s of
    Left err => Left err
    Right (a, rest) => case p2 s of
      Left err => Left err
      Right (b, rest') => Right ((a, b), rest')

public export
(<*) : Parser a -> Parser b -> Parser a
p1 <* p2 = p1 <*> p2 <&> \x => fst x

public export
(*>) : Parser a -> Parser b -> Parser b
p1 *> p2 = p1 <*> p2 <&> \x => snd x

infixl 4 <**>
public export
(<**>) : Parser a -> Parser (List a) -> Parser (List a)
p <**> ps = p <*> ps <&> (\(a, as) => a :: as)

public export
(<|>) : Parser a -> Parser a -> Parser a
p1 <|> p2 = \s =>
  case p1 s of
    Left _ => p2 s
    Right result => Right result

mutual
  export
  many : Parser a -> Parser (List a)
  many p = some p <|> empty

  export
  some : Parser a -> Parser (List a)
  some p = p <**> many p

export
satisfy : (Char -> Bool) -> Parser Char
satisfy f s = case s of
  [] => Left $ "Unexpected end of input"
  (c :: rest) => if f c then Right (c, rest) else Left $ "Character '" ++ show c ++ "' does not satisfy the condition"

export
spaces : Parser HString
spaces = many (satisfy isSpace)

export
symbol : HString -> Parser HString
symbol s = case s of
  [] => empty <* spaces
  (c :: rest) => satisfy (== c) <**> symbol rest

export
integer : Parser Integer
integer = some (satisfy isDigit) <* spaces <&> cast . pack

export
float : Parser Double
float = some (satisfy isDigit) <* satisfy (== '.') <**> some (satisfy isDigit) <* spaces <&> cast . pack

export
string : Parser HString
string = satisfy (== '"') *> many (satisfy (/= '"')) <* satisfy (== '"') <* spaces
