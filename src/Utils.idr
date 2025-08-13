module Utils

public export
HString : Type
HString = List Char

export
joined : String -> List String -> String
joined _ [] = ""
joined _ [x] = x
joined sep (x :: xs) = x ++ sep ++ joined sep xs