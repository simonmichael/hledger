data Token
  = Number Double
  | Operator String
  | Parenthesis String
  | Other String
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | c `elem` "+-*/()" = Operator [c] : lexer cs
  | isDigit c || c == '.' = let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs) in Number (read num) : lexer rest
  | otherwise = Other [c] : lexer cs