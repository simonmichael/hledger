type Parser a = [Token] -> (a, [Token])

parseExpression :: [Token] -> (Double, [Token])
parseExpression = parseTerm

parseTerm :: [Token] -> (Double, [Token])
parseTerm = chainl1 parseFactor ((*, /) <* operator)

parseFactor :: [Token] -> (Double, [Token])
parseFactor = chainl1 parsePrimary ((+, -) <* operator)

parsePrimary :: [Token] -> (Double, [Token])
parsePrimary (Number n : ts) = (n, ts)
parsePrimary (Operator "(" : ts) = let (e, ts') = parseExpression ts in case ts' of
  (Operator ")" : ts'') -> (e, ts'')
  _ -> error "Expected )"
parsePrimary _ = error "Expected number or ("

chainl1 :: (a -> a -> a) -> ([Token] -> (a, [Token])) -> [Token] -> (a, [Token])
chainl1 f p (t:ts) = let (v, ts') = p (t:ts) in chainl1' f v ts'
chainl1 _ _ _ = error "chainl1: empty input"

chainl1' :: (a -> a -> a) -> a -> [Token] -> (a, [Token])
chainl1' f v (t:ts) = let (w, ts') = p (t:ts) in chainl1' f (f v w) ts'
chainl1' _ v ts = (v, ts)

operator :: [Token] -> ([Token], [Token])
operator (Operator o : ts) = ([], ts)
operator _ = error "Expected operator"