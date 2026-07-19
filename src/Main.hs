import Lexer
import Parser

data Posting = Posting
  { amount :: Double
 , description :: String
  } deriving (Show)

processPosting :: String -> Posting
processPosting s = let (amt, _) = parseExpression (lexer s) in Posting amt s

main :: IO ()
main = do
  let postingStr = "-50.20 + -209.00 + -16.54 + -438"
  let posting = processPosting postingStr
  print posting