-- | String formatting helpers, starting to get a bit out of control.

module Hledger.Utils.String (
 takeEnd,
 -- * misc
 capitalise,
 lowercase,
 uppercase,
 underline,
 stripbrackets,
 -- quoting
 quoteIfNeeded,
 singleQuoteIfNeeded,
 quoteForCommandLine,
 -- quotechars,
 -- whitespacechars,
 words',
 unwords',
 stripAnsi,
 -- * single-line layout
 strip,
 lstrip,
 rstrip,
 strip1Char,
 stripBy,
 strip1By,
 chomp,
 chomp1,
 singleline,
 elideLeft,
 elideRight,
 formatString,
 -- * wide-character-aware layout
 charWidth,
 strWidth,
 strWidthAnsi,
 takeWidth,
 ) where


import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate, dropWhileEnd)
import qualified Data.Text as T
import Text.Megaparsec ((<|>), between, many, noneOf, sepBy)
import Text.Megaparsec.Char (char)
import Text.Printf (printf)

import Hledger.Utils.Parse
import Hledger.Utils.Regex (toRegex', regexReplace)
import Text.DocLayout (charWidth, realLength)


-- | Take elements from the end of a list.
takeEnd n l = go (drop n l) l
  where
    go (_:xs) (_:ys) = go xs ys
    go []     r      = r
    go _      []     = []

capitalise :: String -> String
capitalise (c:cs) = toUpper c : cs
capitalise s = s

lowercase, uppercase :: String -> String
lowercase = map toLower
uppercase = map toUpper

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = lstrip . rstrip

-- | Remove leading whitespace.
lstrip :: String -> String
lstrip = dropWhile isSpace

-- | Remove trailing whitespace.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- | Strip the given starting and ending character
-- from the start and end of a string if both are present.
strip1Char :: Char -> Char -> String -> String
strip1Char b e s = case s of
  (c:cs) | c==b, not $ null cs, last cs==e -> init cs
  _ -> s

-- | Strip a run of zero or more characters matching the predicate
-- from the start and end of a string.
stripBy :: (Char -> Bool) -> String -> String
stripBy f = dropWhileEnd f . dropWhile f

-- | Strip a single balanced enclosing pair of a character matching the predicate
-- from the start and end of a string.
strip1By :: (Char -> Bool) -> String -> String
strip1By f s = case s of
  (c:cs) | f c, not $ null cs, last cs==c -> init cs
  _ -> s

-- | Remove all trailing newlines/carriage returns.
chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse

-- | Remove all trailing newline/carriage returns, leaving just one trailing newline.
chomp1 :: String -> String
chomp1 = (++"\n") . chomp

-- | Remove consecutive line breaks, replacing them with single space
singleline :: String -> String
singleline = unwords . filter (/="") . (map strip) . lines

stripbrackets :: String -> String
stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse :: String -> String

elideLeft :: Int -> String -> String
elideLeft width s =
    if length s > width then ".." ++ takeEnd (width - 2) s else s

elideRight :: Int -> String -> String
elideRight width s =
    if length s > width then take (width - 2) s ++ ".." else s

-- | Clip and pad a string to a minimum & maximum width, and/or left/right justify it.
-- Works on multi-line strings too (but will rewrite non-unix line endings).
formatString :: Bool -> Maybe Int -> Maybe Int -> String -> String
formatString leftJustified minwidth maxwidth s = intercalate "\n" $ map (printf fmt) $ lines s
    where
      justify = if leftJustified then "-" else ""
      minwidth' = maybe "" show minwidth
      maxwidth' = maybe "" (("."++).show) maxwidth
      fmt = "%" ++ justify ++ minwidth' ++ maxwidth' ++ "s"

underline :: String -> String
underline s = s' ++ replicate (length s) '-' ++ "\n"
    where s'
            | last s == '\n' = s
            | otherwise = s ++ "\n"

-- | Double-quote this string if it contains whitespace, single quotes
-- or double-quotes, escaping the quotes as needed.
quoteIfNeeded :: String -> String
quoteIfNeeded s | any (`elem` s) (quotechars++whitespacechars++redirectchars) = showChar '"' $ escapeQuotes s "\""
                | otherwise = s
  where
    escapeQuotes []       x = x
    escapeQuotes ('"':cs) x = showString "\\\"" $ escapeQuotes cs x
    escapeQuotes (c:cs)   x = showChar c        $ escapeQuotes cs x

-- | Single-quote this string if it contains whitespace or double-quotes.
-- Does not work for strings containing single quotes.
singleQuoteIfNeeded :: String -> String
singleQuoteIfNeeded s | any (`elem` s) (quotechars++whitespacechars) = singleQuote s
                      | otherwise = s

-- | Prepend and append single quotes to a string.
singleQuote :: String -> String
singleQuote s = "'"++s++"'"

-- | Try to single- and backslash-quote a string as needed to make it usable
-- as an argument on a (sh/bash) shell command line. At least, well enough 
-- to handle common currency symbols, like $. Probably broken in many ways.
--
-- >>> quoteForCommandLine "a"
-- "a"
-- >>> quoteForCommandLine "\""
-- "'\"'"
-- >>> quoteForCommandLine "$"
-- "'\\$'"
--
quoteForCommandLine :: String -> String
quoteForCommandLine s
  | any (`elem` s) (quotechars++whitespacechars++shellchars) = singleQuote $ quoteShellChars s
  | otherwise = s

-- | Try to backslash-quote common shell-significant characters in this string.
-- Doesn't handle single quotes, & probably others.
quoteShellChars :: String -> String
quoteShellChars = concatMap escapeShellChar
  where
    escapeShellChar c | c `elem` shellchars = ['\\',c]
    escapeShellChar c = [c]

quotechars, whitespacechars, redirectchars, shellchars :: [Char]
quotechars      = "'\""
whitespacechars = " \t\n\r"
redirectchars   = "<>"
shellchars      = "<>(){}[]$7?#!~`"

-- | Quote-aware version of words - don't split on spaces which are inside quotes.
-- NB correctly handles "a'b" but not "''a''". Can raise an error if parsing fails.
words' :: String -> [String]
words' "" = []
words' s  = map stripquotes $ fromparse $ parsewithString p s
    where
      p = (singleQuotedPattern <|> doubleQuotedPattern <|> patterns) `sepBy` skipNonNewlineSpaces1
          -- eof
      patterns = many (noneOf whitespacechars)
      singleQuotedPattern = between (char '\'') (char '\'') (many $ noneOf "'")
      doubleQuotedPattern = between (char '"') (char '"') (many $ noneOf "\"")

-- | Quote-aware version of unwords - single-quote strings which contain whitespace
unwords' :: [String] -> String
unwords' = unwords . map quoteIfNeeded

-- | Strip one matching pair of single or double quotes on the ends of a string.
stripquotes :: String -> String
stripquotes s = if isSingleQuoted s || isDoubleQuoted s then init $ tail s else s

isSingleQuoted s@(_:_:_) = head s == '\'' && last s == '\''
isSingleQuoted _ = False

isDoubleQuoted s@(_:_:_) = head s == '"' && last s == '"'
isDoubleQuoted _ = False

-- Functions below treat wide (eg CJK) characters as double-width.

-- | Double-width-character-aware string truncation. Take as many
-- characters as possible from a string without exceeding the
-- specified width. Eg takeWidth 3 "りんご" = "り".
takeWidth :: Int -> String -> String
takeWidth _ ""     = ""
takeWidth 0 _      = ""
takeWidth w (c:cs) | cw <= w   = c:takeWidth (w-cw) cs
                   | otherwise = ""
  where cw = charWidth c

-- | Like strWidth, but also strips ANSI escape sequences before
-- calculating the width.
--
-- This is no longer used in code, as widths are calculated before
-- adding ANSI escape sequences, but is being kept around for now.
strWidthAnsi :: String -> Int
strWidthAnsi = strWidth . stripAnsi

-- | Alias for 'realLength'.
strWidth :: String -> Int
strWidth = realLength

-- | Strip ANSI escape sequences from a string.
--
-- >>> stripAnsi "\ESC[31m-1\ESC[m"
-- "-1"
stripAnsi :: String -> String
stripAnsi s = either err id $ regexReplace ansire "" s
 where
   err    = error "stripAnsi: invalid replacement pattern"      -- PARTIAL, shouldn't happen
   ansire = toRegex' $ T.pack "\ESC\\[([0-9]+;)*([0-9]+)?[ABCDHJKfmsu]"  -- PARTIAL, should succeed
