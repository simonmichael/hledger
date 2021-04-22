-- | String formatting helpers, starting to get a bit out of control.

module Hledger.Utils.String (
 takeEnd,
 -- * misc
 lowercase,
 uppercase,
 underline,
 stripbrackets,
 -- quoting
 quoteIfNeeded,
 singleQuoteIfNeeded,
 -- quotechars,
 -- whitespacechars,
 words',
 unwords',
 stripAnsi,
 -- * single-line layout
 strip,
 lstrip,
 rstrip,
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
import Data.List (intercalate)
import qualified Data.Text as T
import Text.Megaparsec ((<|>), between, many, noneOf, sepBy)
import Text.Megaparsec.Char (char)
import Text.Printf (printf)

import Hledger.Utils.Parse
import Hledger.Utils.Regex (toRegex', regexReplace)
import Text.WideString (charWidth, strWidth)


-- | Take elements from the end of a list.
takeEnd n l = go (drop n l) l
  where
    go (_:xs) (_:ys) = go xs ys
    go []     r      = r
    go _      []     = []

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
-- No good for strings containing single quotes.
singleQuoteIfNeeded :: String -> String
singleQuoteIfNeeded s | any (`elem` s) (quotechars++whitespacechars) = "'"++s++"'"
                      | otherwise = s

quotechars, whitespacechars, redirectchars :: [Char]
quotechars      = "'\""
whitespacechars = " \t\n\r"
redirectchars   = "<>"

-- | Quote-aware version of words - don't split on spaces which are inside quotes.
-- NB correctly handles "a'b" but not "''a''". Can raise an error if parsing fails.
words' :: String -> [String]
words' "" = []
words' s  = map stripquotes $ fromparse $ parsewithString p s
    where
      p = do ss <- (singleQuotedPattern <|> doubleQuotedPattern <|> pattern) `sepBy` skipNonNewlineSpaces1
             -- eof
             return ss
      pattern = many (noneOf whitespacechars)
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

-- | Strip ANSI escape sequences from a string.
--
-- >>> stripAnsi "\ESC[31m-1\ESC[m"
-- "-1"
stripAnsi :: String -> String
stripAnsi s = either err id $ regexReplace ansire "" s
 where
   err    = error "stripAnsi: invalid replacement pattern"      -- PARTIAL, shouldn't happen
   ansire = toRegex' $ T.pack "\ESC\\[([0-9]+;)*([0-9]+)?[ABCDHJKfmsu]"  -- PARTIAL, should succeed
