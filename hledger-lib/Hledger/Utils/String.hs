module Hledger.Utils.String (
 -- * misc
 lowercase,
 uppercase,
 underline,
 stripbrackets,
 unbracket,
 -- quoting
 quoteIfSpaced,
 quoteIfNeeded,
 singleQuoteIfNeeded,
 -- quotechars,
 -- whitespacechars,
 escapeDoubleQuotes,
 escapeSingleQuotes,
 escapeQuotes,
 words',
 unwords',
 stripquotes,
 isSingleQuoted,
 isDoubleQuoted,
 -- * single-line layout
 strip,
 lstrip,
 rstrip,
 chomp,
 elideLeft,
 elideRight,
 formatString,
 -- * multi-line layout
 concatTopPadded,
 concatBottomPadded,
 concatOneLine,
 vConcatLeftAligned,
 vConcatRightAligned,
 padtop,
 padbottom,
 padleft,
 padright,
 cliptopleft,
 fitto
 ) where


import Data.Char
import Data.List
import Text.Parsec
import Text.Printf (printf)

import Hledger.Utils.Parse
import Hledger.Utils.Regex

lowercase, uppercase :: String -> String
lowercase = map toLower
uppercase = map toUpper

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = lstrip . rstrip

-- | Remove leading whitespace.
lstrip :: String -> String
lstrip = dropWhile (`elem` " \t") :: String -> String -- XXX isSpace ?

-- | Remove trailing whitespace.
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- | Remove trailing newlines/carriage returns.
chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse

stripbrackets :: String -> String
stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse :: String -> String

elideLeft :: Int -> String -> String
elideLeft width s =
    if length s > width then ".." ++ reverse (take (width - 2) $ reverse s) else s

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

-- | Wrap a string in double quotes, and \-prefix any embedded single
-- quotes, if it contains whitespace and is not already single- or
-- double-quoted.
quoteIfSpaced :: String -> String
quoteIfSpaced s | isSingleQuoted s || isDoubleQuoted s = s
                | not $ any (`elem` s) whitespacechars = s
                | otherwise = "'"++escapeSingleQuotes s++"'"

-- | Double-quote this string if it contains whitespace, single quotes
-- or double-quotes, escaping the quotes as needed.
quoteIfNeeded :: String -> String
quoteIfNeeded s | any (`elem` s) (quotechars++whitespacechars) = "\"" ++ escapeDoubleQuotes s ++ "\""
                | otherwise = s

-- | Single-quote this string if it contains whitespace or double-quotes.
-- No good for strings containing single quotes.
singleQuoteIfNeeded :: String -> String
singleQuoteIfNeeded s | any (`elem` s) whitespacechars = "'"++s++"'"
                      | otherwise = s

quotechars, whitespacechars :: [Char]
quotechars      = "'\""
whitespacechars = " \t\n\r"

escapeDoubleQuotes :: String -> String
escapeDoubleQuotes = regexReplace "\"" "\""

escapeSingleQuotes :: String -> String
escapeSingleQuotes = regexReplace "'" "\'"

escapeQuotes :: String -> String
escapeQuotes = regexReplace "([\"'])" "\\1"

-- | Quote-aware version of words - don't split on spaces which are inside quotes.
-- NB correctly handles "a'b" but not "''a''". Can raise an error if parsing fails.
words' :: String -> [String]
words' "" = []
words' s  = map stripquotes $ fromparse $ parsewith p s
    where
      p = do ss <- (singleQuotedPattern <|> doubleQuotedPattern <|> pattern) `sepBy` many1 spacenonewline
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

unbracket :: String -> String
unbracket s
    | (head s == '[' && last s == ']') || (head s == '(' && last s == ')') = init $ tail s
    | otherwise = s

-- | Join multi-line strings as side-by-side rectangular strings of the same height, top-padded.
concatTopPadded :: [String] -> String
concatTopPadded strs = intercalate "\n" $ map concat $ transpose padded
    where
      lss = map lines strs
      h = maximum $ map length lss
      ypad ls = replicate (difforzero h (length ls)) "" ++ ls
      xpad ls = map (padleft w) ls where w | null ls = 0
                                           | otherwise = maximum $ map length ls
      padded = map (xpad . ypad) lss

-- | Join multi-line strings as side-by-side rectangular strings of the same height, bottom-padded.
concatBottomPadded :: [String] -> String
concatBottomPadded strs = intercalate "\n" $ map concat $ transpose padded
    where
      lss = map lines strs
      h = maximum $ map length lss
      ypad ls = ls ++ replicate (difforzero h (length ls)) ""
      xpad ls = map (padright w) ls where w | null ls = 0
                                            | otherwise = maximum $ map length ls
      padded = map (xpad . ypad) lss


-- | Join multi-line strings horizontally, after compressing each of
-- them to a single line with a comma and space between each original line.
concatOneLine :: [String] -> String
concatOneLine strs = concat $ map ((intercalate ", ").lines) strs

-- | Join strings vertically, left-aligned and right-padded.
vConcatLeftAligned :: [String] -> String
vConcatLeftAligned ss = intercalate "\n" $ map showfixedwidth ss
    where
      showfixedwidth = printf (printf "%%-%ds" width)
      width = maximum $ map length ss

-- | Join strings vertically, right-aligned and left-padded.
vConcatRightAligned :: [String] -> String
vConcatRightAligned ss = intercalate "\n" $ map showfixedwidth ss
    where
      showfixedwidth = printf (printf "%%%ds" width)
      width = maximum $ map length ss

-- | Convert a multi-line string to a rectangular string top-padded to the specified height.
padtop :: Int -> String -> String
padtop h s = intercalate "\n" xpadded
    where
      ls = lines s
      sh = length ls
      sw | null ls = 0
         | otherwise = maximum $ map length ls
      ypadded = replicate (difforzero h sh) "" ++ ls
      xpadded = map (padleft sw) ypadded

-- | Convert a multi-line string to a rectangular string bottom-padded to the specified height.
padbottom :: Int -> String -> String
padbottom h s = intercalate "\n" xpadded
    where
      ls = lines s
      sh = length ls
      sw | null ls = 0
         | otherwise = maximum $ map length ls
      ypadded = ls ++ replicate (difforzero h sh) ""
      xpadded = map (padleft sw) ypadded

difforzero :: (Num a, Ord a) => a -> a -> a
difforzero a b = maximum [(a - b), 0]

-- | Convert a multi-line string to a rectangular string left-padded to the specified width.
padleft :: Int -> String -> String
padleft w "" = concat $ replicate w " "
padleft w s = intercalate "\n" $ map (printf (printf "%%%ds" w)) $ lines s

-- | Convert a multi-line string to a rectangular string right-padded to the specified width.
padright :: Int -> String -> String
padright w "" = concat $ replicate w " "
padright w s = intercalate "\n" $ map (printf (printf "%%-%ds" w)) $ lines s

-- | Clip a multi-line string to the specified width and height from the top left.
cliptopleft :: Int -> Int -> String -> String
cliptopleft w h = intercalate "\n" . take h . map (take w) . lines

-- | Clip and pad a multi-line string to fill the specified width and height.
fitto :: Int -> Int -> String -> String
fitto w h s = intercalate "\n" $ take h $ rows ++ repeat blankline
    where
      rows = map (fit w) $ lines s
      fit w = take w . (++ repeat ' ')
      blankline = replicate w ' '

