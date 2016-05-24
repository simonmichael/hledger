-- | String formatting helpers, starting to get a bit out of control.

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
 fitto,
 -- * wide-character-aware layout
 charWidth,
 strWidth,
 takeWidth,
 fitString,
 fitStringMulti,
 padLeftWide,
 padRightWide
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

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, top-padded.
-- Treats wide characters as double width.
concatTopPadded :: [String] -> String
concatTopPadded strs = intercalate "\n" $ map concat $ transpose padded
    where
      lss = map lines strs
      h = maximum $ map length lss
      ypad ls = replicate (difforzero h (length ls)) "" ++ ls
      xpad ls = map (padLeftWide w) ls where w | null ls = 0
                                               | otherwise = maximum $ map strWidth ls
      padded = map (xpad . ypad) lss

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, bottom-padded.
-- Treats wide characters as double width.
concatBottomPadded :: [String] -> String
concatBottomPadded strs = intercalate "\n" $ map concat $ transpose padded
    where
      lss = map lines strs
      h = maximum $ map length lss
      ypad ls = ls ++ replicate (difforzero h (length ls)) ""
      xpad ls = map (padRightWide w) ls where w | null ls = 0
                                                | otherwise = maximum $ map strWidth ls
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
-- Treats wide characters as double width.
padleft :: Int -> String -> String
padleft w "" = concat $ replicate w " "
padleft w s = intercalate "\n" $ map (printf (printf "%%%ds" w)) $ lines s

-- | Convert a multi-line string to a rectangular string right-padded to the specified width.
-- Treats wide characters as double width.
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

-- Functions below treat wide (eg CJK) characters as double-width.

-- | General-purpose wide-char-aware single-line string layout function.
-- It can left- or right-pad a short string to a minimum width.
-- It can left- or right-clip a long string to a maximum width, optionally inserting an ellipsis (the third argument).
-- It clips and pads on the right when the fourth argument is true, otherwise on the left.
-- It treats wide characters as double width.
fitString :: Maybe Int -> Maybe Int -> Bool -> Bool -> String -> String
fitString mminwidth mmaxwidth ellipsify rightside s = (clip . pad) s
  where
    clip :: String -> String
    clip s =
      case mmaxwidth of
        Just w
          | strWidth s > w ->
            case rightside of
              True  -> takeWidth (w - length ellipsis) s ++ ellipsis
              False -> ellipsis ++ reverse (takeWidth (w - length ellipsis) $ reverse s)
          | otherwise -> s
          where
            ellipsis = if ellipsify then ".." else ""
        Nothing -> s
    pad :: String -> String
    pad s =
      case mminwidth of
        Just w
          | sw < w ->
            case rightside of
              True  -> s ++ replicate (w - sw) ' '
              False -> replicate (w - sw) ' ' ++ s
          | otherwise -> s
        Nothing -> s
      where sw = strWidth s

-- | A version of fitString that works on multi-line strings,
-- separate for now to avoid breakage.
-- This will rewrite any line endings to unix newlines.
fitStringMulti :: Maybe Int -> Maybe Int -> Bool -> Bool -> String -> String
fitStringMulti mminwidth mmaxwidth ellipsify rightside s =
  (intercalate "\n" . map (fitString mminwidth mmaxwidth ellipsify rightside) . lines) s

-- | Left-pad a string to the specified width.
-- Treats wide characters as double width.
-- Works on multi-line strings too (but will rewrite non-unix line endings).
padLeftWide :: Int -> String -> String
padLeftWide w "" = replicate w ' '
padLeftWide w s  = intercalate "\n" $ map (fitString (Just w) Nothing False False) $ lines s
-- XXX not yet replaceable by
-- padLeftWide w = fitStringMulti (Just w) Nothing False False

-- | Right-pad a string to the specified width.
-- Treats wide characters as double width.
-- Works on multi-line strings too (but will rewrite non-unix line endings).
padRightWide :: Int -> String -> String
padRightWide w "" = replicate w ' '
padRightWide w s  = intercalate "\n" $ map (fitString (Just w) Nothing False True) $ lines s
-- XXX not yet replaceable by
-- padRightWide w = fitStringMulti (Just w) Nothing False True

-- | Double-width-character-aware string truncation. Take as many
-- characters as possible from a string without exceeding the
-- specified width. Eg takeWidth 3 "りんご" = "り".
takeWidth :: Int -> String -> String
takeWidth _ ""     = ""
takeWidth 0 _      = ""
takeWidth w (c:cs) | cw <= w   = c:takeWidth (w-cw) cs
                   | otherwise = ""
  where cw = charWidth c

-- from Pandoc (copyright John MacFarlane, GPL)
-- see also http://unicode.org/reports/tr11/#Description

-- | Calculate the designated render width of a string, taking into
-- account wide characters and line breaks (the longest line within a
-- multi-line string determines the width ).
strWidth :: String -> Int
strWidth "" = 0
strWidth s = maximum $ map (foldr (\a b -> charWidth a + b) 0) $ lines s

-- | Get the designated render width of a character: 0 for a combining
-- character, 1 for a regular character, 2 for a wide character.
-- (Wide characters are rendered as exactly double width in apps and
-- fonts that support it.) (From Pandoc.)
charWidth :: Char -> Int
charWidth c =
  case c of
      _ | c <  '\x0300'                    -> 1
        | c >= '\x0300' && c <= '\x036F'   -> 0  -- combining
        | c >= '\x0370' && c <= '\x10FC'   -> 1
        | c >= '\x1100' && c <= '\x115F'   -> 2
        | c >= '\x1160' && c <= '\x11A2'   -> 1
        | c >= '\x11A3' && c <= '\x11A7'   -> 2
        | c >= '\x11A8' && c <= '\x11F9'   -> 1
        | c >= '\x11FA' && c <= '\x11FF'   -> 2
        | c >= '\x1200' && c <= '\x2328'   -> 1
        | c >= '\x2329' && c <= '\x232A'   -> 2
        | c >= '\x232B' && c <= '\x2E31'   -> 1
        | c >= '\x2E80' && c <= '\x303E'   -> 2
        | c == '\x303F'                    -> 1
        | c >= '\x3041' && c <= '\x3247'   -> 2
        | c >= '\x3248' && c <= '\x324F'   -> 1 -- ambiguous
        | c >= '\x3250' && c <= '\x4DBF'   -> 2
        | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
        | c >= '\x4E00' && c <= '\xA4C6'   -> 2
        | c >= '\xA4D0' && c <= '\xA95F'   -> 1
        | c >= '\xA960' && c <= '\xA97C'   -> 2
        | c >= '\xA980' && c <= '\xABF9'   -> 1
        | c >= '\xAC00' && c <= '\xD7FB'   -> 2
        | c >= '\xD800' && c <= '\xDFFF'   -> 1
        | c >= '\xE000' && c <= '\xF8FF'   -> 1 -- ambiguous
        | c >= '\xF900' && c <= '\xFAFF'   -> 2
        | c >= '\xFB00' && c <= '\xFDFD'   -> 1
        | c >= '\xFE00' && c <= '\xFE0F'   -> 1 -- ambiguous
        | c >= '\xFE10' && c <= '\xFE19'   -> 2
        | c >= '\xFE20' && c <= '\xFE26'   -> 1
        | c >= '\xFE30' && c <= '\xFE6B'   -> 2
        | c >= '\xFE70' && c <= '\xFEFF'   -> 1
        | c >= '\xFF01' && c <= '\xFF60'   -> 2
        | c >= '\xFF61' && c <= '\x16A38'  -> 1
        | c >= '\x1B000' && c <= '\x1B001' -> 2
        | c >= '\x1D000' && c <= '\x1F1FF' -> 1
        | c >= '\x1F200' && c <= '\x1F251' -> 2
        | c >= '\x1F300' && c <= '\x1F773' -> 1
        | c >= '\x20000' && c <= '\x3FFFD' -> 2
        | otherwise                        -> 1

