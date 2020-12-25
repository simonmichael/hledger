-- | String formatting helpers, starting to get a bit out of control.

module Hledger.Utils.String (
 takeEnd,
 -- * misc
 lowercase,
 uppercase,
 underline,
 stripbrackets,
 unbracket,
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
 strWidthAnsi,
 takeWidth,
 fitString,
 fitStringMulti,
 padLeftWide,
 padRightWide
 ) where


import Data.Char (isSpace, toLower, toUpper)
import Data.Default (def)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Megaparsec ((<|>), between, many, noneOf, sepBy)
import Text.Megaparsec.Char (char)
import Text.Printf (printf)

import Hledger.Utils.Parse
import Hledger.Utils.Regex (toRegex', regexReplace)
import Text.Tabular (Header(..), Properties(..))
import Text.Tabular.AsciiWide (Align(..), TableOpts(..), alignCell, renderRow)
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

unbracket :: String -> String
unbracket s
    | (head s == '[' && last s == ']') || (head s == '(' && last s == ')') = init $ tail s
    | otherwise = s

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, top-padded.
-- Treats wide characters as double width.
concatTopPadded :: [String] -> String
concatTopPadded = TL.unpack . renderRow def{tableBorders=False, borderSpaces=False}
                . Group NoLine . map (Header . cell)
  where cell = alignCell BottomLeft . T.pack

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, bottom-padded.
-- Treats wide characters as double width.
concatBottomPadded :: [String] -> String
concatBottomPadded = TL.unpack . renderRow def{tableBorders=False, borderSpaces=False}
                   . Group NoLine . map (Header . cell)
  where cell = alignCell TopLeft . T.pack


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
   ansire = toRegex' "\ESC\\[([0-9]+;)*([0-9]+)?[ABCDHJKfmsu]"  -- PARTIAL, should succeed
