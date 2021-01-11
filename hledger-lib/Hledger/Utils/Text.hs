-- | Text formatting helpers, ported from String as needed.
-- There may be better alternatives out there.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Hledger.Utils.Text
  (
 -- -- * misc
 -- lowercase,
 -- uppercase,
 -- underline,
 -- stripbrackets,
  textUnbracket,
  wrap,
  textChomp,
 -- -- quoting
  quoteIfSpaced,
  textQuoteIfNeeded,
 -- singleQuoteIfNeeded,
 -- -- quotechars,
 -- -- whitespacechars,
  escapeDoubleQuotes,
 -- escapeSingleQuotes,
 -- escapeQuotes,
 -- words',
 -- unwords',
  stripquotes,
 -- isSingleQuoted,
 -- isDoubleQuoted,
 -- -- * single-line layout
 -- elideLeft,
  textElideRight,
  formatText,
 -- -- * multi-line layout
  textConcatTopPadded,
 -- concatBottomPadded,
 -- concatOneLine,
 -- vConcatLeftAligned,
 -- vConcatRightAligned,
 -- padtop,
 -- padbottom,
 -- padleft,
 -- padright,
 -- cliptopleft,
 -- fitto,
  fitText,
  linesPrepend,
  linesPrepend2,
  unlinesB,
 -- -- * wide-character-aware layout
  WideBuilder(..),
  wbToText,
  wbUnpack,
  textWidth,
  textTakeWidth,
 -- fitString,
 -- fitStringMulti,
  textPadLeftWide,
  textPadRightWide,
  -- -- * Reading
  readDecimal,
  -- -- * tests
  tests_Text
  )
where

import Data.Char (digitToInt)
import Data.List
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
-- import Text.Parsec
-- import Text.Printf (printf)

-- import Hledger.Utils.Parse
-- import Hledger.Utils.Regex
import Hledger.Utils.Test
import Text.WideString (WideBuilder(..), wbToText, wbUnpack, charWidth, textWidth)


-- lowercase, uppercase :: String -> String
-- lowercase = map toLower
-- uppercase = map toUpper

-- stripbrackets :: String -> String
-- stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse :: String -> String

-- elideLeft :: Int -> String -> String
-- elideLeft width s =
--     if length s > width then ".." ++ reverse (take (width - 2) $ reverse s) else s

textElideRight :: Int -> Text -> Text
textElideRight width t =
    if T.length t > width then T.take (width - 2) t <> ".." else t

-- | Wrap a Text with the surrounding Text.
wrap :: Text -> Text -> Text -> Text
wrap start end x = start <> x <> end

-- | Remove trailing newlines/carriage returns.
textChomp :: Text -> Text
textChomp = T.dropWhileEnd (`elem` ['\r', '\n'])

-- | Clip and pad a string to a minimum & maximum width, and/or left/right justify it.
-- Works on multi-line strings too (but will rewrite non-unix line endings).
formatText :: Bool -> Maybe Int -> Maybe Int -> Text -> Text
formatText leftJustified minwidth maxwidth t =
    T.intercalate "\n" . map (pad . clip) $ if T.null t then [""] else T.lines t
  where
    pad  = maybe id justify minwidth
    clip = maybe id T.take maxwidth
    justify n = if leftJustified then T.justifyLeft n ' ' else T.justifyRight n ' '

-- underline :: String -> String
-- underline s = s' ++ replicate (length s) '-' ++ "\n"
--     where s'
--             | last s == '\n' = s
--             | otherwise = s ++ "\n"

-- | Wrap a string in double quotes, and \-prefix any embedded single
-- quotes, if it contains whitespace and is not already single- or
-- double-quoted.
quoteIfSpaced :: T.Text -> T.Text
quoteIfSpaced s | isSingleQuoted s || isDoubleQuoted s = s
                | not $ any (\c -> T.any (==c) s) whitespacechars = s
                | otherwise = textQuoteIfNeeded s

-- -- | Wrap a string in double quotes, and \-prefix any embedded single
-- -- quotes, if it contains whitespace and is not already single- or
-- -- double-quoted.
-- quoteIfSpaced :: String -> String
-- quoteIfSpaced s | isSingleQuoted s || isDoubleQuoted s = s
--                 | not $ any (`elem` s) whitespacechars = s
--                 | otherwise = "'"++escapeSingleQuotes s++"'"

-- -- | Double-quote this string if it contains whitespace, single quotes
-- -- or double-quotes, escaping the quotes as needed.
textQuoteIfNeeded :: T.Text -> T.Text
textQuoteIfNeeded s | any (\c -> T.any (==c) s) (quotechars++whitespacechars) = "\"" <> escapeDoubleQuotes s <> "\""
                    | otherwise = s

-- -- | Single-quote this string if it contains whitespace or double-quotes.
-- -- No good for strings containing single quotes.
-- singleQuoteIfNeeded :: String -> String
-- singleQuoteIfNeeded s | any (`elem` s) whitespacechars = "'"++s++"'"
--                       | otherwise = s

quotechars, whitespacechars :: [Char]
quotechars      = "'\""
whitespacechars = " \t\n\r"

escapeDoubleQuotes :: T.Text -> T.Text
escapeDoubleQuotes = T.replace "\"" "\\\""

-- escapeSingleQuotes :: T.Text -> T.Text
-- escapeSingleQuotes = T.replace "'" "\'"

-- escapeQuotes :: String -> String
-- escapeQuotes = regexReplace "([\"'])" "\\1"

-- -- | Quote-aware version of words - don't split on spaces which are inside quotes.
-- -- NB correctly handles "a'b" but not "''a''". Can raise an error if parsing fails.
-- words' :: String -> [String]
-- words' "" = []
-- words' s  = map stripquotes $ fromparse $ parsewith p s
--     where
--       p = do ss <- (singleQuotedPattern <|> doubleQuotedPattern <|> pattern) `sepBy` many1 spacenonewline
--              -- eof
--              return ss
--       pattern = many (noneOf whitespacechars)
--       singleQuotedPattern = between (char '\'') (char '\'') (many $ noneOf "'")
--       doubleQuotedPattern = between (char '"') (char '"') (many $ noneOf "\"")

-- -- | Quote-aware version of unwords - single-quote strings which contain whitespace
-- unwords' :: [Text] -> Text
-- unwords' = T.unwords . map quoteIfNeeded

-- | Strip one matching pair of single or double quotes on the ends of a string.
stripquotes :: Text -> Text
stripquotes s = if isSingleQuoted s || isDoubleQuoted s then T.init $ T.tail s else s

isSingleQuoted :: Text -> Bool
isSingleQuoted s =
  T.length (T.take 2 s) == 2 && T.head s == '\'' && T.last s == '\''

isDoubleQuoted :: Text -> Bool
isDoubleQuoted s =
  T.length (T.take 2 s) == 2 && T.head s == '"' && T.last s == '"'

textUnbracket :: Text -> Text
textUnbracket s
    | (T.head s == '[' && T.last s == ']') || (T.head s == '(' && T.last s == ')') = T.init $ T.tail s
    | otherwise = s

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, top-padded.
-- Treats wide characters as double width.
textConcatTopPadded :: [Text] -> Text
textConcatTopPadded ts = T.intercalate "\n" $ map T.concat $ transpose padded
    where
      lss = map T.lines ts :: [[Text]]
      h = maximum $ map length lss
      ypad ls = replicate (difforzero h (length ls)) "" ++ ls
      xpad ls = map (textPadLeftWide w) ls
        where w | null ls = 0
                | otherwise = maximum $ map textWidth ls
      padded = map (xpad . ypad) lss :: [[Text]]

-- -- | Join several multi-line strings as side-by-side rectangular strings of the same height, bottom-padded.
-- -- Treats wide characters as double width.
-- concatBottomPadded :: [String] -> String
-- concatBottomPadded strs = intercalate "\n" $ map concat $ transpose padded
--     where
--       lss = map lines strs
--       h = maximum $ map length lss
--       ypad ls = ls ++ replicate (difforzero h (length ls)) ""
--       xpad ls = map (padRightWide w) ls where w | null ls = 0
--                                                 | otherwise = maximum $ map strWidth ls
--       padded = map (xpad . ypad) lss


-- -- | Join multi-line strings horizontally, after compressing each of
-- -- them to a single line with a comma and space between each original line.
-- concatOneLine :: [String] -> String
-- concatOneLine strs = concat $ map ((intercalate ", ").lines) strs

-- -- | Join strings vertically, left-aligned and right-padded.
-- vConcatLeftAligned :: [String] -> String
-- vConcatLeftAligned ss = intercalate "\n" $ map showfixedwidth ss
--     where
--       showfixedwidth = printf (printf "%%-%ds" width)
--       width = maximum $ map length ss

-- -- | Join strings vertically, right-aligned and left-padded.
-- vConcatRightAligned :: [String] -> String
-- vConcatRightAligned ss = intercalate "\n" $ map showfixedwidth ss
--     where
--       showfixedwidth = printf (printf "%%%ds" width)
--       width = maximum $ map length ss

-- -- | Convert a multi-line string to a rectangular string top-padded to the specified height.
-- padtop :: Int -> String -> String
-- padtop h s = intercalate "\n" xpadded
--     where
--       ls = lines s
--       sh = length ls
--       sw | null ls = 0
--          | otherwise = maximum $ map length ls
--       ypadded = replicate (difforzero h sh) "" ++ ls
--       xpadded = map (padleft sw) ypadded

-- -- | Convert a multi-line string to a rectangular string bottom-padded to the specified height.
-- padbottom :: Int -> String -> String
-- padbottom h s = intercalate "\n" xpadded
--     where
--       ls = lines s
--       sh = length ls
--       sw | null ls = 0
--          | otherwise = maximum $ map length ls
--       ypadded = ls ++ replicate (difforzero h sh) ""
--       xpadded = map (padleft sw) ypadded

difforzero :: (Num a, Ord a) => a -> a -> a
difforzero a b = maximum [(a - b), 0]

-- -- | Convert a multi-line string to a rectangular string left-padded to the specified width.
-- -- Treats wide characters as double width.
-- padleft :: Int -> String -> String
-- padleft w "" = concat $ replicate w " "
-- padleft w s = intercalate "\n" $ map (printf (printf "%%%ds" w)) $ lines s

-- -- | Convert a multi-line string to a rectangular string right-padded to the specified width.
-- -- Treats wide characters as double width.
-- padright :: Int -> String -> String
-- padright w "" = concat $ replicate w " "
-- padright w s = intercalate "\n" $ map (printf (printf "%%-%ds" w)) $ lines s

-- -- | Clip a multi-line string to the specified width and height from the top left.
-- cliptopleft :: Int -> Int -> String -> String
-- cliptopleft w h = intercalate "\n" . take h . map (take w) . lines

-- -- | Clip and pad a multi-line string to fill the specified width and height.
-- fitto :: Int -> Int -> String -> String
-- fitto w h s = intercalate "\n" $ take h $ rows ++ repeat blankline
--     where
--       rows = map (fit w) $ lines s
--       fit w = take w . (++ repeat ' ')
--       blankline = replicate w ' '

-- -- Functions below treat wide (eg CJK) characters as double-width.

-- | General-purpose wide-char-aware single-line text layout function.
-- It can left- or right-pad a short string to a minimum width.
-- It can left- or right-clip a long string to a maximum width, optionally inserting an ellipsis (the third argument).
-- It clips and pads on the right when the fourth argument is true, otherwise on the left.
-- It treats wide characters as double width.
fitText :: Maybe Int -> Maybe Int -> Bool -> Bool -> Text -> Text
fitText mminwidth mmaxwidth ellipsify rightside = clip . pad
  where
    clip :: Text -> Text
    clip s =
      case mmaxwidth of
        Just w
          | textWidth s > w ->
            case rightside of
              True  -> textTakeWidth (w - T.length ellipsis) s <> ellipsis
              False -> ellipsis <> T.reverse (textTakeWidth (w - T.length ellipsis) $ T.reverse s)
          | otherwise -> s
          where
            ellipsis = if ellipsify then ".." else ""
        Nothing -> s
    pad :: Text -> Text
    pad s =
      case mminwidth of
        Just w
          | sw < w ->
            case rightside of
              True  -> s <> T.replicate (w - sw) " "
              False -> T.replicate (w - sw) " " <> s
          | otherwise -> s
        Nothing -> s
      where sw = textWidth s

-- -- | A version of fitString that works on multi-line strings,
-- -- separate for now to avoid breakage.
-- -- This will rewrite any line endings to unix newlines.
-- fitStringMulti :: Maybe Int -> Maybe Int -> Bool -> Bool -> String -> String
-- fitStringMulti mminwidth mmaxwidth ellipsify rightside s =
--   (intercalate "\n" . map (fitString mminwidth mmaxwidth ellipsify rightside) . lines) s

-- | Left-pad a text to the specified width.
-- Treats wide characters as double width.
-- Works on multi-line texts too (but will rewrite non-unix line endings).
textPadLeftWide :: Int -> Text -> Text
textPadLeftWide w "" = T.replicate w " "
textPadLeftWide w s  = T.intercalate "\n" $ map (fitText (Just w) Nothing False False) $ T.lines s
-- XXX not yet replaceable by
-- padLeftWide w = fitStringMulti (Just w) Nothing False False

-- | Right-pad a string to the specified width.
-- Treats wide characters as double width.
-- Works on multi-line strings too (but will rewrite non-unix line endings).
textPadRightWide :: Int -> Text -> Text
textPadRightWide w "" = T.replicate w " "
textPadRightWide w s  = T.intercalate "\n" $ map (fitText (Just w) Nothing False True) $ T.lines s
-- XXX not yet replaceable by
-- padRightWide w = fitStringMulti (Just w) Nothing False True

-- | Double-width-character-aware string truncation. Take as many
-- characters as possible from a string without exceeding the
-- specified width. Eg textTakeWidth 3 "りんご" = "り".
textTakeWidth :: Int -> Text -> Text
textTakeWidth _ ""     = ""
textTakeWidth 0 _      = ""
textTakeWidth w t | not (T.null t),
                let c = T.head t,
                let cw = charWidth c,
                cw <= w
                = T.cons c $ textTakeWidth (w-cw) (T.tail t)
              | otherwise = ""

-- | Add a prefix to each line of a string.
linesPrepend :: Text -> Text -> Text
linesPrepend prefix = T.unlines . map (prefix<>) . T.lines

-- | Add a prefix to the first line of a string, 
-- and a different prefix to the remaining lines.
linesPrepend2 :: Text -> Text -> Text -> Text
linesPrepend2 prefix1 prefix2 s = T.unlines $ case T.lines s of
    []   -> []
    l:ls -> (prefix1<>l) : map (prefix2<>) ls

-- | Concatenate a list of Text Builders with a newline between each item, and
-- another at the end. If the list is empty, return the identity builder.
unlinesB :: [TB.Builder] -> TB.Builder
unlinesB [] = mempty
unlinesB xs = mconcat (intersperse (TB.singleton '\n') xs) <> TB.singleton '\n'

-- | Read a decimal number from a Text. Assumes the input consists only of digit
-- characters.
readDecimal :: Text -> Integer
readDecimal = T.foldl' step 0
  where step a c = a * 10 + toInteger (digitToInt c)


tests_Text = tests "Text" [
   test "quoteIfSpaced" $ do
     quoteIfSpaced "a'a" @?= "a'a"
     quoteIfSpaced "a\"a" @?= "a\"a"
     quoteIfSpaced "a a" @?= "\"a a\""
     quoteIfSpaced "mimi's cafe" @?= "\"mimi's cafe\""
     quoteIfSpaced "\"alex\" cafe" @?= "\"\\\"alex\\\" cafe\""
     quoteIfSpaced "le'shan's cafe" @?= "\"le'shan's cafe\""
     quoteIfSpaced "\"be'any's\" cafe" @?= "\"\\\"be'any's\\\" cafe\""
  ]
