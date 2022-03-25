-- | Text formatting helpers, ported from String as needed.
-- There may be better alternatives out there.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Utils.Text
  (
  -- * misc
  -- lowercase,
  -- uppercase,
  -- underline,
  -- stripbrackets,
  textUnbracket,
  wrap,
  textChomp,
  -- quoting
  quoteIfSpaced,
  textQuoteIfNeeded,
  -- singleQuoteIfNeeded,
  -- quotechars,
  -- whitespacechars,
  escapeDoubleQuotes,
  -- escapeSingleQuotes,
  -- escapeQuotes,
  -- words',
  -- unwords',
  stripquotes,
  -- isSingleQuoted,
  -- isDoubleQuoted,
  -- * single-line layout
  -- elideLeft,
  textElideRight,
  formatText,
  -- * multi-line layout
  textConcatTopPadded,
  textConcatBottomPadded,
  fitText,
  linesPrepend,
  linesPrepend2,
  unlinesB,
  -- * wide-character-aware layout
  WideBuilder(..),
  wbToText,
  wbFromText,
  wbUnpack,
  textTakeWidth,
  -- ** table layout
  module Text.Layout.Table.Cell.Formatted,
  module Text.Layout.Table.Cell.WideString,
  RenderText,
  renderText,
  -- * table rendering
  hledgerStyle,
  hledgerStyleBorders,
  hledgerPrettyStyle,
  hledgerPrettyStyleBorders,
  -- * Reading
  readDecimal,
  -- * tests
  tests_Text
  )
where

import Data.Char (digitToInt)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Text.Layout.Table
import Text.Layout.Table.Cell
import Text.Layout.Table.Cell.Formatted
import Text.Layout.Table.Cell.WideString

import Test.Tasty (testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.WideString (WideBuilder(..), wbToText, wbFromText, wbUnpack)


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
  T.length s >= 2 && T.head s == '\'' && T.last s == '\''

isDoubleQuoted :: Text -> Bool
isDoubleQuoted s =
  T.length s >= 2 && T.head s == '"' && T.last s == '"'

textUnbracket :: Text -> Text
textUnbracket s
    | T.null s = s
    | T.head s == '[' && T.last s == ']' = T.init $ T.tail s
    | T.head s == '(' && T.last s == ')' = T.init $ T.tail s
    | otherwise = s

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, top-padded.
-- Treats wide characters as double width.
textConcatTopPadded :: [Text] -> Text
textConcatTopPadded = concatLines . map mconcat . gridB (repeat def)
                    . colsAsRowsAll bottom . map (map WideText) . map T.lines

-- | Join several multi-line strings as side-by-side rectangular strings of the same height, bottom-padded.
-- Treats wide characters as double width.
textConcatBottomPadded :: [Text] -> Text
textConcatBottomPadded = concatLines . map mconcat . gridB (repeat def)
                       . colsAsRowsAll top . map (map WideText) . map T.lines

-- -- Functions below treat wide (eg CJK) characters as double-width.

-- | General-purpose wide-char-aware single-line text layout function.
-- It can left- or right-pad a short string to a minimum width.
-- It can left- or right-clip a long string to a maximum width, optionally inserting an ellipsis (the third argument).
-- It clips and pads on the right when the fourth argument is true, otherwise on the left.
-- It treats wide characters as double width.
fitText :: Maybe Int -> Maybe Int -> Bool -> Bool -> Text -> Text
fitText mminwidth mmaxwidth ellipsify rightside =
    maybe id clip' mmaxwidth . maybe buildCell pad' mminwidth . WideText
  where
    clip' = trimIfWider ellipsify rightside
    pad'  = pad (if rightside then left else right)

-- | Trim a piece of text if it is wider than given.
trimIfWider :: Bool -> Bool -> Int -> Text -> Text
trimIfWider ellipsify rightside w t
  | visibleLength (WideText t) > w = trim (if rightside then left else right) (if ellipsify then singleCutMark ".." else noCutMark) w $ WideText t
  | otherwise = t

-- | Double-width-character-aware string truncation. Take as many
-- characters as possible from a string without exceeding the
-- specified width. Eg textTakeWidth 3 "りんご" = "り".
textTakeWidth :: Int -> Text -> Text
textTakeWidth = trimIfWider False True

-- | Add a prefix to each line of a string.
linesPrepend :: Text -> Text -> Text
linesPrepend prefix = T.unlines . map (prefix<>) . T.lines

-- | Add a prefix to the first line of a string,
-- and a different prefix to the remaining lines.
linesPrepend2 :: Text -> Text -> Text -> Text
linesPrepend2 prefix1 prefix2 s = T.unlines $ case T.lines s of
    []   -> []
    l:ls -> (prefix1<>l) : map (prefix2<>) ls

-- | Join a list of Text Builders with a newline after each item.
unlinesB :: [TB.Builder] -> TB.Builder
unlinesB = foldMap (<> TB.singleton '\n')

-- | Read a decimal number from a Text. Assumes the input consists only of digit
-- characters.
readDecimal :: Text -> Integer
readDecimal = T.foldl' step 0
  where step a c = a * 10 + toInteger (digitToInt c)

-- | An alias for formatted text measured by display length.
type RenderText = Formatted WideText

-- | Wrap 'Text' in a TextWide wrapper and apply trivial formatting.
renderText :: Text -> RenderText
renderText = plain . WideText

-- | The 'TableStyle' used by hledger when using ascii characters.
hledgerStyle :: TableStyle LineStyle LineStyle
hledgerStyle = withoutBorders hledgerStyleBorders

-- | The 'TableStyle' used by hledger when using ascii characters, including a border.
hledgerStyleBorders :: TableStyle LineStyle LineStyle
hledgerStyleBorders = asciiTableStyleFromSpec . setTableStyleSpecSeparator DoubleLine $
    simpleTableStyleSpec SingleLine SingleLine

-- | The 'TableStyle' used by hledger allowing unicode characters.
hledgerPrettyStyle :: TableStyle LineStyle LineStyle
hledgerPrettyStyle = withoutBorders hledgerPrettyStyleBorders

-- | The 'TableStyle' used by hledger allowing unicode characters, including a border.
hledgerPrettyStyleBorders :: TableStyle LineStyle LineStyle
hledgerPrettyStyleBorders = unicodeS


tests_Text = testGroup "Text" [
   testCase "quoteIfSpaced" $ do
     quoteIfSpaced "a'a" @?= "a'a"
     quoteIfSpaced "a\"a" @?= "a\"a"
     quoteIfSpaced "a a" @?= "\"a a\""
     quoteIfSpaced "mimi's cafe" @?= "\"mimi's cafe\""
     quoteIfSpaced "\"alex\" cafe" @?= "\"\\\"alex\\\" cafe\""
     quoteIfSpaced "le'shan's cafe" @?= "\"le'shan's cafe\""
     quoteIfSpaced "\"be'any's\" cafe" @?= "\"\\\"be'any's\\\" cafe\""
  ]
