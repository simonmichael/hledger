-- | Parse format strings provided by --format, with awareness of
-- hledger's report item fields. The formats are used by
-- report-specific renderers like renderBalanceReportItem.

{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, PackageImports #-}

module Hledger.Data.StringFormat (
          parseStringFormat
        , defaultStringFormatStyle
        , StringFormat(..)
        , StringFormatComponent(..)
        , ReportItemField(..)
        , tests_StringFormat
        ) where

import Prelude ()
import "base-compat-batteries" Prelude.Compat
import Numeric
import Data.Char (isPrint)
import Data.Maybe
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger.Utils.Parse
import Hledger.Utils.String (formatString)
import Hledger.Utils.Test

-- | A format specification/template to use when rendering a report line item as text.
--
-- A format is a sequence of components; each is either a literal
-- string, or a hledger report item field with specified width and
-- justification whose value will be interpolated at render time.
--
-- A component's value may be a multi-line string (or a
-- multi-commodity amount), in which case the final string will be
-- either single-line or a top or bottom-aligned multi-line string
-- depending on the StringFormat variant used.
--
-- Currently this is only used in the balance command's single-column
-- mode, which provides a limited StringFormat renderer.
--
data StringFormat =
    OneLine [StringFormatComponent]       -- ^ multi-line values will be rendered on one line, comma-separated
  | TopAligned [StringFormatComponent]    -- ^ values will be top-aligned (and bottom-padded to the same height)
  | BottomAligned [StringFormatComponent] -- ^ values will be bottom-aligned (and top-padded)
  deriving (Show, Eq)

data StringFormatComponent =
    FormatLiteral String        -- ^ Literal text to be rendered as-is
  | FormatField Bool
                (Maybe Int)
                (Maybe Int)
                ReportItemField -- ^ A data field to be formatted and interpolated. Parameters:
                                --
                                -- - Left justify ? Right justified if false
                                -- - Minimum width ? Will be space-padded if narrower than this
                                -- - Maximum width ? Will be clipped if wider than this
                                -- - Which of the standard hledger report item fields to interpolate
  deriving (Show, Eq)

-- | An id identifying which report item field to interpolate.  These
-- are drawn from several hledger report types, so are not all
-- applicable for a given report.
data ReportItemField =
    AccountField      -- ^ A posting or balance report item's account name
  | DefaultDateField  -- ^ A posting or register or entry report item's date
  | DescriptionField  -- ^ A posting or register or entry report item's description
  | TotalField        -- ^ A balance or posting report item's balance or running total.
                      --   Always rendered right-justified.
  | DepthSpacerField  -- ^ A balance report item's indent level (which may be different from the account name depth).
                      --   Rendered as this number of spaces, multiplied by the minimum width spec if any.
  | FieldNo Int       -- ^ A report item's nth field. May be unimplemented.
    deriving (Show, Eq)

----------------------------------------------------------------------

-- renderStringFormat :: StringFormat -> Map String String -> String
-- renderStringFormat fmt params =

----------------------------------------------------------------------

-- | Parse a string format specification, or return a parse error.
parseStringFormat :: String -> Either String StringFormat
parseStringFormat input = case (runParser (stringformatp <* eof) "(unknown)") input of
    Left y -> Left $ show y
    Right x -> Right x

defaultStringFormatStyle = BottomAligned

stringformatp :: SimpleStringParser StringFormat
stringformatp = do
  alignspec <- optional (try $ char '%' >> oneOf ("^_,"::String))
  let constructor =
        case alignspec of
          Just '^' -> TopAligned
          Just '_' -> BottomAligned
          Just ',' -> OneLine
          _        -> defaultStringFormatStyle
  constructor <$> many componentp

componentp :: SimpleStringParser StringFormatComponent
componentp = formatliteralp <|> formatfieldp

formatliteralp :: SimpleStringParser StringFormatComponent
formatliteralp = do
    s <- some c
    return $ FormatLiteral s
    where
      isPrintableButNotPercentage x = isPrint x && x /= '%'
      c =     (satisfy isPrintableButNotPercentage <?> "printable character")
          <|> try (string "%%" >> return '%')

formatfieldp :: SimpleStringParser StringFormatComponent
formatfieldp = do
    char '%'
    leftJustified <- optional (char '-')
    minWidth <- optional (some $ digitChar)
    maxWidth <- optional (do char '.'; some $ digitChar) -- TODO: Can this be (char '1') *> (some digitChar)
    char '('
    f <- fieldp
    char ')'
    return $ FormatField (isJust leftJustified) (parseDec minWidth) (parseDec maxWidth) f
    where
      parseDec s = case s of
        Just text -> Just m where ((m,_):_) = readDec text
        _ -> Nothing

fieldp :: SimpleStringParser ReportItemField
fieldp = do
        try (string "account" >> return AccountField)
    <|> try (string "depth_spacer" >> return DepthSpacerField)
    <|> try (string "date" >> return DescriptionField)
    <|> try (string "description" >> return DescriptionField)
    <|> try (string "total" >> return TotalField)
    <|> try ((FieldNo . read) <$> some digitChar)

----------------------------------------------------------------------

formatStringTester fs value expected = actual `is` expected
  where
    actual = case fs of
      FormatLiteral l                   -> formatString False Nothing Nothing l
      FormatField leftJustify min max _ -> formatString leftJustify min max value

tests_StringFormat = tests "StringFormat" [

   tests "formatStringHelper" [
      formatStringTester (FormatLiteral " ")                                     ""            " "
    , formatStringTester (FormatField False Nothing Nothing DescriptionField)    "description" "description"
    , formatStringTester (FormatField False (Just 20) Nothing DescriptionField)  "description" "         description"
    , formatStringTester (FormatField False Nothing (Just 20) DescriptionField)  "description" "description"
    , formatStringTester (FormatField True Nothing (Just 20) DescriptionField)   "description" "description"
    , formatStringTester (FormatField True (Just 20) Nothing DescriptionField)   "description" "description         "
    , formatStringTester (FormatField True (Just 20) (Just 20) DescriptionField) "description" "description         "
    , formatStringTester (FormatField True Nothing (Just 3) DescriptionField)    "description" "des"
    ]

  ,tests "parseStringFormat" $
    let s `gives` expected = test (T.pack s) $ parseStringFormat s `is` Right expected
    in [
      ""                           `gives` (defaultStringFormatStyle [])
    , "D"                          `gives` (defaultStringFormatStyle [FormatLiteral "D"])
    , "%(date)"                    `gives` (defaultStringFormatStyle [FormatField False Nothing Nothing DescriptionField])
    , "%(total)"                   `gives` (defaultStringFormatStyle [FormatField False Nothing Nothing TotalField])
    -- TODO
    -- , "^%(total)"                  `gives` (TopAligned [FormatField False Nothing Nothing TotalField])
    -- , "_%(total)"                  `gives` (BottomAligned [FormatField False Nothing Nothing TotalField])
    -- , ",%(total)"                  `gives` (OneLine [FormatField False Nothing Nothing TotalField])
    , "Hello %(date)!"             `gives` (defaultStringFormatStyle [FormatLiteral "Hello ", FormatField False Nothing Nothing DescriptionField, FormatLiteral "!"])
    , "%-(date)"                   `gives` (defaultStringFormatStyle [FormatField True Nothing Nothing DescriptionField])
    , "%20(date)"                  `gives` (defaultStringFormatStyle [FormatField False (Just 20) Nothing DescriptionField])
    , "%.10(date)"                 `gives` (defaultStringFormatStyle [FormatField False Nothing (Just 10) DescriptionField])
    , "%20.10(date)"               `gives` (defaultStringFormatStyle [FormatField False (Just 20) (Just 10) DescriptionField])
    , "%20(account) %.10(total)"   `gives` (defaultStringFormatStyle [FormatField False (Just 20) Nothing AccountField
                                                                     ,FormatLiteral " "
                                                                     ,FormatField False Nothing (Just 10) TotalField
                                                                     ])
    , test "newline not parsed" $ expectLeft $ parseStringFormat "\n"
    ]
 ]
