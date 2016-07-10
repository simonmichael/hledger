-- | Parse format strings provided by --format, with awareness of
-- hledger's report item fields. The formats are used by
-- report-specific renderers like renderBalanceReportItem.

{-# LANGUAGE FlexibleContexts #-}

module Hledger.Data.StringFormat (
          parseStringFormat
        , defaultStringFormatStyle
        , StringFormat(..)
        , StringFormatComponent(..)
        , ReportItemField(..)
        , tests
        ) where

import Prelude ()
import Prelude.Compat
import Numeric
import Data.Char (isPrint)
import Data.Maybe
import Test.HUnit
import Text.Megaparsec

import Hledger.Utils.String (formatString)

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

stringformatp :: Stream s Char => ParsecT s m StringFormat
stringformatp = do
  alignspec <- optional (try $ char '%' >> oneOf "^_,")
  let constructor =
        case alignspec of
          Just '^' -> TopAligned
          Just '_' -> BottomAligned
          Just ',' -> OneLine
          _        -> defaultStringFormatStyle
  constructor <$> many componentp

componentp :: Stream s Char => ParsecT s m StringFormatComponent
componentp = formatliteralp <|> formatfieldp

formatliteralp :: Stream s Char => ParsecT s m StringFormatComponent
formatliteralp = do
    s <- some c
    return $ FormatLiteral s
    where
      isPrintableButNotPercentage x = isPrint x && (not $ x == '%')
      c =     (satisfy isPrintableButNotPercentage <?> "printable character")
          <|> try (string "%%" >> return '%')

formatfieldp :: Stream s Char => ParsecT s m StringFormatComponent
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

fieldp :: Stream s Char => ParsecT s m ReportItemField
fieldp = do
        try (string "account" >> return AccountField)
    <|> try (string "depth_spacer" >> return DepthSpacerField)
    <|> try (string "date" >> return DescriptionField)
    <|> try (string "description" >> return DescriptionField)
    <|> try (string "total" >> return TotalField)
    <|> try (some digitChar >>= (\s -> return $ FieldNo $ read s))

----------------------------------------------------------------------

testFormat :: StringFormatComponent -> String -> String -> Assertion
testFormat fs value expected = assertEqual name expected actual
    where
        (name, actual) = case fs of
            FormatLiteral l -> ("literal", formatString False Nothing Nothing l)
            FormatField leftJustify min max _ -> ("field", formatString leftJustify min max value)

testParser :: String -> StringFormat -> Assertion
testParser s expected = case (parseStringFormat s) of
    Left  error -> assertFailure $ show error
    Right actual -> assertEqual ("Input: " ++ s) expected actual

tests = test [ formattingTests ++ parserTests ]

formattingTests = [
      testFormat (FormatLiteral " ")                                ""            " "
    , testFormat (FormatField False Nothing Nothing DescriptionField)    "description" "description"
    , testFormat (FormatField False (Just 20) Nothing DescriptionField)  "description" "         description"
    , testFormat (FormatField False Nothing (Just 20) DescriptionField)  "description" "description"
    , testFormat (FormatField True Nothing (Just 20) DescriptionField)   "description" "description"
    , testFormat (FormatField True (Just 20) Nothing DescriptionField)   "description" "description         "
    , testFormat (FormatField True (Just 20) (Just 20) DescriptionField) "description" "description         "
    , testFormat (FormatField True Nothing (Just 3) DescriptionField)    "description" "des"
    ]

parserTests = [
      testParser ""                             (defaultStringFormatStyle [])
    , testParser "D"                            (defaultStringFormatStyle [FormatLiteral "D"])
    , testParser "%(date)"                      (defaultStringFormatStyle [FormatField False Nothing Nothing DescriptionField])
    , testParser "%(total)"                     (defaultStringFormatStyle [FormatField False Nothing Nothing TotalField])
    , testParser "^%(total)"                    (TopAligned [FormatField False Nothing Nothing TotalField])
    , testParser "_%(total)"                    (BottomAligned [FormatField False Nothing Nothing TotalField])
    , testParser ",%(total)"                    (OneLine [FormatField False Nothing Nothing TotalField])
    , testParser "Hello %(date)!"               (defaultStringFormatStyle [FormatLiteral "Hello ", FormatField False Nothing Nothing DescriptionField, FormatLiteral "!"])
    , testParser "%-(date)"                     (defaultStringFormatStyle [FormatField True Nothing Nothing DescriptionField])
    , testParser "%20(date)"                    (defaultStringFormatStyle [FormatField False (Just 20) Nothing DescriptionField])
    , testParser "%.10(date)"                   (defaultStringFormatStyle [FormatField False Nothing (Just 10) DescriptionField])
    , testParser "%20.10(date)"                 (defaultStringFormatStyle [FormatField False (Just 20) (Just 10) DescriptionField])
    , testParser "%20(account) %.10(total)\n"   (defaultStringFormatStyle [FormatField False (Just 20) Nothing AccountField
                                                , FormatLiteral " "
                                                , FormatField False Nothing (Just 10) TotalField
                                                , FormatLiteral "\n"
                                                ])
  ]
