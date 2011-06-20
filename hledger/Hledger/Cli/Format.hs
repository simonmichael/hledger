module Hledger.Cli.Format (
          parseFormatString
        , formatValue
        , FormatString(..)
        , Field(..)
        , tests
        ) where

import Numeric
import Data.Maybe
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

{-
%[-][MIN WIDTH][.MAX WIDTH]EXPR

%-P     a transaction's payee, left justified
%20P    The same, right justified, at least 20 chars wide
%.20P   The same, no more than 20 chars wide
%-.20P  Left justified, maximum twenty chars wide
-}

data Field =
    Account
  | DefaultDate
  | Description
  | Total
  | DepthSpacer
    deriving (Show, Eq)

data FormatString =
    FormatLiteral String
  | FormatField 
    Bool            -- Left justified 
    (Maybe Int)     -- Min width
    (Maybe Int)     -- Max width
    Field           -- Field
    deriving (Show, Eq)

formatValue :: Bool -> Maybe Int -> Maybe Int -> String -> String
formatValue leftJustified min max value = printf formatS value
    where
      l = if leftJustified then "-" else ""
      min' = maybe "" show min
      max' = maybe "" (\i -> "." ++ (show i)) max
      formatS = "%" ++ l ++ min' ++ max' ++ "s"

parseFormatString :: String -> Either String [FormatString]
parseFormatString input = case parse formatStrings "(unknown)" input of
    Left y -> Left $ show y
    Right x -> Right x

{-
Parsers
-}

field :: Parser Field
field = do
        try (string "account" >> return Account)
--    <|> try (string "date" >> return DefaultDate)
--    <|> try (string "description" >> return Description)
    <|> try (string "depth_spacer" >> return DepthSpacer)
    <|> try (string "total" >> return Total)

formatField :: Parser FormatString
formatField = do
    char '%'
    leftJustified <- optionMaybe (char '-')
    minWidth <- optionMaybe (many1 $ digit)
    maxWidth <- optionMaybe (do char '.'; many1 $ digit)
    char '('
    field <- field
    char ')'
    return $ FormatField (isJust leftJustified) (parseDec minWidth) (parseDec maxWidth) field
    where
      parseDec s = case s of
        Just text -> Just m where ((m,_):_) = readDec text
        _ -> Nothing

formatLiteral :: Parser FormatString
formatLiteral = do
    s <- many1 c
    return $ FormatLiteral s
    where
      c =     noneOf "%"
          <|> try (string "%%" >> return '%')

formatString :: Parser FormatString
formatString =
        formatField
    <|> formatLiteral

formatStrings = many formatString

testFormat :: FormatString -> String -> String -> Assertion
testFormat fs value expected = assertEqual name expected actual
    where
        (name, actual) = case fs of
            FormatLiteral l -> ("literal", formatValue False Nothing Nothing l)
            FormatField leftJustify min max _ -> ("field", formatValue leftJustify min max value)

testParser :: String -> [FormatString] -> Assertion
testParser s expected = case (parseFormatString s) of
    Left  error -> assertFailure $ show error
    Right actual -> assertEqual ("Input: " ++ s) expected actual

tests = test [ formattingTests ++ parserTests ]

formattingTests = [
      testFormat (FormatLiteral " ")                                ""            " "
    , testFormat (FormatField False Nothing Nothing Description)    "description" "description"
    , testFormat (FormatField False (Just 20) Nothing Description)  "description" "         description"
    , testFormat (FormatField False Nothing (Just 20) Description)  "description" "description"
    , testFormat (FormatField True Nothing (Just 20) Description)   "description" "description"
    , testFormat (FormatField True (Just 20) Nothing Description)   "description" "description         "
    , testFormat (FormatField True (Just 20) (Just 20) Description) "description" "description         "
    , testFormat (FormatField True Nothing (Just 3) Description)    "description" "des"
    ]

parserTests = [
      testParser ""                             []
    , testParser "D"                            [FormatLiteral "D"]
    , testParser "%(date)"                      [FormatField False Nothing Nothing Description]
    , testParser "%(total)"                     [FormatField False Nothing Nothing Total]
    , testParser "Hello %(date)!"               [FormatLiteral "Hello ", FormatField False Nothing Nothing Description, FormatLiteral "!"]
    , testParser "%-(date)"                     [FormatField True Nothing Nothing Description]
    , testParser "%20(date)"                    [FormatField False (Just 20) Nothing Description]
    , testParser "%.10(date)"                   [FormatField False Nothing (Just 10) Description]
    , testParser "%20.10(date)"                 [FormatField False (Just 20) (Just 10) Description]
    , testParser "%20(account) %.10(total)\n"   [ FormatField False (Just 20) Nothing Account
                                                , FormatLiteral " "
                                                , FormatField False Nothing (Just 10) Total
                                                , FormatLiteral "\n"
                                                ]
  ]
