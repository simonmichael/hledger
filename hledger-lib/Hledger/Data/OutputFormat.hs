{-# LANGUAGE FlexibleContexts #-}
module Hledger.Data.OutputFormat (
          parseStringFormat
        , formatsp
        , formatValue
        , OutputFormat(..)
        , HledgerFormatField(..)
        , tests
        ) where

import Numeric
import Data.Char (isPrint)
import Data.Maybe
import Test.HUnit
import Text.Parsec
import Text.Printf

import Hledger.Data.Types


formatValue :: Bool -> Maybe Int -> Maybe Int -> String -> String
formatValue leftJustified min max value = printf formatS value
    where
      l = if leftJustified then "-" else ""
      min' = maybe "" show min
      max' = maybe "" (\i -> "." ++ (show i)) max
      formatS = "%" ++ l ++ min' ++ max' ++ "s"

parseStringFormat :: String -> Either String [OutputFormat]
parseStringFormat input = case (runParser formatsp () "(unknown)") input of
    Left y -> Left $ show y
    Right x -> Right x

{-
Parsers
-}

field :: Stream [Char] m Char => ParsecT [Char] st m HledgerFormatField
field = do
        try (string "account" >> return AccountField)
    <|> try (string "depth_spacer" >> return DepthSpacerField)
    <|> try (string "date" >> return DescriptionField)
    <|> try (string "description" >> return DescriptionField)
    <|> try (string "total" >> return TotalField)
    <|> try (many1 digit >>= (\s -> return $ FieldNo $ read s))

formatField :: Stream [Char] m Char => ParsecT [Char] st m OutputFormat
formatField = do
    char '%'
    leftJustified <- optionMaybe (char '-')
    minWidth <- optionMaybe (many1 $ digit)
    maxWidth <- optionMaybe (do char '.'; many1 $ digit) -- TODO: Can this be (char '1') *> (many1 digit)
    char '('
    f <- field
    char ')'
    return $ FormatField (isJust leftJustified) (parseDec minWidth) (parseDec maxWidth) f
    where
      parseDec s = case s of
        Just text -> Just m where ((m,_):_) = readDec text
        _ -> Nothing

formatLiteral :: Stream [Char] m Char => ParsecT [Char] st m OutputFormat
formatLiteral = do
    s <- many1 c
    return $ FormatLiteral s
    where
      isPrintableButNotPercentage x = isPrint x && (not $ x == '%')
      c =     (satisfy isPrintableButNotPercentage <?> "printable character")
          <|> try (string "%%" >> return '%')

formatp :: Stream [Char] m Char => ParsecT [Char] st m OutputFormat
formatp =
        formatField
    <|> formatLiteral

formatsp :: Stream [Char] m Char => ParsecT [Char] st m [OutputFormat]
formatsp = many formatp

testFormat :: OutputFormat -> String -> String -> Assertion
testFormat fs value expected = assertEqual name expected actual
    where
        (name, actual) = case fs of
            FormatLiteral l -> ("literal", formatValue False Nothing Nothing l)
            FormatField leftJustify min max _ -> ("field", formatValue leftJustify min max value)

testParser :: String -> [OutputFormat] -> Assertion
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
      testParser ""                             []
    , testParser "D"                            [FormatLiteral "D"]
    , testParser "%(date)"                      [FormatField False Nothing Nothing DescriptionField]
    , testParser "%(total)"                     [FormatField False Nothing Nothing TotalField]
    , testParser "Hello %(date)!"               [FormatLiteral "Hello ", FormatField False Nothing Nothing DescriptionField, FormatLiteral "!"]
    , testParser "%-(date)"                     [FormatField True Nothing Nothing DescriptionField]
    , testParser "%20(date)"                    [FormatField False (Just 20) Nothing DescriptionField]
    , testParser "%.10(date)"                   [FormatField False Nothing (Just 10) DescriptionField]
    , testParser "%20.10(date)"                 [FormatField False (Just 20) (Just 10) DescriptionField]
    , testParser "%20(account) %.10(total)\n"   [ FormatField False (Just 20) Nothing AccountField
                                                , FormatLiteral " "
                                                , FormatField False Nothing (Just 10) TotalField
                                                , FormatLiteral "\n"
                                                ]
  ]
