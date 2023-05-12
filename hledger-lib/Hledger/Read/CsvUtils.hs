--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
{-|

CSV utilities.

-}

--- ** language
{-# LANGUAGE OverloadedStrings    #-}

--- ** exports
module Hledger.Read.CsvUtils (
  CSV, CsvRecord, CsvValue,
  printCSV,
  -- * Tests
  tests_CsvUtils,
)
where

--- ** imports
import Prelude hiding (Applicative(..))
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

import Hledger.Utils

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

type CSV       = [CsvRecord]
type CsvRecord = [CsvValue]
type CsvValue  = Text

printCSV :: [CsvRecord] -> TL.Text
printCSV = TB.toLazyText . unlinesB . map printRecord
    where printRecord = foldMap TB.fromText . intersperse "," . map printField
          printField = wrap "\"" "\"" . T.replace "\"" "\"\""

--- ** tests

tests_CsvUtils :: TestTree
tests_CsvUtils = testGroup "CsvUtils" [
  ]

