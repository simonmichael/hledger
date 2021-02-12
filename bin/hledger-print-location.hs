#!/usr/bin/env stack
-- stack runghc --verbosity info --package hledger --package string-qq
-- Run from inside the hledger source tree, or compile with compile.sh.
-- See hledger-check-fancyassertions.hs.

{-
Quick script that adds file/line number tags to print output.
cf https://www.reddit.com/r/plaintextaccounting/comments/ddzn8o/finding_corresponding_journal_files_from_hledger/

$ hledger print-location -f examples/sample.journal desc:eat
2008/06/03 * eat & shop
    ; location: /Users/simon/src/hledger/examples/sample.journal:30
    expenses:food                  $1
    expenses:supplies              $1
    assets:cash
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.String.QQ (s)
import Data.Text (pack)
import Text.Printf
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [s| print-location
Like print, but adds tags showing the file path and location of transactions.
_FLAGS
  |]
  [] 
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $ \j -> 
    print' opts j{jtxns = map addLocationTag $ jtxns j}

addLocationTag :: Transaction -> Transaction
addLocationTag t = t{tcomment = tcomment t `commentAddTagNextLine` loctag}
  where
    loctag = ("location", pack $ showGenericSourcePosLine $ tsourcepos t)

-- Like showGenericSourcePos in Hledger.Data.Transaction, but show just the starting line number.
showGenericSourcePosLine :: GenericSourcePos -> String
showGenericSourcePosLine (GenericSourcePos f line _)         = printf "%s:%d" f line
showGenericSourcePosLine (JournalSourcePos f (startline, _)) = printf "%s:%d" f startline
