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
import qualified Data.Text as T
import Hledger.Cli.Script

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
    loctag = ("location", T.pack . sourcePosPairPretty $ tsourcepos t)

-- Like showSourcePosPair in Hledger.Data.Transaction, but show just the starting line number.
showSourcePosPairLine :: (SourcePos, SourcePos) -> String
showSourcePosPairLine (SourcePos f line _, _) = f ++ ":" ++ show line
