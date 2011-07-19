{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @print@ command.

-}

module Hledger.Cli.Print (
  print'
 ,showTransactions
) where
import Data.List

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8 (putStr)
import Hledger.Cli.Options
import Hledger.Cli.Reports

-- | Print journal transactions in standard format.
print' :: [Opt] -> [String] -> Journal -> IO ()
print' opts args j = do
  d <- getCurrentDay
  putStr $ showTransactions opts (optsToFilterSpec opts args d) j

showTransactions :: [Opt] -> FilterSpec -> Journal -> String
showTransactions opts fspec j = rawJournalReportAsText opts fspec $ rawJournalReport opts fspec j

rawJournalReportAsText :: [Opt] -> FilterSpec -> RawJournalReport -> String
rawJournalReportAsText opts _ items = concatMap (showTransactionForPrint effective) items
    where effective = Effective `elem` opts

