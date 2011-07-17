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

-- | Print journal transactions in standard format.
print' :: [Opt] -> [String] -> Journal -> IO ()
print' opts args j = do
  d <- getCurrentDay
  putStr $ showTransactions opts (optsToFilterSpec opts args d) j

showTransactions :: [Opt] -> FilterSpec -> Journal -> String
showTransactions opts fspec j = journalReportAsText opts fspec $ journalReport opts fspec j

journalReportAsText :: [Opt] -> FilterSpec -> JournalReport -> String
journalReportAsText opts _ items = concatMap (showTransactionForPrint effective) items
    where effective = Effective `elem` opts

