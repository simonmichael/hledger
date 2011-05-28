{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @print@ command.

-}

module Hledger.Cli.Print (
  JournalReport
 ,JournalReportItem
 ,print'
 ,journalReport
 ,showTransactions
) where
import Data.List
import Data.Ord

import Hledger.Cli.Options
import Hledger.Data
import Hledger.Utils
import Prelude hiding (putStr)
import Hledger.Utils.UTF8 (putStr)


-- | A "journal report" is just a list of transactions.
type JournalReport = [JournalReportItem]

-- | The data for a single journal report item, representing one transaction.
type JournalReportItem = Transaction

-- | Print journal transactions in standard format.
print' :: [Opt] -> [String] -> Journal -> IO ()
print' opts args j = do
  t <- getCurrentLocalTime
  let j' = journalSelectingDate (whichDateFromOpts opts) j
  putStr $ showTransactions opts (optsToFilterSpec opts args t) j'

showTransactions :: [Opt] -> FilterSpec -> Journal -> String
showTransactions opts fspec j = journalReportAsText opts fspec $ journalReport [] fspec j

journalReportAsText :: [Opt] -> FilterSpec -> JournalReport -> String
journalReportAsText opts _ items = concatMap (showTransactionForPrint effective) items
    where effective = Effective `elem` opts

journalReport :: [Opt] -> FilterSpec -> Journal -> JournalReport
journalReport _ fspec j = sortBy (comparing tdate) $ jtxns $ filterJournalTransactions fspec j
