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
import Hledger.Data
import Hledger.Cli.Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif


-- | A "journal report" is just a list of transactions.
type JournalReport = [JournalReportItem]

-- | The data for a single journal report item, representing one transaction.
type JournalReportItem = Transaction

-- | Print journal transactions in standard format.
print' :: [Opt] -> [String] -> Journal -> IO ()
print' opts args j = do
  t <- getCurrentLocalTime
  putStr $ showTransactions (optsToFilterSpec opts args t) j

showTransactions :: FilterSpec -> Journal -> String
showTransactions fspec j = journalReportAsText [] fspec $ journalReport [] fspec j

journalReportAsText :: [Opt] -> FilterSpec -> JournalReport -> String -- XXX unlike the others, this one needs fspec not opts
journalReportAsText _ fspec items = concatMap (showTransactionForPrint effective) items
    where effective = EffectiveDate == whichdate fspec

journalReport :: [Opt] -> FilterSpec -> Journal -> JournalReport
journalReport _ fspec j = sortBy (comparing tdate) $ jtxns $ filterJournalTransactions fspec j
