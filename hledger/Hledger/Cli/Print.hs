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
print' :: CliOpts -> Journal -> IO ()
print' CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  putStr $ showTransactions ropts (optsToFilterSpec ropts d) j

showTransactions :: ReportOpts -> FilterSpec -> Journal -> String
showTransactions opts fspec j = entriesReportAsText opts fspec $ entriesReport opts fspec j

entriesReportAsText :: ReportOpts -> FilterSpec -> EntriesReport -> String
entriesReportAsText _ _ items = concatMap showTransactionUnelided items

