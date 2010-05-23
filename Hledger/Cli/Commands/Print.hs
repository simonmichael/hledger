{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @print@ command.

-}

module Hledger.Cli.Commands.Print
where
import Hledger.Data
import Hledger.Cli.Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif


-- | Print ledger transactions in standard format.
print' :: [Opt] -> [String] -> Journal -> IO ()
print' opts args j = do
  t <- getCurrentLocalTime
  putStr $ showTransactions (optsToFilterSpec opts args t) j

showTransactions :: FilterSpec -> Journal -> String
showTransactions filterspec j =
    concatMap (showTransactionForPrint effective) $ sortBy (comparing tdate) txns
        where
          effective = EffectiveDate == whichdate filterspec
          txns = jtxns $ filterJournalTransactions filterspec j
