{-| 

A ledger-compatible @print@ command.

-}

module Commands.Print
where
import Prelude hiding (putStr)
import Ledger
import Options
import System.IO.UTF8


-- | Print ledger transactions in standard format.
print' :: [Opt] -> [String] -> Ledger -> IO ()
print' opts args l = do
  t <- getCurrentLocalTime
  putStr $ showTransactions (optsToFilterSpec opts args t) l

showTransactions :: FilterSpec -> Ledger -> String
showTransactions filterspec l =
    concatMap (showTransactionForPrint effective) $ sortBy (comparing tdate) txns
        where
          effective = EffectiveDate == whichdate filterspec
          txns = jtxns $ filterJournalTransactions filterspec $ journal l
