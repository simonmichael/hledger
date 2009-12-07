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
print' opts args = putStr . showLedgerTransactions opts args

showLedgerTransactions :: [Opt] -> [String] -> Ledger -> String
showLedgerTransactions opts args l = concatMap (showLedgerTransactionForPrint effective) txns
    where 
      txns = sortBy (comparing ltdate) $
               ledger_txns $ 
               filterRawLedgerPostingsByDepth depth $ 
               filterRawLedgerTransactionsByAccount apats $ 
               rawledger l
      depth = depthFromOpts opts
      effective = Effective `elem` opts
      (apats,_) = parsePatternArgs args
