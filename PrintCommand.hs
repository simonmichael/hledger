{-| 

A ledger-compatible @print@ command.

-}

module PrintCommand
where
import Ledger
import Options


-- | Print ledger transactions in standard format.
print' :: [Opt] -> [String] -> Ledger -> IO ()
print' opts args l = putStr $ showLedgerTransactions opts args l

showLedgerTransactions :: [Opt] -> [String] -> Ledger -> String
showLedgerTransactions opts args l = concatMap showLedgerTransaction $ filteredtxns
    where 
      filteredtxns = ledger_txns $ 
                        filterRawLedgerPostingsByDepth depth $ 
                        filterRawLedgerTransactionsByAccount apats $ 
                        rawledger l
      depth = depthFromOpts opts
      (apats,_) = parseAccountDescriptionArgs opts args
