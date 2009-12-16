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
print' opts args = putStr . showTransactions opts args

showTransactions :: [Opt] -> [String] -> Ledger -> String
showTransactions opts args l = concatMap (showTransactionForPrint effective) txns
    where 
      txns = sortBy (comparing ltdate) $
               ledger_txns $ 
               filterJournalPostingsByDepth depth $ 
               filterJournalPostingsByAccount apats $ 
               journal l
      depth = depthFromOpts opts
      effective = Effective `elem` opts
      (apats,_) = parsePatternArgs args
