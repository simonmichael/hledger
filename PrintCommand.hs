{-| 

A ledger-compatible @print@ command.

-}

module PrintCommand
where
import Ledger
import Options


-- | Print ledger entries in standard format.
print' :: [Opt] -> [String] -> Ledger -> IO ()
print' opts args l = putStr $ showEntries opts args l

showEntries :: [Opt] -> [String] -> Ledger -> String
showEntries opts args l = concatMap showEntry $ filteredentries
    where 
      filteredentries = entries $ filterRawLedgerEntriesByAccount apats $ rawledger l
      (apats,_) = parseAccountDescriptionArgs args
