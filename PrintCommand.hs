{-| 

A ledger-compatible @print@ command.

-}

module PrintCommand
where
import Ledger
import Options


printcommandtests = TestList [
                    ]

-- | Print ledger entries in standard format.
print' :: [Opt] -> [String] -> Ledger -> IO ()
print' opts args l = putStr $ showEntries opts args l

showEntries :: [Opt] -> [String] -> Ledger -> String
showEntries opts args l = concatMap showEntry $ entries $ rawledger l
