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
printentries :: [Opt] -> [String] -> Ledger -> IO ()
printentries opts args l = putStr $ showEntries $ setprecisions $ entries $ rawledger l
    where setprecisions = map (entrySetPrecision (lprecision l))
