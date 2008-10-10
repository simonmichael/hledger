{-| 

A ledger-compatible @register@ command.

-}

module RegisterCommand
where
import Ledger
import Options


registercommandtests = TestList [
                       ]

-- | Print a register report.
printregister :: [Opt] -> [String] -> Ledger -> IO ()
printregister opts args l = putStr $ showTransactionsWithBalances txns startingbalance
    where
      txns = sortBy (comparing date) $ ledgerTransactions l
      startingbalance = nullamt{precision=lprecision l}
