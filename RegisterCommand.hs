{-| 

A ledger-compatible @register@ command.

-}

module RegisterCommand
where
import Ledger
import Options


-- | Print a register report.
register :: [Opt] -> [String] -> Ledger -> IO ()
register opts args l = putStr $ showTransactionsWithBalances opts args l

showTransactionsWithBalances :: [Opt] -> [String] -> Ledger -> String
showTransactionsWithBalances opts args l =
    unlines $ showTransactionsWithBalances' ts nulltxn startingbalance
        where
          ts = filter matchtxn $ ledgerTransactions l
          matchtxn (Transaction _ _ desc acct _) = matchLedgerPatterns False apats acct
          pats@(apats,dpats) = parseAccountDescriptionArgs args
          startingbalance = nullamt
          showTransactionsWithBalances' :: [Transaction] -> Transaction -> Amount -> [String]
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t:ts) tprev b =
              (if sameentry t tprev
               then [showTransactionAndBalance t b']
               else [showTransactionDescriptionAndBalance t b'])
              ++ (showTransactionsWithBalances' ts t b')
                  where 
                    b' = b + (amount t)
                    sameentry (Transaction e1 _ _ _ _) (Transaction e2 _ _ _ _) = e1 == e2

showTransactionDescriptionAndBalance :: Transaction -> Amount -> String
showTransactionDescriptionAndBalance t b =
    (showEntryDescription $ Entry (date t) False "" (description t) "" [] "") 
    ++ (showLedgerTransaction $ RawTransaction (account t) (amount t) "") ++ (showBalance b)

showTransactionAndBalance :: Transaction -> Amount -> String
showTransactionAndBalance t b =
    (replicate 32 ' ') ++ (showLedgerTransaction $ RawTransaction (account t) (amount t) "") ++ (showBalance b)

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountOrZero b)

