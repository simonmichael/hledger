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
          matchtxn (Transaction _ _ desc acct _ _) = matchLedgerPatterns False apats acct
          apats = fst $ parseAccountDescriptionArgs args
          startingbalance = nullamt
          showTransactionsWithBalances' :: [Transaction] -> Transaction -> Amount -> [String]
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t@Transaction{amount=a}:ts) tprev b = 
              (if isZeroAmount a then [] else this) ++ rest
              where
                b' = b + (amount t)
                sameentry (Transaction {entryno=e1}) (Transaction {entryno=e2}) = e1 == e2
                this = if sameentry t tprev
                       then [showTransactionWithoutDescription t b']
                       else [showTransactionWithDescription t b']
                rest = showTransactionsWithBalances' ts t b'

showTransactionWithDescription :: Transaction -> Amount -> String
showTransactionWithDescription t b =
    (showEntryDescription $ Entry (date t) False "" (description t) "" [] "") 
    ++ (showTransactionFormatted t)
    ++ (showBalance b)

showTransactionWithoutDescription :: Transaction -> Amount -> String
showTransactionWithoutDescription t b = 
    (replicate 32 ' ') 
    ++ (showTransactionFormatted t) 
    ++ (showBalance b)

showTransactionFormatted :: Transaction -> String
showTransactionFormatted (Transaction eno d desc a amt ttype) = 
    showRawTransaction $ RawTransaction a amt "" ttype

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountOrZero b)

