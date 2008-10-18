{-| 

A ledger-compatible @register@ command.

-}

module RegisterCommand
where
import Ledger
import Options


-- | Print a register report.
register :: [Opt] -> [String] -> Ledger -> IO ()
register opts args l = putStr $ showRegisterReport opts args l

{- |
Generate the register report. Each ledger entry is displayed as two or
more lines like this:

@
date (10)  description (20)     account (22)            amount (11)  balance (12)
DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                ...                     ...         ...
@
-}
showRegisterReport :: [Opt] -> [String] -> Ledger -> String
showRegisterReport opts args l = showtxns ts nulltxn nullamt
    where
      ts = filter matchtxn $ ledgerTransactions l
      matchtxn Transaction{account=a} = matchLedgerPatterns False apats a
      apats = fst $ parseAccountDescriptionArgs args

      -- show transactions, one per line, keeping a running balance
      showtxns [] _ _ = ""
      showtxns (t@Transaction{amount=a}:ts) tprev bal =
          (if isZeroAmount a then "" else this) ++ showtxns ts t bal'
          where
            this = if t `issame` tprev
                   then showTransactionWithoutDescription t bal'
                   else showTransactionWithDescription t bal'
            issame t1 t2 = entryno t1 == entryno t2
            bal' = bal + amount t

showTransactionWithDescription :: Transaction -> Amount -> String
showTransactionWithDescription t b =
    (showEntryDescription $ Entry (date t) False "" (description t) "" [] "") 
    ++ (showTransactionFormatted t)
    ++ (showBalance b)
    ++ "\n"

showTransactionWithoutDescription :: Transaction -> Amount -> String
showTransactionWithoutDescription t b = 
    (replicate 32 ' ') 
    ++ (showTransactionFormatted t) 
    ++ (showBalance b)
    ++ "\n"

showTransactionFormatted :: Transaction -> String
showTransactionFormatted (Transaction eno d desc a amt ttype) = 
    showRawTransaction $ RawTransaction a amt "" ttype

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountOrZero b)

