#!/usr/bin/env runhaskell
{-

Like ledger's equity command, print a journal entry posting the total
balance of all accounts (or the specified account and its subaccounts)
in the default journal.

An entry like this is useful in the transition to a new journal file,
to zero out asset/liability balances in the old file and initialise
them in the new one. This way you get correct balances when reporting
on either file, and when including both files at once.

Usage: hledger-equity [ACCTPAT]
-}

import Hledger.Cli
import System.Environment

main = do
  putStrLn "(-f option not supported, see hledger-accountnames.hs for how to add it)"
  j <- defaultJournal
  d <- getCurrentDay
  args <- getArgs
  let query = Or $ map Acct args
      ropts = defreportopts{flat_=True}
      (acctbals,_) = balanceReport ropts query j
      balancingamt = negate $ sum $ map (\(_,_,_,b) -> b) acctbals
      ps = [posting{paccount=a, pamount=b} | (a,_,_,b) <- acctbals]
           ++ [posting{paccount="equity:opening balances", pamount=balancingamt}]
      txn = nulltransaction{tdate=d, tpostings=ps}
  putStr $ showTransactionUnelided txn
