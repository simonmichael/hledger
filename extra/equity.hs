#!/usr/bin/env runhaskell
{-
Print an entry posting the total balance of the specified account and
subaccounts, or all accounts, from the default journal. Like ledger's
equity command. Useful when starting a new journal or closing the books.

Usage: equity.hs [ACCTPAT]
-}
import Hledger
import Hledger.Cli
import System.Environment

main = do
  j <- myJournal
  d <- getCurrentDay
  args <- getArgs
  let acctpat = head $ args ++ [""]
      (acctbals,_) = balanceReport [Flat] (optsToFilterSpec [] [acctpat] d) j
      txn = nulltransaction{
              tdate=d,
              tpostings=[nullposting{paccount=a,pamount=b} | (a,_,_,b) <- acctbals]
                         ++ [nullposting{paccount="equity:opening balances",pamount=missingamt}]}
  putStr $ show txn
