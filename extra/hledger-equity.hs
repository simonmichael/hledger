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

import Data.Maybe (fromMaybe)
import Hledger.Cli

argsmode :: Mode RawOpts
argsmode = (defCommandMode ["equity"])
  { modeHelp = "print a journal entry posting the total balance of all accounts"
    ++ " (or the specified account and its subaccounts)"
    , modeGroupFlags = Group
      { groupNamed =
         [ ("Input",inputflags)
         , ("Reporting",reportflags)
         , ("Misc",helpflags)
         ]
      , groupUnnamed = []
      , groupHidden = []
      }
    }

main :: IO ()
main = do
  opts <- getCliOpts argsmode
  withJournalDo opts $
   \CliOpts{reportopts_=ropts} j -> do
        today <- getCurrentDay
        let ropts_ = ropts{flat_=True}
            q = queryFromOpts today ropts_
            (acctbals,_) = balanceReport ropts_ q j
            balancingamt = negate $ sum $ map (\((_,_,_),b) -> b) acctbals
            ps = [posting{paccount=a, pamount=b} | ((a,_,_),b) <- acctbals]
                 ++ [posting{paccount="equity:opening balances", pamount=balancingamt}]
            enddate = fromMaybe today $ queryEndDate (date2_ ropts_) q
            nps = [posting{paccount=a, pamount=negate b} | ((a,_,_),b) <- acctbals]
                 ++ [posting{paccount="equity:closing balances", pamount=negate balancingamt}]
        putStr $ showTransactionUnelided (nulltransaction{tdate=enddate, tpostings=nps})
        putStr $ showTransactionUnelided (nulltransaction{tdate=enddate, tpostings=ps})
