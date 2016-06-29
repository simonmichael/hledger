#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package time
-}

{-
hledger-equity [HLEDGEROPTS] [QUERY]

Show a "closing balances" transaction that brings the balance of the
accounts matched by QUERY (or all accounts) to zero, and an opposite
"opening balances" transaction that restores the balances from zero.

The opening balances transaction is useful to carry over
asset/liability balances if you choose to start a new journal file,
eg at the beginning of the year.

The closing balances transaction is useful to zero out balances in
the old file, which gives you the option of reporting on both files
at once while still seeing correct balances.

Balances are calculated, and the opening transaction is dated, as of
the report end date, which you should specify with -e or date: (and
the closing transaction is dated one day earlier). If a report end
date is not specified, it defaults to today.

If any matched account directly contains multiple commodities, the
output may not be valid journal syntax, and will need some editing.

Example:
$ hledger equity -f 2015.journal -e 2016/1/1 assets liabilities >>2015.journal
move opening balances txn to 2016.journal

Open question: how to handle txns spanning a file boundary ? Eg:
2015/12/30 * food
    expenses:food:dining   $10
    assets:bank:checking  -$10  ; date:2016/1/4

This might or might not have some connection to the concept of
"closing the books" in accounting.

-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (fromMaybe)
import Data.Time.Calendar (addDays)
import Hledger.Cli

argsmode :: Mode RawOpts
argsmode = (defCommandMode ["equity"])
  { modeHelp = "print a journal entry posting the total balance of all accounts"
    ++ " (or the specified account and its subaccounts)"
    , modeGroupFlags = Group
      { groupNamed =
          -- XXX update to match hledger
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
        let ropts_ = ropts{accountlistmode_=ALFlat}
            q = queryFromOpts today ropts_
            (acctbals,_) = balanceReport ropts_ q j
            balancingamt = negate $ sum $ map (\((_,_,_),b) -> b) acctbals
            ps = [posting{paccount=a, pamount=b} | ((a,_,_),b) <- acctbals]
                 ++ [posting{paccount="equity:opening balances", pamount=balancingamt}]
            enddate = fromMaybe today $ queryEndDate (date2_ ropts_) q
            nps = [posting{paccount=a, pamount=negate b} | ((a,_,_),b) <- acctbals]
                 ++ [posting{paccount="equity:closing balances", pamount=negate balancingamt}]
        putStr $ showTransaction (nulltransaction{tdate=addDays (-1) enddate, tdescription="closing balances", tpostings=nps})
        putStr $ showTransaction (nulltransaction{tdate=enddate, tdescription="opening balances", tpostings=ps})
