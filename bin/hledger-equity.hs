#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
   --package time
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Maybe
import Data.String.Here
import Data.Time.Calendar
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode :: Mode RawOpts
cmdmode = hledgerCommandMode
  [here| equity
Print a "closing balances" transaction that brings all accounts (or with
query arguments, just the matched accounts) to a zero balance, followed by an
opposite "opening balances" transaction that restores the balances from zero.
Such transactions can be useful, eg, for bringing account balances across
file boundaries.

FLAGS

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

Example:
```shell
$ hledger equity -f 2015.journal -e 2016/1/1 assets liabilities >>2015.journal
# move the opening balances transaction to 2016.journal
$ hledger -f 2015.journal bal assets liabilities not:desc:closing # shows correct 2015 balances
$ hledger -f 2016.journal bal assets liabilities                  # shows correct 2016 balances
$ hledger -f 2015.journal -f 2016.journal bal assets liabilities  # still shows correct 2016 balances
```
Open question: how to handle txns spanning a file boundary ? Eg:
```journal
2015/12/30 * food
    expenses:food:dining   $10
    assets:bank:checking  -$10  ; date:2016/1/4
```
This command might or might not have some connection to the concept of
"closing the books" in accounting.
  |]
  []
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $
   \CliOpts{reportopts_=ropts} j -> do
        today <- getCurrentDay
        let ropts_ = ropts{accountlistmode_=ALFlat}
            q = queryFromOpts today ropts_
            (acctbals,_) = balanceReport ropts_ q j
            balancingamt = negate $ sum $ map (\(_,_,_,b) -> normaliseMixedAmountSquashPricesForDisplay b) acctbals
            ps = [posting{paccount=a
                         ,pamount=mixed [b]
                         ,pbalanceassertion=Just b
                         }
                 |(a,_,_,mb) <- acctbals
                 ,b <- amounts $ normaliseMixedAmountSquashPricesForDisplay mb
                 ]
                 ++ [posting{paccount="equity:opening balances", pamount=balancingamt}]
            enddate = fromMaybe today $ queryEndDate (date2_ ropts_) q
            nps = [posting{paccount=a
                          ,pamount=mixed [negate b]
                          ,pbalanceassertion=Just b{aquantity=0}
                          }
                  |(a,_,_,mb) <- acctbals
                  ,b <- amounts $ normaliseMixedAmountSquashPricesForDisplay mb
                  ]
                 ++ [posting{paccount="equity:closing balances", pamount=negate balancingamt}]
        putStr $ showTransaction (nulltransaction{tdate=addDays (-1) enddate, tdescription="closing balances", tpostings=nps})
        putStr $ showTransaction (nulltransaction{tdate=enddate, tdescription="opening balances", tpostings=ps})
