#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
-}

 {-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
 {-# LANGUAGE QuasiQuotes #-}

import Data.List
import Data.Ord
import Data.String.Here
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| print-unique
Remove transactions which reuse an already-seen description.

FLAGS

Example:
```shell
$ cat unique.journal
1/1 test
 (acct:one)  1
2/2 test
 (acct:two)  2
$ LEDGER_FILE=unique.journal hledger print-unique
(-f option not supported)
2015/01/01 test
    (acct:one)             1
```
  |]
  []
  [generalflagsgroup1]
  []
  ([], Nothing)
------------------------------------------------------------------------------

main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $
    \opts j@Journal{jtxns=ts} -> print' opts j{jtxns=uniquify ts}
    where
      uniquify = nubBy (\t1 t2 -> thingToCompare t1 == thingToCompare t2) . sortBy (comparing thingToCompare)
      thingToCompare = tdescription
      -- thingToCompare = tdate
