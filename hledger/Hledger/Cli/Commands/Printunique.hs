{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands.Printunique (
  printuniquemode
 ,printunique
) 
where

import Data.List
import Data.Ord
import Data.String.Here
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Print

printuniquemode = hledgerCommandMode
  [here| print-unique
Print transactions which do not reuse an already-seen description.

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

printunique opts j@Journal{jtxns=ts} = do
  print' opts j{jtxns=uniquify ts}
  where
    uniquify = nubBy (\t1 t2 -> thingToCompare t1 == thingToCompare t2) . sortBy (comparing thingToCompare)
    thingToCompare = tdescription
    -- thingToCompare = tdate
