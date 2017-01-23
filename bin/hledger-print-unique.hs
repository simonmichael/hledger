#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
-}

{-# LANGUAGE QuasiQuotes #-}

import Data.List
import Data.Ord
import Data.String.Here
import Hledger.Cli

------------------------------------------------------------------------------
doc = [here|

Usage:
```
$ hledger-print-unique -h
hledger-print-unique [OPTIONS] [ARGS]

...common hledger options...
```

Print only journal entries which are unique by description (or
something else). Reads the default or specified journal, or stdin.

|]
------------------------------------------------------------------------------

main = do
  putStrLn "(-f option not supported)"
  opts <- getHledgerOptsOrShowHelp (defAddonCommandMode "hledger-print-unique") doc
  withJournalDo opts $
    \opts j@Journal{jtxns=ts} -> print' opts j{jtxns=uniquify ts}
    where
      uniquify = nubBy (\t1 t2 -> thingToCompare t1 == thingToCompare t2) . sortBy (comparing thingToCompare)
      thingToCompare = tdescription
      -- thingToCompare = tdate
