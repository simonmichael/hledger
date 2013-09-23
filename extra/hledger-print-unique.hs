#!/usr/bin/env runhaskell
{-|
hledger-print-unique [-f JOURNALFILE | -f-]

Print only journal entries which are unique by description (or
something else). Reads the default or specified journal, or stdin.

|-}

import Data.List
import Data.Ord
import Hledger
import Hledger.Cli

main = do
  opts <- getCliOpts (defCommandMode ["hledger-print-unique"])
  withJournalDo opts $
    \opts j@Journal{jtxns=ts} -> print' opts j{jtxns=uniquify ts}
    where 
      uniquify = nubBy (\t1 t2 -> thingToCompare t1 == thingToCompare t2) . sortBy (comparing thingToCompare)
      thingToCompare = tdescription
      -- thingToCompare = tdate
