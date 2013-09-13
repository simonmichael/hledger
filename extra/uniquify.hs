#!/usr/bin/env runhaskell
{-|
Uniquify journal entries based on some id in the description. Reads the
default or specified journal, or stdin.

Usage: uniquify.hs [-f JOURNALFILE | -f-]
|-}

import Data.List
import Hledger
import Hledger.Cli

main = do
  opts <- getHledgerOpts
  withJournalDo opts uniquifyAndPrint

uniquifyAndPrint :: CliOpts -> Journal -> IO ()
uniquifyAndPrint opts j@Journal{jtxns=ts} = print' opts j{jtxns=uniquify ts}
    where
      uniquify = nubBy (\t1 t2 -> extractId (tdescription t1) == extractId (tdescription t2))
      extractId desc = desc -- extract some part that's supposed to be unique
