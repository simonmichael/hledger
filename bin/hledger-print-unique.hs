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
cmdmode = (defAddonCommandMode "print-unique") {
   modeHelp = [here|
Print only journal entries which are unique by description (or
something else). Reads the default or specified journal, or stdin.
  |]
  ,modeHelpSuffix=lines [here|
  |]
  }
------------------------------------------------------------------------------

main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $
    \opts j@Journal{jtxns=ts} -> print' opts j{jtxns=uniquify ts}
    where
      uniquify = nubBy (\t1 t2 -> thingToCompare t1 == thingToCompare t2) . sortBy (comparing thingToCompare)
      thingToCompare = tdescription
      -- thingToCompare = tdate
