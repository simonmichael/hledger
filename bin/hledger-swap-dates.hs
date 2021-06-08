#!/usr/bin/env stack
-- stack runghc --verbosity info --package hledger --package string-qq
-- Run from inside the hledger source tree, or compile with compile.sh.
-- See hledger-check-fancyassertions.hs.

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String.QQ (s)
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [s| swap-dates
Swap date and date2, on transactions which have date2 defined.
(Does not yet swap posting dates.)
_FLAGS
  |]
  [] 
  [generalflagsgroup1]
  []
  ([], Nothing) -- Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournalDo opts $
   \j -> do
    d <- getCurrentDay
    let
      q = rsQuery rspec
      ts = filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
      ts' = map transactionSwapDates ts
    mapM_ (T.putStrLn . showTransaction) ts'

transactionSwapDates :: Transaction -> Transaction
transactionSwapDates t@Transaction{tdate2=Nothing} = t
transactionSwapDates t@Transaction{tdate2=Just d} = t{tdate=d, tdate2=Just $ tdate t}
