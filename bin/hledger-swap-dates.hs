#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
   --package text
-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String.Here
import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| swap-dates
Swap date and date2, on transactions which have date2 defined.
(Does not yet swap posting dates.)
FLAGS
  |]
  [] 
  [generalflagsgroup1]
  []
  ([], Nothing) -- Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts@CliOpts{reportopts_=ropts} <- getHledgerCliOpts cmdmode
  withJournalDo opts $
   \j -> do
    d <- getCurrentDay
    let
      q = queryFromOpts d ropts
      ts = filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts ropts j
      ts' = map transactionSwapDates ts
    mapM_ (putStrLn . showTransactionUnelided) ts'

transactionSwapDates :: Transaction -> Transaction
transactionSwapDates t@Transaction{tdate2=Nothing} = t
transactionSwapDates t@Transaction{tdate2=Just d} = t{tdate=d, tdate2=Just $ tdate t}
