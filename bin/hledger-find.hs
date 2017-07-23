#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
   --package text
-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.String.Here
import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| find
Print transactions with their locations.
With a query, only matched transactions are shown.
Reads the default journal file, or another specified with -f.
FLAGS
  |]
  [] -- [flagNone ["strict"] (\opts -> setboolopt "strict" opts) "makes date comparing strict"] --
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $
   \CliOpts{rawopts_=_rawopts,reportopts_=ropts} j -> do
    d <- getCurrentDay
    let
      q = queryFromOpts d ropts
      ts = filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts ropts j
      location t = "; " ++ (showGenericSourcePos $ tsourcepos t) ++ "\n"
      showTransactionWithLocation t = location t ++ showTransactionUnelided t
    putStr $ unlines $ map showTransactionWithLocation ts
