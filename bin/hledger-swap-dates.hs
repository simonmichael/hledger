#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
   --package text
-}

{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}

import Data.List
import Data.String.Here
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| swap-dates
Swap date and date2, on transactions and postings which have date2 defined.
FLAGS
  |]
  [] 
  [generalflagsgroup1]
  []
  ([], Nothing) -- Just $ argsFlag "[QUERY]")
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
      tags = nub $ sort $ map fst $ concatMap transactionAllTags ts
    mapM_ T.putStrLn tags
    -- replace txns with date-swapped txns
    -- print txns
