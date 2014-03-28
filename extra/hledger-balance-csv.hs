#!/usr/bin/env runhaskell
{-|
hledger-balance-csv [OPTIONS] [ARGS]

Show a balance report as CSV.
-}

import Hledger.Cli
import Text.CSV


argsmode = 
  (defCommandMode ["balance-csv"]) {
     modeHelp = "show matched postings accounts and their balances as CSV"
    ,modeGroupFlags = Group {
       groupNamed = []
      ,groupUnnamed = inputflags ++ reportflags ++ helpflags
      ,groupHidden = []
      }
    }

main = getCliOpts argsmode >>= printBalanceCsv

printBalanceCsv opts = withJournalDo opts $
  \CliOpts{reportopts_=ropts} j -> do
    d <- getCurrentDay
    let (items,_) = balanceReport ropts (queryFromOpts d ropts) j
    putStrLn $ printCSV $
      ["account","balance"] :
      [[a, showMixedAmountWithoutPrice b] | ((a, _, _), b) <- items]
