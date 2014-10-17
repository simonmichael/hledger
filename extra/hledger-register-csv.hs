#!/usr/bin/env runhaskell
{-|
hledger-register-csv [OPTIONS] [ARGS]

Show a register report as CSV.
-}

module Main
where

import Hledger.Cli
import Text.CSV


argsmode :: Mode RawOpts
argsmode = (defCommandMode ["register-csv"]) {
  modeHelp = "show matched postings and running total as CSV"
 ,modeGroupFlags = Group {
     groupNamed = [
         ("Input",inputflags)
        ,("Reporting",reportflags)
        ,("Misc",helpflags)
        ]
    ,groupUnnamed = []
    ,groupHidden = []
    }
 }

main :: IO ()
main = do
  opts <- getCliOpts argsmode
  withJournalDo opts $
    \CliOpts{reportopts_=ropts} j -> do
      d <- getCurrentDay
      putStrLn $ printCSV $ postingsReportAsCsv $ postingsReport ropts (queryFromOpts d ropts) j
    
postingsReportAsCsv :: PostingsReport -> CSV
postingsReportAsCsv (_,is) =
  ["date","description","account","amount","running total or balance"]
  :
  map postingsReportItemAsCsvRecord is

postingsReportItemAsCsvRecord :: PostingsReportItem -> Record
postingsReportItemAsCsvRecord (_, _, _, p, b) = [date,desc,acct,amt,bal]
  where
    date = showDate $ postingDate p
    desc = maybe "" tdescription $ ptransaction p
    acct = bracket $ paccount p
      where
        bracket = case ptype p of
                             BalancedVirtualPosting -> (\s -> "["++s++"]")
                             VirtualPosting -> (\s -> "("++s++")")
                             _ -> id
    amt = showMixedAmountWithoutPrice $ pamount p
    bal = showMixedAmountWithoutPrice b
