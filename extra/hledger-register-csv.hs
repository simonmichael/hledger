#!/usr/bin/env runhaskell
{-|
hledger-register-csv [OPTIONS] [ARGS]

Show a register report as CSV.
This includes one's total assets, liabilities, and net financial position as of each report
using the 'hledger balancesheet' logic rather than 'hledger balance' (the latter does not
indicate one's solvency).

For ease of parsing, in the latter columns, account names are separated from amounts by
a pipe delimiter. (One cannot treat spaces as delimiters because commodity names in hledger
may include spaces and so lead to ambiguity.)
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
      putStrLn $ printCSV $ (postingsReportAsCsv opts j) $ (postingsReport ropts (queryFromOpts d ropts) j)

postingsReportAsCsv :: CliOpts -> Journal -> PostingsReport -> CSV
postingsReportAsCsv o j (_,is) =
  ["Date","Description","Account","Amount","Assets","Liabilities","Net.Balance"]
  :
  map (postingsReportItemAsCsvRecord o j) is

postingsReportItemAsCsvRecord :: CliOpts -> Journal -> PostingsReportItem -> Record
postingsReportItemAsCsvRecord o j (_, _, _, p, _) = [date,desc,acct,amt,assets,liabilities,balance]
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
    ropts = reportopts_ o
    (assets,liabilities,balance) = balancesheet (postingDate p) o{reportopts_=ropts{format_=Just "%-(account)|%-(total)", flat_=True}} j
