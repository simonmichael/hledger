{-|

A ledger-compatible @print@ command.

-}

module Hledger.Cli.Print (
  printmode
 ,print'
 ,tests_Hledger_Cli_Print
)
where

import Data.List
import System.Console.CmdArgs.Explicit
import System.FilePath
import Test.HUnit
import Text.CSV

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)
import Hledger.Cli.Options


printmode = (defCommandMode $ ["print"] ++ aliases) {
  modeHelp = "show transaction entries" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
         flagReq  ["output","o"] (\s opts -> Right $ setopt "output" s opts) "[FILE][.FMT]" "write output to FILE (- or nothing means stdout). With a recognised FMT suffix, write that format (txt, csv)."
        ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = []

-- | Print journal transactions in standard format.
print' :: CliOpts -> Journal -> IO ()
print' opts@CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
  (path, ext) <- outputFilePathAndExtensionFromOpts opts
  let filename = fst $ splitExtension $ snd $ splitFileName path
      write  | filename `elem` ["","-"] && ext `elem` ["","csv","txt"] = putStr
             | otherwise                                               = writeFile path
      (render,ropts') | ext=="csv" = ((++"\n") . printCSV . entriesReportAsCsv, ropts{flat_=True})
                      | otherwise  = (entriesReportAsText, ropts)

  write $ render $ entriesReport ropts' q j

entriesReportAsText :: EntriesReport -> String
entriesReportAsText items = concatMap showTransactionUnelided items

-- XXX
-- tests_showTransactions = [
--   "showTransactions" ~: do

--    -- "print expenses" ~:
--    do
--     let opts = defreportopts{query_="expenses"}
--     d <- getCurrentDay
--     showTransactions opts (queryFromOpts d opts) samplejournal `is` unlines
--      ["2008/06/03 * eat & shop"
--      ,"    expenses:food                $1"
--      ,"    expenses:supplies            $1"
--      ,"    assets:cash                 $-2"
--      ,""
--      ]

--   -- , "print report with depth arg" ~:
--    do
--     let opts = defreportopts{depth_=Just 2}
--     d <- getCurrentDay
--     showTransactions opts (queryFromOpts d opts) samplejournal `is` unlines
--       ["2008/01/01 income"
--       ,"    assets:bank:checking            $1"
--       ,"    income:salary                  $-1"
--       ,""
--       ,"2008/06/01 gift"
--       ,"    assets:bank:checking            $1"
--       ,"    income:gifts                   $-1"
--       ,""
--       ,"2008/06/03 * eat & shop"
--       ,"    expenses:food                $1"
--       ,"    expenses:supplies            $1"
--       ,"    assets:cash                 $-2"
--       ,""
--       ,"2008/12/31 * pay off"
--       ,"    liabilities:debts               $1"
--       ,"    assets:bank:checking           $-1"
--       ,""
--       ]
--  ]

entriesReportAsCsv :: EntriesReport -> CSV
entriesReportAsCsv items =
  concat $
  ([["nth","date","date2","status","code","description","comment","account","amount","commodity","credit","debit","status","posting-comment"]]:).snd $
  mapAccumL (\n e -> (n + 1, transactionToCSV n e)) 0 items

transactionToCSV :: Integer -> Transaction -> CSV
transactionToCSV n t =
	map (\p -> show n:date:date2:status:code:description:comment:p)
	 (concatMap postingToCSV $ tpostings t)
	where
		description = tdescription t
		date = showDate (tdate t)
		date2 = maybe "" showDate (tdate2 t)
		status = if tstatus t then "*" else ""
		code = tcode t
		comment = chomp $ strip $ tcomment t

postingToCSV :: Posting -> CSV
postingToCSV p =
	map (\(a@(Amount {aquantity=q,acommodity=c})) ->
		let a_ = a{acommodity=""} in
		let amount = showAmount a_ in
		let commodity = c in
		let credit = if q < 0 then showAmount $ negate a_ else "" in
		let debit  = if q > 0 then showAmount a_ else "" in
		account:amount:commodity:credit:debit:status:comment:[])
	 amounts
	where
		Mixed amounts = pamount p
		status = if pstatus p then "*" else ""
		account = showAccountName Nothing (ptype p) (paccount p)
		comment = chomp $ strip $ pcomment p

tests_Hledger_Cli_Print = TestList []
  -- tests_showTransactions
