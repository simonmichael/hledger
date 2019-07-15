{-|

A ledger-compatible @print@ command.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Print (
  printmode
 ,print'
 -- ,entriesReportAsText
 ,originalTransaction
)
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Console.CmdArgs.Explicit
import Hledger.Read.CsvReader (CSV, printCSV)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Hledger.Cli.Commands.Add ( transactionsSimilarTo )


printmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Print.txt")
  ([let arg = "STR" in
   flagReq  ["match","m"] (\s opts -> Right $ setopt "match" s opts) arg
    ("show the transaction whose description is most similar to "++arg++", and is most recent")
  ,flagNone ["explicit","x"] (setboolopt "explicit")
    "show all amounts explicitly"
  ,flagNone ["new"] (setboolopt "new")
    "show only newer-dated transactions added in each file since last run"
  ] ++ outputflags)
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Print journal transactions in standard format.
print' :: CliOpts -> Journal -> IO ()
print' opts j = do
  case maybestringopt "match" $ rawopts_ opts of
    Nothing   -> printEntries opts j
    Just desc -> printMatch opts j $ T.pack desc

printEntries :: CliOpts -> Journal -> IO ()
printEntries opts@CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
      fmt = outputFormatFromOpts opts
      (render, ropts') = case fmt of
        "csv"  -> ((++"\n") . printCSV . entriesReportAsCsv, ropts{accountlistmode_=ALFlat})
        "html" -> (const $ error' "Sorry, HTML output is not yet implemented for this kind of report.", ropts{accountlistmode_=ALFlat})  -- TODO
        _      -> (entriesReportAsText opts,                 ropts)
  writeOutput opts $ render $ entriesReport ropts' q j

entriesReportAsText :: CliOpts -> EntriesReport -> String
entriesReportAsText opts = concatMap (showTransactionUnelided . gettxn)
  where
    gettxn | useexplicittxn = id                   -- use fully inferred amounts & txn prices
           | otherwise      = originalTransaction  -- use original as-written amounts/txn prices
    -- Original vs inferred transactions/postings were causing problems here, disabling -B (#551).
    -- Use the explicit one if -B or -x are active.
    -- This passes tests; does it also mean -B sometimes shows missing amounts unnecessarily ?
    useexplicittxn = boolopt "explicit" (rawopts_ opts) || (valuationTypeIsCost $ reportopts_ opts)

-- Replace this transaction's postings with the original postings if any, but keep the
-- current possibly rewritten account names.
originalTransaction t = t { tpostings = map originalPostingPreservingAccount $ tpostings t }

-- Get the original posting if any, but keep the current possibly rewritten account name.
originalPostingPreservingAccount p = (originalPosting p) { paccount = paccount p }

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
entriesReportAsCsv txns =
  ["txnidx","date","date2","status","code","description","comment","account","amount","commodity","credit","debit","posting-status","posting-comment"] :
  concatMap transactionToCSV txns

-- | Generate one CSV record per posting, duplicating the common transaction fields.
-- The txnidx field (transaction index) allows postings to be grouped back into transactions.
transactionToCSV :: Transaction -> CSV
transactionToCSV t =
  map (\p -> show idx:date:date2:status:code:description:comment:p)
   (concatMap postingToCSV $ tpostings t)
  where
    idx = tindex t
    description = T.unpack $ tdescription t
    date = showDate (tdate t)
    date2 = maybe "" showDate (tdate2 t)
    status = show $ tstatus t
    code = T.unpack $ tcode t
    comment = chomp $ strip $ T.unpack $ tcomment t

postingToCSV :: Posting -> CSV
postingToCSV p =
  map (\(a@(Amount {aquantity=q,acommodity=c})) ->
    let a_ = a{acommodity=""} in
    let amount = showAmount a_ in
    let commodity = T.unpack c in
    let credit = if q < 0 then showAmount $ negate a_ else "" in
    let debit  = if q >= 0 then showAmount a_ else "" in
    [account, amount, commodity, credit, debit, status, comment])
   amounts
  where
    Mixed amounts = pamount p
    status = show $ pstatus p
    account = showAccountName Nothing (ptype p) (paccount p)
    comment = chomp $ strip $ T.unpack $ pcomment p

-- --match

-- | Print the transaction most closely and recently matching a description
-- (and the query, if any).
printMatch :: CliOpts -> Journal -> Text -> IO ()
printMatch CliOpts{reportopts_=ropts} j desc = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
  case similarTransaction' j q desc of
                Nothing -> putStrLn "no matches found."
                Just t  -> putStr $ showTransactionUnelided t

  where
    -- Identify the closest recent match for this description in past transactions.
    similarTransaction' :: Journal -> Query -> Text -> Maybe Transaction
    similarTransaction' j q desc
      | null historymatches = Nothing
      | otherwise           = Just $ snd $ head historymatches
      where
        historymatches = transactionsSimilarTo j q desc
