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

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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
  ,outputFormatFlag ["txt","csv","json"]
  ,outputFileFlag
  ])
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
      render = case fmt of
        "txt"  -> entriesReportAsText opts
        "csv"  -> (++"\n") . printCSV . entriesReportAsCsv
        "json" -> (++"\n") . TL.unpack . toJsonText
        _      -> const $ error' $ unsupportedOutputFormatError fmt
  writeOutput opts $ render $ entriesReport ropts q j

entriesReportAsText :: CliOpts -> EntriesReport -> String
entriesReportAsText opts = concatMap (showTransaction . whichtxn)
  where
    whichtxn
      -- With -x, use the fully-inferred txn with all amounts & txn prices explicit.
      | boolopt "explicit" (rawopts_ opts)
        -- Or also, if any of -B/-V/-X/--value are active.
        -- Because of #551, and because of print -V valuing only one
        -- posting when there's an implicit txn price.
        -- So -B/-V/-X/--value implies -x. Is this ok ?
        || (isJust $ value_ $ reportopts_ opts) = id
      -- By default, use the original as-written-in-the-journal txn.
      | otherwise = originalTransaction

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
    -- commodity goes into separate column, so we suppress it, along with digit group
    -- separators and prices
    let a_ = a{acommodity="",astyle=(astyle a){asdigitgroups=Nothing},aprice=Nothing} in
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
                Just t  -> putStr $ showTransaction t

  where
    -- Identify the closest recent match for this description in past transactions.
    similarTransaction' :: Journal -> Query -> Text -> Maybe Transaction
    similarTransaction' j q desc
      | null historymatches = Nothing
      | otherwise           = Just $ snd $ head historymatches
      where
        historymatches = transactionsSimilarTo j q desc
