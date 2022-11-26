{-|

A ledger-compatible @print@ command.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Print (
  printmode
 ,print'
 -- ,entriesReportAsText
 ,originalTransaction
)
where

import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Lens.Micro ((^.), _Just, has)
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Read.CsvReader (CSV, printCSV)
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import System.Exit (exitFailure)


printmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Print.txt")
  ([let arg = "DESC" in
   flagReq  ["match","m"] (\s opts -> Right $ setopt "match" s opts) arg
    ("fuzzy search for the transaction with description closest to "++arg++", and also most recent")
  ,flagNone ["explicit","x"] (setboolopt "explicit")
    "show all amounts explicitly"
  ,flagNone ["show-costs"] (setboolopt "show-costs")
    "show transaction prices even with conversion postings"
  ,flagNone ["new"] (setboolopt "new")
    "show only newer-dated transactions added in each file since last run"
  ,outputFormatFlag ["txt","csv","json","sql"]
  ,outputFileFlag
  ])
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Print journal transactions in standard format.
print' :: CliOpts -> Journal -> IO ()
print' opts j = do
  -- The print command should show all amounts with their original decimal places,
  -- but as part of journal reading the posting amounts have already been normalised
  -- according to commodity display styles, and currently it's not easy to avoid
  -- that. For now we try to reverse it by increasing all amounts' decimal places 
  -- sufficiently to show the amount exactly. The displayed amounts may have minor
  -- differences from the originals, such as trailing zeroes added.
  let j' = journalMapPostingAmounts mixedAmountSetFullPrecision j
  case maybestringopt "match" $ rawopts_ opts of
    Nothing   -> printEntries opts j'
    Just desc -> 
      case journalSimilarTransaction opts j' (dbg1 "finding best match for description" $ T.pack desc) of
        Just t  -> printEntries opts j'{jtxns=[t]}
        Nothing -> putStrLn "no matches found." >> exitFailure

printEntries :: CliOpts -> Journal -> IO ()
printEntries opts@CliOpts{reportspec_=rspec} j =
  writeOutputLazyText opts . render $ entriesReport rspec j
  where
    fmt = outputFormatFromOpts opts
    render | fmt=="txt"  = entriesReportAsText opts
           | fmt=="csv"  = printCSV . entriesReportAsCsv
           | fmt=="json" = toJsonText
           | fmt=="sql"  = entriesReportAsSql
           | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:

entriesReportAsText :: CliOpts -> EntriesReport -> TL.Text
entriesReportAsText opts =
    TB.toLazyText . foldMap (TB.fromText . showTransaction . whichtxn)
  where
    whichtxn
      -- With -x, use the fully-inferred txn with all amounts & txn prices explicit.
      | boolopt "explicit" (rawopts_ opts) = id
      -- With --show-costs, make txn prices explicit.
      | opts ^. infer_costs = id
      -- Or also, if any of -B/-V/-X/--value are active.
      -- Because of #551, and because of print -V valuing only one
      -- posting when there's an implicit txn price.
      -- So -B/-V/-X/--value implies -x. Is this ok ?
      | has (value . _Just) opts = id
      -- By default, use the original as-written-in-the-journal txn.
      | otherwise = originalTransaction

-- Replace this transaction's postings with the original postings if any, but keep the
-- current possibly rewritten account names, and the inferred values of any auto postings
originalTransaction t = t { tpostings = map originalPostingPreservingAccount $ tpostings t }

-- Get the original posting if any, but keep the current possibly rewritten account name, and
-- the inferred values of any auto postings
originalPostingPreservingAccount p = orig
    { paccount = paccount p
    , pamount = pamount $ if isGenerated then p else orig }
  where
    orig = originalPosting p
    isGenerated = "generated-posting" `elem` map fst (ptags p)

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

entriesReportAsSql :: EntriesReport -> TL.Text
entriesReportAsSql txns = TB.toLazyText $ mconcat
    [ TB.fromText "create table if not exists postings(id serial,txnidx int,date1 date,date2 date,status text,code text,description text,comment text,account text,amount numeric,commodity text,credit numeric,debit numeric,posting_status text,posting_comment text);\n"
    , TB.fromText "insert into postings(txnidx,date1,date2,status,code,description,comment,account,amount,commodity,credit,debit,posting_status,posting_comment) values\n"
    , mconcat . intersperse (TB.fromText ",") $ map values csv
    , TB.fromText ";\n"
    ]
  where
    values vs = TB.fromText "(" <> mconcat (intersperse (TB.fromText ",") $ map toSql vs) <> TB.fromText ")\n"
    toSql "" = TB.fromText "NULL"
    toSql s  = TB.fromText "'" <> TB.fromText (T.replace "'" "''" s) <> TB.fromText "'"
    csv = concatMap (transactionToCSV . transactionMapPostingAmounts (mapMixedAmount setDecimalPoint)) txns
      where
        setDecimalPoint a = a{astyle=(astyle a){asdecimalpoint=Just '.'}}

entriesReportAsCsv :: EntriesReport -> CSV
entriesReportAsCsv txns =
  ["txnidx","date","date2","status","code","description","comment","account","amount","commodity","credit","debit","posting-status","posting-comment"] :
  concatMap transactionToCSV txns

-- | Generate one CSV record per posting, duplicating the common transaction fields.
-- The txnidx field (transaction index) allows postings to be grouped back into transactions.
transactionToCSV :: Transaction -> CSV
transactionToCSV t =
  map (\p -> T.pack (show idx):d:d2:status:code:description:comment:p)
   (concatMap postingToCSV $ tpostings t)
  where
    idx = tindex t
    description = tdescription t
    d = showDate (tdate t)
    d2 = maybe "" showDate $ tdate2 t
    status = T.pack . show $ tstatus t
    code = tcode t
    comment = T.strip $ tcomment t

postingToCSV :: Posting -> CSV
postingToCSV p =
  map (\(a@(Amount {aquantity=q,acommodity=c})) ->
    -- commodity goes into separate column, so we suppress it, along with digit group
    -- separators and prices
    let a_ = amountStripPrices a{acommodity=""} in
    let showamt = wbToText . showAmountB csvDisplay in
    let amt = showamt a_ in
    let credit = if q < 0 then showamt $ negate a_ else "" in
    let debit  = if q >= 0 then showamt a_ else "" in
    [account, amt, c, credit, debit, status, comment])
    . amounts $ pamount p
  where
    status = T.pack . show $ pstatus p
    account = showAccountName Nothing (ptype p) (paccount p)
    comment = T.strip $ pcomment p
