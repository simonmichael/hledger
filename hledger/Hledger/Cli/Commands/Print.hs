{-|

A ledger-compatible @print@ command.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Print (
  printmode
 ,print'
 -- ,entriesReportAsText
 ,transactionWithMostlyOriginalPostings
)
where


import Data.List (intersperse, intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Lens.Micro ((^.), _Just, has)
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Read.CsvUtils (CSV, printCSV)
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import System.Exit (exitFailure)
import Safe (lastMay)


printmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Print.txt")
  ([flagNone ["explicit","x"] (setboolopt "explicit")
    "show all amounts explicitly"
  ,flagNone ["show-costs"] (setboolopt "show-costs")
    "show transaction prices even with conversion postings"
  ,flagReq  ["round"] (\s opts -> Right $ setopt "round" s opts) "TYPE" $
    intercalate "\n"
    ["how much rounding or padding should be done when displaying amounts ?"
    ,"none - show original decimal digits,"
    ,"       as in journal"
    ,"soft - just add or remove decimal zeros"
    ,"       to match precision (default)"
    ,"hard - round posting amounts to precision"
    ,"       (can unbalance transactions)"
    ,"all  - also round cost amounts to precision"
    ,"       (can unbalance transactions)"
    ]
  ,flagNone ["new"] (setboolopt "new")
    "show only newer-dated transactions added in each file since last run"
  ,let arg = "DESC" in
   flagReq  ["match","m"] (\s opts -> Right $ setopt "match" s opts) arg
    ("fuzzy search for one recent transaction with description closest to "++arg)
  ,outputFormatFlag ["txt","csv","json","sql"]
  ,outputFileFlag
  ])
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Get the --round option's value, if any. Can fail with a parse error.
roundFromRawOpts :: RawOpts -> Maybe Rounding
roundFromRawOpts = lastMay . collectopts roundfromrawopt
  where
    roundfromrawopt (n,v)
      | n=="round", v=="none" = Just NoRounding
      | n=="round", v=="soft" = Just SoftRounding
      | n=="round", v=="hard" = Just HardRounding
      | n=="round", v=="all"  = Just AllRounding
      | n=="round"            = error' $ "--round's value should be none, soft, hard or all; got: "++v
      | otherwise             = Nothing

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
      -- match mode, prints one recent transaction most similar to given description
      -- XXX should match similarly to register --match
      case journalSimilarTransaction opts j' (dbg1 "finding best match for description" $ T.pack desc) of
        Just t  -> printEntries opts j'{jtxns=[t]}
        Nothing -> putStrLn "no matches found." >> exitFailure

printEntries :: CliOpts -> Journal -> IO ()
printEntries opts@CliOpts{rawopts_=rawopts, reportspec_=rspec} j =
  writeOutputLazyText opts $ render $ entriesReport rspec j
  where
    -- print does user-specified rounding or (by default) no rounding, in all output formats
    styles =
      case roundFromRawOpts rawopts of
        Nothing         -> styles0
        Just NoRounding -> styles0
        Just r          -> amountStylesSetRounding r styles0
      where styles0 = journalCommodityStyles j

    fmt = outputFormatFromOpts opts
    render | fmt=="txt"  = entriesReportAsText opts      . styleAmounts styles
           | fmt=="csv"  = printCSV . entriesReportAsCsv . styleAmounts styles
           | fmt=="json" = toJsonText                    . styleAmounts styles
           | fmt=="sql"  = entriesReportAsSql            . styleAmounts styles
           | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:

entriesReportAsText :: CliOpts -> EntriesReport -> TL.Text
entriesReportAsText opts =
    TB.toLazyText . foldMap (TB.fromText . showTransaction . txntransform)
  where
    txntransform
      -- Use the fully inferred and amount-styled/rounded transaction in the following situations:
      -- with -x/--explicit:
      | boolopt "explicit" (rawopts_ opts) = id
      -- with --show-costs:
      | opts ^. infer_costs = id
      -- with -B/-V/-X/--value ("because of #551, and because of print -V valuing only one posting when there's an implicit txn price.")
      | has (value . _Just) opts = id
      -- Otherwise, keep the transaction's amounts close to how they were written in the journal.
      | otherwise = transactionWithMostlyOriginalPostings

-- | Replace this transaction's postings with the original postings if any, but keep the
-- current possibly rewritten account names, and the inferred values of any auto postings.
-- This is mainly for showing transactions with the amounts in their original journal format.
transactionWithMostlyOriginalPostings :: Transaction -> Transaction
transactionWithMostlyOriginalPostings = transactionMapPostings postingMostlyOriginal

-- Get the original posting if any, but keep the current (possibly rewritten) account name,
-- and the amounts of any auto postings.
postingMostlyOriginal p = orig
    { paccount = paccount p
    , pamount = pamount $ if isGenerated then p else orig }
  where
    orig = originalPosting p
    isGenerated = "_generated-posting" `elem` map fst (ptags p)

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
        setDecimalPoint a = a{astyle=(astyle a){asdecimalmark=Just '.'}}

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
