{-|

A ledger-compatible @print@ command.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Print (
  printmode
 ,print'
 -- ,entriesReportAsText
 ,roundFlag
 ,roundFromRawOpts
 ,amountStylesSetRoundingFromRawOpts
 ,transactionWithMostlyOriginalPostings
)
where


import Data.Function ((&))
import Data.List (intersperse, intercalate)
import Data.List.Extra (nubSort)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Lens.Micro ((^.), _Just, has)
import Safe (lastMay, minimumDef)
import System.Console.CmdArgs.Explicit
import System.Exit (exitFailure)

import Hledger
import Hledger.Write.Beancount (accountNameToBeancount, showTransactionBeancount)
import Hledger.Write.Csv (CSV, printCSV, printTSV)
import Hledger.Write.Ods (printFods)
import Hledger.Write.Html.Lucid (printHtml)
import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Hledger.Cli.Anchor (setAccountAnchor)
import qualified Lucid
import qualified System.IO as IO
import Data.Maybe (isJust)

printmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Print.txt")
  ([flagNone ["explicit","x"] (setboolopt "explicit")
    "show all amounts explicitly"
  ,flagNone ["show-costs"] (setboolopt "show-costs")
    "show transaction prices even with conversion postings"
  ,roundFlag
  ,flagNone ["new"] (setboolopt "new")
    "show only newer-dated transactions added in each file since last run"
  ,let arg = "DESC" in
   flagReq  ["match","m"] (\s opts -> Right $ setopt "match" s opts) arg
    ("fuzzy search for one recent transaction with description closest to "++arg)
  ,flagReq  ["base-url"] (\s opts -> Right $ setopt "base-url" s opts) "URLPREFIX" "in html output, generate links to hledger-web, with this prefix. (Usually the base url shown by hledger-web; can also be relative.)"
  ,outputFormatFlag ["txt","beancount","csv","tsv","html","fods","json","sql"]
  ,outputFileFlag
  ])
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

roundFlag = flagReq  ["round"] (\s opts -> Right $ setopt "round" s opts) "TYPE" $
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

-- | Set these amount styles' rounding strategy when they are being applied to amounts,
-- according to the value of the --round option, if any.
amountStylesSetRoundingFromRawOpts :: RawOpts -> Map CommoditySymbol AmountStyle -> Map CommoditySymbol AmountStyle
amountStylesSetRoundingFromRawOpts rawopts styles =
  case roundFromRawOpts rawopts of
    Just r  -> amountStylesSetRounding r styles
    Nothing -> styles

-- | Print journal transactions in standard format.
print' :: CliOpts -> Journal -> IO ()
print' opts j = do
  -- The print command should show all amounts with their original decimal places,
  -- but as part of journal reading the posting amounts have already been normalised
  -- according to commodity display styles, and currently it's not easy to avoid
  -- that. For now we try to reverse it by increasing all amounts' decimal places 
  -- sufficiently to show the amount exactly. The displayed amounts may have minor
  -- differences from the originals, such as trailing zeroes added.
  let
    -- lbl = lbl_ "print'"
    j' = j
      -- & dbg9With (lbl "amounts before setting full precision".showJournalAmountsDebug)
      & journalMapPostingAmounts mixedAmountSetFullPrecision
      -- & dbg9With (lbl "amounts after  setting full precision: ".showJournalAmountsDebug)

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
    styles = amountStylesSetRoundingFromRawOpts rawopts $ journalCommodityStyles j

    fmt = outputFormatFromOpts opts
    baseUrl = balance_base_url_ $ _rsReportOpts rspec
    query = querystring_ $ _rsReportOpts rspec
    render | fmt=="txt"       = entriesReportAsText           . styleAmounts styles . map maybeoriginalamounts
           | fmt=="beancount" = entriesReportAsBeancount      . styleAmounts styles . map maybeoriginalamounts
           | fmt=="csv"       = printCSV . entriesReportAsCsv . styleAmounts styles
           | fmt=="tsv"       = printTSV . entriesReportAsCsv . styleAmounts styles
           | fmt=="json"      = toJsonText                    . styleAmounts styles
           | fmt=="sql"       = entriesReportAsSql            . styleAmounts styles
           | fmt=="html" =
                (<>"\n") . Lucid.renderText . printHtml .
                map (map (fmap Lucid.toHtml)) .
                entriesReportAsSpreadsheet oneLineNoCostFmt baseUrl query .
                styleAmounts styles
           | fmt=="fods" =
                printFods IO.localeEncoding . Map.singleton "Print" .
                (,) (1,0) .
                entriesReportAsSpreadsheet oneLineNoCostFmt baseUrl query .
                styleAmounts styles
           | otherwise = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
      where
        maybeoriginalamounts
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
  where
    postingMostlyOriginal p = orig
        { paccount = paccount p
        , pamount = pamount $ if isGenerated then p else orig }
      where
        orig = originalPosting p
        isGenerated = "_generated-posting" `elem` map fst (ptags p)

entriesReportAsText :: EntriesReport -> TL.Text
entriesReportAsText = entriesReportAsTextHelper showTransaction

entriesReportAsTextHelper :: (Transaction -> T.Text) -> EntriesReport -> TL.Text
entriesReportAsTextHelper showtxn = TB.toLazyText . foldMap (TB.fromText . showtxn)

-- This transforms transactions in various ways (see Beancount.hs) to make them Beancount-compatible.
-- It also generates an account open directive for each account used (on their earliest transaction dates).
entriesReportAsBeancount :: EntriesReport -> TL.Text
entriesReportAsBeancount ts =
  -- PERF: gathers and converts all account names, then repeats that work when showing each transaction
  opendirectives <> "\n" <> entriesReportAsTextHelper showTransactionBeancount ts3
  where
    -- Remove any virtual postings.
    ts2 = [t{tpostings=filter isReal $ tpostings t} | t <- ts]

    -- Remove any conversion postings that are redundant with costs.
    -- It would be easier to remove the costs instead,
    -- but those are more useful to Beancount than conversion postings.
    ts3 =
      [ t{tpostings=filter (not . isredundantconvp) $ tpostings t}
      | t <- ts2
      -- XXX But how to do it ? conversion-posting tag is on non-redundant postings too.
      -- Assume the simple case of no more than one cost + conversion posting group in each transaction.
      -- Actually that seems to be required by hledger right now.
      , let isredundantconvp p =
              matchesPosting (Tag (toRegex' "conversion-posting") Nothing) p
              && any (any (isJust.acost) . amounts . pamount) (tpostings t)
      ]

    opendirectives
      | null ts = ""
      | otherwise = TL.fromStrict $ T.unlines [
          firstdate <> " open " <> accountNameToBeancount a
          | a <- nubSort $ concatMap (map paccount.tpostings) ts3
          ]
        where
          firstdate = showDate $ minimumDef err $ map tdate ts3
            where err = error' "entriesReportAsBeancount: should not happen"

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
    csv =
        Spr.rawTableContent . transactionToSpreadsheet machineFmt Nothing [] .
        transactionMapPostingAmounts (mapMixedAmount setDecimalPoint)
            =<< txns
      where
        setDecimalPoint a = a{astyle=(astyle a){asdecimalmark=Just '.'}}

entriesReportAsCsv :: EntriesReport -> CSV
entriesReportAsCsv =
  Spr.rawTableContent . entriesReportAsSpreadsheet machineFmt Nothing []

entriesReportAsSpreadsheet ::
  AmountFormat -> Maybe Text -> [Text] ->
  EntriesReport -> [[Spr.Cell Spr.NumLines Text]]
entriesReportAsSpreadsheet fmt baseUrl query txns =
  Spr.addHeaderBorders
    (map Spr.headerCell
        ["txnidx","date","date2","status","code","description","comment",
         "account","amount","commodity","credit","debit",
         "posting-status","posting-comment"])
  :
  concatMap (transactionToSpreadsheet fmt baseUrl query) txns

-- | Generate one record per posting, duplicating the common transaction fields.
-- The txnidx field (transaction index) allows postings to be grouped back into transactions.
transactionToSpreadsheet ::
  AmountFormat -> Maybe Text -> [Text] ->
  Transaction -> [[Spr.Cell Spr.NumLines Text]]
transactionToSpreadsheet fmt baseUrl query t =
  addRowSpanHeader (idx:d:d2:status:code:description:comment:[])
    (postingToSpreadsheet fmt baseUrl query =<< tpostings t)
  where
    cell = Spr.defaultCell
    idx = Spr.integerCell $ tindex t
    description = cell $ tdescription t
    dateCell date =
        (Spr.defaultCell $ showDate date) {Spr.cellType = Spr.TypeDate}
    d = dateCell $ tdate t
    d2 = maybe Spr.emptyCell dateCell $ tdate2 t
    status = cell $ T.pack . show $ tstatus t
    code = cell $ tcode t
    comment = cell $ T.strip $ tcomment t

addRowSpanHeader ::
    [Spr.Cell border text] ->
    [[Spr.Cell border text]] -> [[Spr.Cell border text]]
addRowSpanHeader common rows =
    case rows of
        [] -> []
        [row] -> [common++row]
        _ ->
            let setSpan spn cell = cell{Spr.cellSpan = spn} in
            zipWith (++)
                (map (setSpan $ Spr.SpanVertical $ length rows) common :
                 repeat (map (setSpan Spr.Covered) common))
                rows

postingToSpreadsheet ::
  (Spr.Lines border) =>
  AmountFormat -> Maybe Text -> [Text] ->
  Posting -> [[Spr.Cell border Text]]
postingToSpreadsheet fmt baseUrl query p =
  map (\(a@(Amount {aquantity=q,acommodity=c})) ->
    -- commodity goes into separate column, so we suppress it, along with digit group
    -- separators and prices
    let a_ = amountStripCost a{acommodity=""} in
    let credit = if q < 0 then amountCell $ negate a_ else Spr.emptyCell in
    let debit  = if q >= 0 then amountCell a_ else Spr.emptyCell in
    [setAccountAnchor baseUrl query (paccount p) $ cell account,
     amountCell a_, cell c,
     credit, debit, cell status, cell comment])
    . amounts $ pamount p
  where
    cell = Spr.defaultCell
    amountCell amt =
      Spr.cellFromAmount fmt
        (Spr.Class "amount", (wbToText $ showAmountB machineFmt amt, amt))
    status = T.pack . show $ pstatus p
    account = showAccountName Nothing (ptype p) (paccount p)
    comment = T.strip $ pcomment p
