{-|

A ledger-compatible @balance@ command, with additional support for
multi-column reports.

Here is a description/specification for the balance command.  See also
"Hledger.Reports" -> \"Balance reports\".


/Basic balance report/

With no report interval (@--monthly@ etc.), hledger's balance
command emulates ledger's, showing accounts indented according to
hierarchy, along with their total amount posted (including subaccounts).

Here's an example. With @examples/sample.journal@, which defines the following account tree:

@
 assets
   bank
     checking
     saving
   cash
 expenses
   food
   supplies
 income
   gifts
   salary
 liabilities
   debts
@

the basic @balance@ command gives this output:

@
 $ hledger -f sample.journal balance
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
                  $2  expenses
                  $1    food
                  $1    supplies
                 $-2  income
                 $-1    gifts
                 $-1    salary
                  $1  liabilities:debts
--------------------
                   0
@

Subaccounts are displayed indented below their parent. Only the account leaf name (the final part) is shown.
(With @--flat@, account names are shown in full and unindented.)

Each account's \"balance\" is the sum of postings in that account and any subaccounts during the report period.
When the report period includes all transactions, this is equivalent to the account's current balance.

The overall total of the highest-level displayed accounts is shown below the line.
(The @--no-total/-N@ flag prevents this.)

/Eliding and omitting/

Accounts which have a zero balance, and no non-zero subaccount
balances, are normally omitted from the report.
(The @--empty/-E@ flag forces such accounts to be displayed.)
Eg, above @checking@ is omitted because it has a zero balance and no subaccounts.

Accounts which have a single subaccount also being displayed, with the same balance,
are normally elided into the subaccount's line.
(The @--no-elide@ flag prevents this.)
Eg, above @bank@ is elided to @bank:saving@ because it has only a
single displayed subaccount (@saving@) and their balance is the same
($1). Similarly, @liabilities@ is elided to @liabilities:debts@.

/Date limiting/

The default report period is that of the whole journal, including all
known transactions. The @--begin\/-b@, @--end\/-e@, @--period\/-p@
options or @date:@/@date2:@ patterns can be used to report only
on transactions before and/or after specified dates.

/Depth limiting/

The @--depth@ option can be used to limit the depth of the balance report.
Eg, to see just the top level accounts (still including their subaccount balances):

@
$ hledger -f sample.journal balance --depth 1
                 $-1  assets
                  $2  expenses
                 $-2  income
                  $1  liabilities
--------------------
                   0
@

/Account limiting/

With one or more account pattern arguments, the report is restricted
to accounts whose name matches one of the patterns, plus their parents
and subaccounts. Eg, adding the pattern @o@ to the first example gives:

@
 $ hledger -f sample.journal balance o
                  $1  expenses:food
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-1
@

* The @o@ pattern matched @food@ and @income@, so they are shown.

* @food@'s parent (@expenses@) is shown even though the pattern didn't
  match it, to clarify the hierarchy. The usual eliding rules cause it to be elided here.

* @income@'s subaccounts are also shown.

/Multi-column balance report/

hledger's balance command will show multiple columns when a reporting
interval is specified (eg with @--monthly@), one column for each sub-period.

There are three accumulation strategies for multi-column balance report, indicated by
the heading:

* A \"period balance\" (or \"flow\") report (with @--change@, the default) shows the
  change of account balance in each period, which is equivalent to the sum of postings
  in each period. Here, checking's balance increased by 10 in Feb:

  > Change of balance (flow):
  >
  >                  Jan   Feb   Mar
  > assets:checking   20    10    -5

* A \"cumulative balance\" report (with @--cumulative@) shows the accumulated ending balance
  across periods, starting from zero at the report's start date.
  Here, 30 is the sum of checking postings during Jan and Feb:

  > Ending balance (cumulative):
  >
  >                  Jan   Feb   Mar
  > assets:checking   20    30    25

* A \"historical balance\" report (with @--historical/-H@) also shows ending balances,
  but it includes the starting balance from any postings before the report start date.
  Here, 130 is the balance from all checking postings at the end of Feb, including
  pre-Jan postings which created a starting balance of 100:

  > Ending balance (historical):
  >
  >                  Jan   Feb   Mar
  > assets:checking  120   130   125

/Eliding and omitting, 2/

Here's a (imperfect?) specification for the eliding/omitting behaviour:

* Each account is normally displayed on its own line.

* An account less deep than the report's max depth, with just one
interesting subaccount, and the same balance as the subaccount, is
non-interesting, and prefixed to the subaccount's line, unless
@--no-elide@ is in effect.

* An account with a zero inclusive balance and less than two interesting
subaccounts is not displayed at all, unless @--empty@ is in effect.

* Multi-column balance reports show full account names with no eliding
  (like @--flat@). Accounts (and periods) are omitted as described below.

/Which accounts to show in balance reports/

By default:

* single-column: accounts with non-zero balance in report period.
                 (With @--flat@: accounts with non-zero balance and postings.)

* change:        accounts with postings and non-zero period balance in any period

* cumulative:    accounts with non-zero cumulative balance in any period

* historical:    accounts with non-zero historical balance in any period

With @-E/--empty@:

* single-column: accounts with postings in report period

* change:        accounts with postings in report period

* cumulative:    accounts with postings in report period

* historical:    accounts with non-zero starting balance +
                 accounts with postings in report period

/Which periods (columns) to show in balance reports/

An empty period/column is one where no report account has any postings.
A zero period/column is one where no report account has a non-zero period balance.

Currently,

by default:

* single-column: N/A

* change:        all periods within the overall report period,
                 except for leading and trailing empty periods

* cumulative:    all periods within the overall report period,
                 except for leading and trailing empty periods

* historical:    all periods within the overall report period,
                 except for leading and trailing empty periods

With @-E/--empty@:

* single-column: N/A

* change:        all periods within the overall report period

* cumulative:    all periods within the overall report period

* historical:    all periods within the overall report period

/What to show in empty cells/

An empty periodic balance report cell is one which has no corresponding postings.
An empty cumulative/historical balance report cell is one which has no corresponding
or prior postings, ie the account doesn't exist yet.
Currently, empty cells show 0.

-}

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}  -- for lucid_
{-# LANGUAGE FlexibleContexts #-}      -- for stylesheet_
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Hledger.Cli.Commands.Balance (
  -- ** balance command
  balancemode
 ,balance
  -- ** balance output rendering
 ,balanceReportAsText
 ,balanceReportAsCsv
 ,balanceReportAsSpreadsheet
 ,balanceReportItemAsText
 ,multiBalanceRowAsCsvText
 ,multiBalanceRowAsText
 ,multiBalanceReportAsText
 ,multiBalanceReportAsCsv
 ,multiBalanceReportAsHtml
 ,multiBalanceReportHtmlRows
 ,multiBalanceReportHtmlFootRow
 ,multiBalanceReportAsTable
 ,multiBalanceReportTableAsText
 ,multiBalanceReportAsSpreadsheet
  -- ** HTML output helpers
 ,stylesheet_
 ,styles_
 ,bold
 ,doubleborder
 ,topdoubleborder
 ,bottomdoubleborder
 ,alignright
 ,alignleft
 ,aligncenter
 ,collapse
 ,lpad
 ,rpad
 ,hpad
 ,vpad
  -- ** Tests
 ,tests_Balance
) where

import Control.Arrow ((***))
import Data.Decimal (roundTo)
import Data.Default (def)
import Data.Function (on)
import Data.List (find, transpose, foldl')
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time (addDays, fromGregorian)
import System.Console.CmdArgs.Explicit as C (flagNone, flagReq, flagOpt)
import Lucid as L hiding (value_)
import Safe (headMay, maximumMay)
import Text.Tabular.AsciiWide
    (Header(..), Align(..), Properties(..), Cell(..), Table(..), TableOpts(..),
    cellWidth, concatTables, renderColumns, renderRowB, renderTableByRowsB, textCell)

import qualified System.IO as IO

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Hledger.Write.Csv (CSV, printCSV, printTSV)
import Hledger.Write.Ods (printFods)
import Hledger.Write.Html (printHtml)
import qualified Hledger.Write.Spreadsheet as Ods


-- | Command line options for this command.
balancemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Balance.txt")
  (
    -- https://hledger.org/dev/hledger.html#calculation-type :
    [flagNone ["sum"] (setboolopt "sum")
      "show sum of posting amounts (default)"
    -- XXX --budget[=DESCPAT], --forecast[=PERIODEXP], could be more consistent
    ,flagOpt "" ["budget"] (\s opts -> Right $ setopt "budget" s opts) "DESCPAT"
      (unlines
      [ "show sum of posting amounts together with budget goals defined by periodic"
      , "transactions. With a DESCPAT argument (must be separated by = not space),"
      , "use only periodic transactions with matching description"
      , "(case insensitive substring match)."
      ])
    ,flagNone ["valuechange"] (setboolopt "valuechange")
      "show total change of value of period-end historical balances (caused by deposits, withdrawals, market price fluctuations)"
    ,flagNone ["gain"] (setboolopt "gain")
      "show unrealised capital gain/loss (historical balance value minus cost basis)"
    ,flagNone ["count"] (setboolopt "count") "show the count of postings"
    -- https://hledger.org/dev/hledger.html#accumulation-type :
    ,flagNone ["change"] (setboolopt "change")
      "accumulate amounts from column start to column end (in multicolumn reports, default)"
    ,flagNone ["cumulative"] (setboolopt "cumulative")
      "accumulate amounts from report start (specified by e.g. -b/--begin) to column end"
    ,flagNone ["historical","H"] (setboolopt "historical")
      "accumulate amounts from journal start to column end (includes postings before report start date)"
    ]
    -- other options specific to this command:
    ++ flattreeflags True ++
    [flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "omit N leading account name parts (in flat mode)"
    ,flagNone ["declared"] (setboolopt "declared") "include non-parent declared accounts (best used with -E)"
    ,flagNone ["average","A"] (setboolopt "average") "show a row average column (in multicolumn reports)"
    ,flagNone ["related","r"] (setboolopt "related") "show postings' siblings instead"
    ,flagNone ["row-total","T"] (setboolopt "row-total") "show a row total column (in multicolumn reports)"
    ,flagNone ["summary-only"] (setboolopt "summary-only") "display only row summaries (e.g. row total, average) (in multicolumn reports)"
    ,flagNone ["no-total","N"] (setboolopt "no-total") "omit the final total row"
    ,flagNone ["no-elide"] (setboolopt "no-elide") "don't squash boring parent accounts (in tree mode)"
    ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
    ,flagNone ["sort-amount","S"] (setboolopt "sort-amount") "sort by amount instead of account code/name (in flat mode). With multiple columns, sorts by the row total, or by row average if that is displayed."
    ,flagNone ["percent", "%"] (setboolopt "percent") "express values in percentage of each column's total"
    ,flagNone ["invert"] (setboolopt "invert") "display all amounts with reversed sign"
    ,flagNone ["transpose"] (setboolopt "transpose") "transpose rows and columns"
    ,flagReq  ["layout"] (\s opts -> Right $ setopt "layout" s opts) "ARG"
      (unlines
        ["how to lay out multi-commodity amounts and the overall table:"
        ,"'wide[,WIDTH]': commodities on one line"
        ,"'tall'        : commodities on separate lines"
        ,"'bare'        : commodity symbols in one column"
        ,"'tidy'        : every attribute in its own column"
        ])
    -- output:
    ,outputFormatFlag ["txt","html","csv","tsv","json","fods"]
    ,outputFileFlag
    ]
  )
  cligeneralflagsgroups1
  (hiddenflags ++
    [ flagNone ["commodity-column"] (setboolopt "commodity-column")
      "show commodity symbols in a separate column, amounts as bare numbers, one row per commodity"
    ])
  ([], Just $ argsFlag "[QUERY]")

-- | The balance command, prints a balance report.
balance :: CliOpts -> Journal -> IO ()
balance opts@CliOpts{reportspec_=rspec} j = case balancecalc_ of
    CalcBudget -> do  -- single or multi period budget report
      let rspan = fst $ reportSpan j rspec
          budgetreport = styleAmounts styles $ budgetReport rspec (balancingopts_ $ inputopts_ opts) rspan j
          render = case fmt of
            "txt"  -> budgetReportAsText ropts
            "json" -> (<>"\n") . toJsonText
            "csv"  -> printCSV . budgetReportAsCsv ropts
            "tsv"  -> printTSV . budgetReportAsCsv ropts
            "html" -> (<>"\n") . L.renderText .
                      printHtml . map (map (fmap L.toHtml)) . budgetReportAsSpreadsheet ropts
            "fods" -> printFods IO.localeEncoding .
                      Map.singleton "Hledger" . (,) (Just 1, Nothing) . budgetReportAsSpreadsheet ropts
            _      -> error' $ unsupportedOutputFormatError fmt
      writeOutputLazyText opts $ render budgetreport

    _ | multiperiod -> do  -- multi period balance report
        let report = styleAmounts styles $ multiBalanceReport rspec j
            render = case fmt of
              "txt"  -> multiBalanceReportAsText ropts
              "csv"  -> printCSV . multiBalanceReportAsCsv ropts
              "tsv"  -> printTSV . multiBalanceReportAsCsv ropts
              "html" -> (<>"\n") . L.renderText . multiBalanceReportAsHtml ropts
              "json" -> (<>"\n") . toJsonText
              "fods" -> printFods IO.localeEncoding .
                        Map.singleton "Hledger" . multiBalanceReportAsSpreadsheet ropts
              _      -> const $ error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        writeOutputLazyText opts $ render report

    _ -> do  -- single period simple balance report
        let report = styleAmounts styles $ balanceReport rspec j -- simple Ledger-style balance report
            render = case fmt of
              "txt"  -> \ropts1 -> TB.toLazyText . balanceReportAsText ropts1
              "csv"  -> \ropts1 -> printCSV . balanceReportAsCsv ropts1
              "tsv"  -> \ropts1 -> printTSV . balanceReportAsCsv ropts1
              "html" -> \ropts1 -> (<>"\n") . L.renderText .
                                   printHtml . map (map (fmap L.toHtml)) . balanceReportAsSpreadsheet ropts1
              "json" -> const $ (<>"\n") . toJsonText
              "fods" -> \ropts1 -> printFods IO.localeEncoding . Map.singleton "Hledger" . (,) (Just 1, Nothing) . balanceReportAsSpreadsheet ropts1
              _      -> error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        writeOutputLazyText opts $ render ropts report
  where
    styles = journalCommodityStylesWith HardRounding j
    ropts@ReportOpts{..} = _rsReportOpts rspec
    -- Tidy csv/tsv should be consistent between single period and multiperiod reports.
    multiperiod = interval_ /= NoInterval || (layout_ == LayoutTidy && delimited)
    delimited   = fmt == "csv" || fmt == "tsv"
    fmt         = outputFormatFromOpts opts

-- Rendering

-- What to show as heading for the totals row in balance reports ?
-- Currently nothing in terminal, Total: in html and xSV output.
totalRowHeadingText       = ""
totalRowHeadingBudgetText = ""
totalRowHeadingHtml       = "Total:"
totalRowHeadingCsv        = "Total:"
totalRowHeadingBudgetCsv  = "Total:"

-- Single-column balance reports

-- | Render a single-column balance report as CSV.
balanceReportAsCsv :: ReportOpts -> BalanceReport -> CSV
balanceReportAsCsv opts =
    map (map Ods.cellContent) . balanceReportAsSpreadsheet opts

-- | Render a single-column balance report as plain text.
balanceReportAsText :: ReportOpts -> BalanceReport -> TB.Builder
balanceReportAsText opts ((items, total)) = case layout_ opts of
    LayoutBare | iscustom -> error' "Custom format not supported with commodity columns"  -- PARTIAL:
    LayoutBare -> bareLayoutBalanceReportAsText opts ((items, total))
    _ -> unlinesB ls <> unlinesB (if no_total_ opts then [] else [overline, totalLines])
  where
    (ls, sizes) = unzip $ map (balanceReportItemAsText opts) items
    -- abuse renderBalanceReportItem to render the total with similar format
    (totalLines, _) = renderBalanceReportItem opts ("",0,total)
    -- with a custom format, extend the line to the full report width;
    -- otherwise show the usual 20-char line for compatibility
    iscustom = case format_ opts of
        OneLine       ((FormatField _ _ _ TotalField):_) -> False
        TopAligned    ((FormatField _ _ _ TotalField):_) -> False
        BottomAligned ((FormatField _ _ _ TotalField):_) -> False
        _ -> True
    overlinewidth = if iscustom then sum (map maximum' $ transpose sizes) else 20
    overline   = TB.fromText $ T.replicate overlinewidth "-"

-- | Render a single-column balance report as plain text with a separate commodity column (--layout=bare)
bareLayoutBalanceReportAsText :: ReportOpts -> BalanceReport -> TB.Builder
bareLayoutBalanceReportAsText opts ((items, total)) =
  unlinesB .
  map 
    (renderColumns def{tableBorders=singleColumnTableOuterBorder} sizes .
     Group singleColumnTableInterColumnBorder . map Header) $
  ls ++ concat [[[overline], totalline] | not (no_total_ opts)]
  where
    render (_, acctname, dep, amt) =
        [ Cell TopRight damts
        , Cell TopLeft (fmap wbFromText cs)
        , Cell TopLeft (replicate (length damts - 1) mempty ++ [wbFromText dispname]) ]
      where dopts = oneLineNoCostFmt{displayCommodity=layout_ opts /= LayoutBare, displayCommodityOrder=Just cs, displayColour=color_ opts}
            cs    = if mixedAmountLooksZero amt then [""] else S.toList $ maCommodities amt
            dispname = T.replicate ((dep - 1) * 2) " " <> acctname
            damts = showMixedAmountLinesB dopts amt
    ls = fmap render items
    totalline = render ("", "", 0, total)
    sizes = fromMaybe 0 . maximumMay . map cellWidth <$>
        transpose ([totalline | not (no_total_ opts)] ++ ls)
    overline = Cell TopLeft . pure . wbFromText . flip T.replicate "-" . fromMaybe 0 $ headMay sizes
    singleColumnTableOuterBorder       = pretty_ opts
    singleColumnTableInterColumnBorder = if pretty_ opts then SingleLine else NoLine

{-
:r
This implementation turned out to be a bit convoluted but implements the following algorithm for formatting:

- If there is a single amount, print it with the account name directly:
- Otherwise, only print the account name on the last line.

    a         USD 1   ; Account 'a' has a single amount
              EUR -1
    b         USD -1  ; Account 'b' has two amounts. The account name is printed on the last line.
-}
-- | Render one balance report line item as plain text suitable for console output (or
-- whatever string format is specified). Note, prices will not be rendered, and
-- differently-priced quantities of the same commodity will appear merged.
-- The output will be one or more lines depending on the format and number of commodities.
balanceReportItemAsText :: ReportOpts -> BalanceReportItem -> (TB.Builder, [Int])
balanceReportItemAsText opts (_, accountName, dep, amt) =
  renderBalanceReportItem opts (accountName, dep, amt)

-- | Render a balance report item, using the StringFormat specified by --format.
-- 
renderBalanceReportItem :: ReportOpts -> (AccountName, Int, MixedAmount) -> (TB.Builder, [Int])
renderBalanceReportItem opts (acctname, dep, total) =
  case format_ opts of
      OneLine       comps -> renderRowFromComponents $ renderComponents True  True  comps
      TopAligned    comps -> renderRowFromComponents $ renderComponents True  False comps
      BottomAligned comps -> renderRowFromComponents $ renderComponents False False comps

  where
    -- Combine the rendered component cells horizontally, as a possibly multi-line text (builder),
    -- aligned in borderless columns (? XXX). Also returns the rendered width of each cell.
    renderRowFromComponents :: [Cell] -> (TB.Builder, [Int])
    renderRowFromComponents cs =
      ( renderRowB def{tableBorders=False, borderSpaces=False} . Group NoLine $ map Header cs
      , map cellWidth cs
      )

    -- Render each of the given StringFormat components for the balance report item,
    -- returning each as a Cell.
    renderComponents :: Bool -> Bool -> [StringFormatComponent] -> [Cell]
    renderComponents topaligned oneline = map (renderComponent topaligned oneline opts (acctname, dep, total))

-- Render one StringFormat component for a balance report item.
-- Returns a Cell, containing 0 or more lines of text (as builders).
renderComponent :: Bool -> Bool -> ReportOpts -> (AccountName, Int, MixedAmount) -> StringFormatComponent -> Cell
renderComponent _ _ _ _ (FormatLiteral s) = textCell TopLeft s
renderComponent topaligned oneline opts (acctname, dep, total) (FormatField ljust mmin mmax field) = case field of
    DepthSpacerField -> Cell align [WideBuilder (TB.fromText $ T.replicate d " ") d]
                        where d = maybe id min mmax $ dep * fromMaybe 1 mmin
    AccountField     -> textCell align $ formatText ljust mmin mmax acctname
    TotalField       -> Cell align . pure $ showMixedAmountB dopts total
    _                -> Cell align [mempty]
  where
    align | topaligned && ljust = TopLeft
          | topaligned          = TopRight
          | ljust               = BottomLeft
          | otherwise           = BottomRight
    dopts = noCostFmt{displayCommodity = layout_ opts /= LayoutBare
                  ,displayOneLine   = oneline
                  ,displayMinWidth  = mmin
                  ,displayMaxWidth  = mmax
                  ,displayColour    = color_ opts
                  }


headerCell :: Text -> Ods.Cell Ods.NumLines Text
headerCell text =
    let deflt = Ods.defaultCell text
    in
    deflt {
        Ods.cellStyle = Ods.Head,
        Ods.cellBorder =
            (Ods.cellBorder deflt) {Ods.borderBottom = Ods.DoubleLine}
    }

addTotalBorders :: [[Ods.Cell border text]] -> [[Ods.Cell Ods.NumLines text]]
addTotalBorders =
    zipWith
        (\border ->
            map (\c -> c {
                    Ods.cellStyle = Ods.Body Ods.Total,
                    Ods.cellBorder = Ods.noBorder {Ods.borderTop = border}}))
        (Ods.DoubleLine : repeat Ods.NoLine)

-- | Render a single-column balance report as FODS.
balanceReportAsSpreadsheet ::
    ReportOpts -> BalanceReport -> [[Ods.Cell Ods.NumLines Text]]
balanceReportAsSpreadsheet opts (items, total) =
    headers :
    concatMap (\(a, _, _, b) -> rows a b) items ++
    if no_total_ opts then []
      else addTotalBorders $ rows totalRowHeadingCsv total
  where
    cell = Ods.defaultCell
    headers =
      map headerCell $
      "account" : case layout_ opts of
        LayoutBare -> ["commodity", "balance"]
        _          -> ["balance"]
    rows :: AccountName -> MixedAmount -> [[Ods.Cell Ods.NumLines Text]]
    rows name ma = case layout_ opts of
      LayoutBare ->
          map (\a ->
                [showName name,
                 cell $ acommodity a,
                 renderAmount $ mixedAmount a])
          . amounts $ mixedAmountStripCosts ma
      _ -> [[showName name, renderAmount ma]]

    showName = cell . accountNameDrop (drop_ opts)
    renderAmount mixedAmt = wbToText <$> cellFromMixedAmount bopts mixedAmt
      where
        bopts = machineFmt{displayCommodity=showcomm, displayCommodityOrder = commorder}
        (showcomm, commorder)
          | layout_ opts == LayoutBare = (False, Just $ S.toList $ maCommodities mixedAmt)
          | otherwise                  = (True, Nothing)

cellFromMixedAmount ::
    (Ods.Lines border) =>
    AmountFormat -> MixedAmount -> Ods.Cell border WideBuilder
cellFromMixedAmount bopts mixedAmt =
    (Ods.defaultCell $ showMixedAmountB bopts mixedAmt) {
        Ods.cellType =
          case unifyMixedAmount mixedAmt of
            Just amt -> amountType bopts amt
            Nothing -> Ods.TypeMixedAmount
    }

cellsFromMixedAmount ::
    (Ods.Lines border) =>
    AmountFormat -> MixedAmount -> [Ods.Cell border WideBuilder]
cellsFromMixedAmount bopts mixedAmt =
    map
        (\(str,amt) ->
            (Ods.defaultCell str) {Ods.cellType = amountType bopts amt})
        (showMixedAmountLinesPartsB bopts mixedAmt)

amountType :: AmountFormat -> Amount -> Ods.Type
amountType bopts amt =
    Ods.TypeAmount $
    if displayCommodity bopts
      then amt
      else amt {acommodity = T.empty}



-- Multi-column balance reports

-- | Render a multi-column balance report as CSV.
-- The CSV will always include the initial headings row,
-- and will include the final totals row unless --no-total is set.
multiBalanceReportAsCsv :: ReportOpts -> MultiBalanceReport -> CSV
multiBalanceReportAsCsv opts@ReportOpts{..} report = maybeTranspose allRows
  where
    allRows = case layout_ of
      LayoutTidy -> rows  -- tidy csv should not include totals or averages
      _ -> rows ++ totals
    (rows, totals) = multiBalanceReportAsCsvHelper False opts report
    maybeTranspose = if transpose_ then transpose else id

-- Helper for CSV (and HTML) rendering.
multiBalanceReportAsCsvHelper :: Bool -> ReportOpts -> MultiBalanceReport -> (CSV, CSV)
multiBalanceReportAsCsvHelper ishtml opts =
    (map (map Ods.cellContent) *** map (map Ods.cellContent)) .
    multiBalanceReportAsSpreadsheetHelper ishtml opts

-- Helper for CSV and ODS and HTML rendering.
multiBalanceReportAsSpreadsheetHelper ::
    Bool -> ReportOpts -> MultiBalanceReport ->
    ([[Ods.Cell Ods.NumLines Text]], [[Ods.Cell Ods.NumLines Text]])
multiBalanceReportAsSpreadsheetHelper ishtml opts@ReportOpts{..} (PeriodicReport colspans items tr) =
    (headers : concatMap fullRowAsTexts items, addTotalBorders totalrows)
  where
    cell = Ods.defaultCell
    headers =
      map headerCell $
      "account" :
      case layout_ of
      LayoutTidy -> ["period", "start_date", "end_date", "commodity", "value"]
      LayoutBare -> "commodity" : dateHeaders
      _          -> dateHeaders
    dateHeaders = map showDateSpan colspans ++ ["total" | row_total_] ++ ["average" | average_]
    fullRowAsTexts row = map (cell (showName row) :) $ rowAsText row
      where showName = accountNameDrop drop_ . prrFullName
    totalrows
      | no_total_ = []
      | ishtml    = zipWith (:) (cell totalRowHeadingHtml : repeat Ods.emptyCell) $ rowAsText tr
      | otherwise = map (cell totalRowHeadingCsv :) $ rowAsText tr
    rowAsText =
        let fmt = if ishtml then oneLineNoCostFmt else machineFmt
        in  map (map (fmap wbToText)) . multiBalanceRowAsCellBuilders fmt opts colspans

-- Helpers and CSS styles for HTML output.

stylesheet_ elstyles = style_ $ T.unlines $ "" : [el<>" {"<>styles<>"}" | (el,styles) <- elstyles]
styles_ = style_ . T.intercalate "; "
bold = "font-weight:bold"
doubleborder = "double black"
topdoubleborder    = "border-top:"<>doubleborder
bottomdoubleborder = "border-bottom:"<>doubleborder
alignright  = "text-align:right"
alignleft   = "text-align:left"
aligncenter = "text-align:center"
collapse = "border-collapse:collapse"
lpad = "padding-left:1em"
rpad = "padding-right:1em"
hpad = "padding-left:1em; padding-right:1em"
vpad = "padding-top:1em;  padding-bottom:1em"

-- | Render a multi-column balance report as HTML.
multiBalanceReportAsHtml :: ReportOpts -> MultiBalanceReport -> Html ()
multiBalanceReportAsHtml ropts mbr =
  let
    (headingsrow,bodyrows,mtotalsrows) = multiBalanceReportHtmlRows ropts mbr
  in do
    stylesheet_ [("table",collapse), ("th, td",lpad), ("th.account, td.account","padding-left:0;")]
    table_ $ mconcat $
         [headingsrow]
      ++ bodyrows
      ++ mtotalsrows

-- | Render the HTML table rows for a MultiBalanceReport.
-- Returns the heading row, 0 or more body rows, and the totals row if enabled.
multiBalanceReportHtmlRows :: ReportOpts -> MultiBalanceReport -> (Html (), [Html ()], [Html ()])
multiBalanceReportHtmlRows ropts mbr =
  let
    -- TODO: should the commodity_column be displayed as a subaccount in this case as well?
    (headingsrow:bodyrows, mtotalsrows)
      | transpose_ ropts = error' "Sorry, --transpose with HTML output is not yet supported"  -- PARTIAL:
      | otherwise = multiBalanceReportAsCsvHelper True ropts mbr
  in
    (multiBalanceReportHtmlHeadRow ropts headingsrow
    ,map (multiBalanceReportHtmlBodyRow ropts) bodyrows
    ,zipWith3 ($)
      (repeat (multiBalanceReportHtmlFootRow ropts))
      (True : repeat False)  -- mark the first html table row for special styling
      mtotalsrows
      -- TODO pad totals row with zeros when there are
    )

-- | Render one MultiBalanceReport heading row as a HTML table row.
multiBalanceReportHtmlHeadRow :: ReportOpts -> [T.Text] -> Html ()
multiBalanceReportHtmlHeadRow _ [] = mempty  -- shouldn't happen
multiBalanceReportHtmlHeadRow ropts (acct:cells) =
  let
    (amts,tot,avg)
      | row_total_ ropts && average_ ropts = (ini2,  sndlst2, lst2)
      | row_total_ ropts                   = (ini1,  lst1,    [])
      |                     average_ ropts = (ini1,  [],      lst1)
      | otherwise                          = (cells, [],      [])
      where
        n = length cells
        (ini1,lst1)    = splitAt (n-1) cells
        (ini2, rest)   = splitAt (n-2) cells
        (sndlst2,lst2) = splitAt 1 rest

  in
    tr_ $ mconcat $
          th_ [styles_ [bottomdoubleborder,alignleft], class_ "account"]    (toHtml acct)
       : [th_ [styles_ [bottomdoubleborder,alignright], class_ ""]           (toHtml a) | a <- amts]
      ++ [th_ [styles_ [bottomdoubleborder,alignright], class_ "rowtotal"]   (toHtml a) | a <- tot]
      ++ [th_ [styles_ [bottomdoubleborder,alignright], class_ "rowaverage"] (toHtml a) | a <- avg]

-- | Render one MultiBalanceReport data row as a HTML table row.
multiBalanceReportHtmlBodyRow :: ReportOpts -> [T.Text] -> Html ()
multiBalanceReportHtmlBodyRow _ [] = mempty  -- shouldn't happen
multiBalanceReportHtmlBodyRow ropts (label:cells) =
  let
    (amts,tot,avg)
      | row_total_ ropts && average_ ropts = (ini2,  sndlst2, lst2)
      | row_total_ ropts                   = (ini1,  lst1,    [])
      |                     average_ ropts = (ini1,  [],      lst1)
      | otherwise                          = (cells, [],      [])
      where
        n = length cells
        (ini1,lst1)    = splitAt (n-1) cells
        (ini2, rest)   = splitAt (n-2) cells
        (sndlst2,lst2) = splitAt 1 rest
  in
    tr_ $ mconcat $
          td_ [styles_ [],  class_ "account"]           (toHtml label)
       : [td_ [styles_ [alignright], class_ "amount"]            (toHtml a) | a <- amts]
      ++ [td_ [styles_ [alignright], class_ "amount rowtotal"]   (toHtml a) | a <- tot]
      ++ [td_ [styles_ [alignright], class_ "amount rowaverage"] (toHtml a) | a <- avg]

-- | Render one MultiBalanceReport totals row as a HTML table row.
multiBalanceReportHtmlFootRow :: ReportOpts -> Bool -> [T.Text] -> Html ()
multiBalanceReportHtmlFootRow _ _ [] = mempty
-- TODO pad totals row with zeros when subreport is empty
--  multiBalanceReportHtmlFootRow ropts $
--     ""
--   : repeat nullmixedamt zeros
--  ++ (if row_total_ ropts then [nullmixedamt] else [])
--  ++ (if average_ ropts   then [nullmixedamt]   else [])
multiBalanceReportHtmlFootRow ropts isfirstline (hdr:cells) =
  let
    (amts,tot,avg)
      | row_total_ ropts && average_ ropts = (ini2,  sndlst2, lst2)
      | row_total_ ropts                   = (ini1,  lst1,    [])
      |                     average_ ropts = (ini1,  [],      lst1)
      | otherwise                          = (cells, [],      [])
      where
        n = length cells
        (ini1,lst1)    = splitAt (n-1) cells
        (ini2, rest)   = splitAt (n-2) cells
        (sndlst2,lst2) = splitAt 1 rest
  in
    tr_ $ mconcat $
          td_ [styles_ $ [topdoubleborder | isfirstline] ++ [bold], class_ "account"]                 (toHtml hdr)
       : [td_ [styles_ $ [topdoubleborder | isfirstline] ++ [alignright], class_ "amount coltotal"]   (toHtml a) | a <- amts]
      ++ [td_ [styles_ $ [topdoubleborder | isfirstline] ++ [alignright], class_ "amount coltotal"]   (toHtml a) | a <- tot]
      ++ [td_ [styles_ $ [topdoubleborder | isfirstline] ++ [alignright], class_ "amount colaverage"] (toHtml a) | a <- avg]

--thRow :: [String] -> Html ()
--thRow = tr_ . mconcat . map (th_ . toHtml)


-- | Render the ODS table rows for a MultiBalanceReport.
-- Returns the heading row, 0 or more body rows, and the totals row if enabled.
multiBalanceReportAsSpreadsheet ::
  ReportOpts -> MultiBalanceReport ->
  ((Maybe Int, Maybe Int), [[Ods.Cell Ods.NumLines Text]])
multiBalanceReportAsSpreadsheet ropts mbr =
  let (upper,lower) = multiBalanceReportAsSpreadsheetHelper True ropts mbr
  in  ((Just 1, case layout_ ropts of LayoutWide _ -> Just 1; _ -> Nothing),
            upper ++ lower)


-- | Render a multi-column balance report as plain text suitable for console output.
multiBalanceReportAsText :: ReportOpts -> MultiBalanceReport -> TL.Text
multiBalanceReportAsText ropts@ReportOpts{..} r = TB.toLazyText $
    TB.fromText title
    <> TB.fromText "\n\n"
    <> multiBalanceReportTableAsText ropts (multiBalanceReportAsTable ropts r)
  where
    title = mtitle <> " in " <> showDateSpan (periodicReportSpan r) <> valuationdesc <> ":"

    mtitle = case (balancecalc_, balanceaccum_) of
        (CalcValueChange, PerPeriod  ) -> "Period-end value changes"
        (CalcValueChange, Cumulative ) -> "Cumulative period-end value changes"
        (CalcGain,        PerPeriod  ) -> "Incremental gain"
        (CalcGain,        Cumulative ) -> "Cumulative gain"
        (CalcGain,        Historical ) -> "Historical gain"
        (_,               PerPeriod  ) -> "Balance changes"
        (_,               Cumulative ) -> "Ending balances (cumulative)"
        (_,               Historical)  -> "Ending balances (historical)"
    valuationdesc =
        (case conversionop_ of
            Just ToCost -> ", converted to cost"
            _           -> "")
        <> (case value_ of
            Just (AtThen _mc)    -> ", valued at posting date"
            Just (AtEnd _mc) | changingValuation -> ""
            Just (AtEnd _mc)     -> ", valued at period ends"
            Just (AtNow _mc)     -> ", current value"
            Just (AtDate d _mc)  -> ", valued at " <> showDate d
            Nothing              -> "")

    changingValuation = case (balancecalc_, balanceaccum_) of
        (CalcValueChange, PerPeriod)  -> True
        (CalcValueChange, Cumulative) -> True
        _                                     -> False

-- | Given a table representing a multi-column balance report,
-- render it in a format suitable for console output.
-- Amounts with more than two commodities will be elided unless --no-elide is used.
multiBalanceReportTableAsText :: ReportOpts -> Table T.Text T.Text WideBuilder -> TB.Builder
multiBalanceReportTableAsText ReportOpts{..} = renderTableByRowsB tableopts renderCh renderRow
  where
    tableopts = def{tableBorders=multiColumnTableOuterBorder, prettyTable=pretty_}
    multiColumnTableOuterBorder = pretty_

    renderCh :: [Text] -> [Cell]
    renderCh
      | layout_ /= LayoutBare || transpose_ = fmap (textCell TopRight)
      | otherwise = zipWith ($) (textCell TopLeft : repeat (textCell TopRight))

    renderRow :: (Text, [WideBuilder]) -> (Cell, [Cell])
    renderRow (rh, row)
      | layout_ /= LayoutBare || transpose_ =
          (textCell TopLeft rh, fmap (Cell TopRight . pure) row)
      | otherwise =
          (textCell TopLeft rh, zipWith ($) (Cell TopLeft : repeat (Cell TopRight)) (fmap pure row))

-- | Build a 'Table' from a multi-column balance report.
multiBalanceReportAsTable :: ReportOpts -> MultiBalanceReport -> Table T.Text T.Text WideBuilder
multiBalanceReportAsTable opts@ReportOpts{summary_only_, average_, row_total_, balanceaccum_}
    (PeriodicReport spans items tr) =
   maybetranspose $
   addtotalrow $
   Table
     (Group multiColumnTableInterRowBorder    $ map Header $ concat accts)
     (Group multiColumnTableInterColumnBorder $ map Header colheadings)
     (concat rows)
  where
    totalscolumn = row_total_ && balanceaccum_ `notElem` [Cumulative, Historical]
    colheadings = ["Commodity" | layout_ opts == LayoutBare]
                  ++ (if not summary_only_ then map (reportPeriodName balanceaccum_ spans) spans else [])
                  ++ ["  Total" | totalscolumn]
                  ++ ["Average" | average_]
    fullRowAsTexts row =
      let rs = multiBalanceRowAsText opts row
       in (replicate (length rs) (renderacct row), rs)
    (accts, rows) = unzip $ fmap fullRowAsTexts items
    renderacct row =
        T.replicate ((prrDepth row - 1) * 2) " " <> prrDisplayName row
    addtotalrow
      | no_total_ opts = id
      | otherwise =
        let totalrows = multiBalanceRowAsText opts tr
            rowhdrs = Group NoLine $ map Header $ totalRowHeadingText : replicate (length totalrows - 1) ""
            colhdrs = Header [] -- unused, concatTables will discard
        in (flip (concatTables SingleLine) $ Table rowhdrs colhdrs totalrows)
    maybetranspose | transpose_ opts = \(Table rh ch vals) -> Table ch rh (transpose vals)
                   | otherwise       = id
    multiColumnTableInterRowBorder    = NoLine
    multiColumnTableInterColumnBorder = if pretty_ opts then SingleLine else NoLine

multiBalanceRowAsTextBuilders :: AmountFormat -> ReportOpts -> [DateSpan] -> PeriodicReportRow a MixedAmount -> [[WideBuilder]]
multiBalanceRowAsTextBuilders bopts ropts colspans row =
    map (map Ods.cellContent) $
    multiBalanceRowAsCellBuilders bopts ropts colspans row

multiBalanceRowAsCellBuilders ::
    AmountFormat -> ReportOpts -> [DateSpan] ->
    PeriodicReportRow a MixedAmount -> [[Ods.Cell Ods.NumLines WideBuilder]]
multiBalanceRowAsCellBuilders bopts ReportOpts{..} colspans (PeriodicReportRow _ as rowtot rowavg) =
    case layout_ of
      LayoutWide width -> [fmap (cellFromMixedAmount bopts{displayMaxWidth=width}) allamts]
      LayoutTall       -> paddedTranspose Ods.emptyCell
                           . fmap (cellsFromMixedAmount bopts{displayMaxWidth=Nothing})
                           $ allamts
      LayoutBare       -> zipWith (:) (map wbCell cs)  -- add symbols
                           . transpose                         -- each row becomes a list of Text quantities
                           . fmap (cellsFromMixedAmount bopts{displayCommodity=False, displayCommodityOrder=Just cs, displayMinWidth=Nothing})
                           $ allamts
      LayoutTidy       -> concat
                           . zipWith (map . addDateColumns) colspans
                           . fmap ( zipWith (\c a -> [wbCell c, a]) cs
                                  . cellsFromMixedAmount bopts{displayCommodity=False, displayCommodityOrder=Just cs, displayMinWidth=Nothing})
                           $ as  -- Do not include totals column or average for tidy output, as this
                                 -- complicates the data representation and can be easily calculated
  where
    wbCell = Ods.defaultCell . wbFromText
    wbDate content = (wbCell content) {Ods.cellType = Ods.TypeDate}
    totalscolumn = row_total_ && balanceaccum_ `notElem` [Cumulative, Historical]
    cs = if all mixedAmountLooksZero allamts then [""] else S.toList $ foldMap maCommodities allamts
    allamts = (if not summary_only_ then as else []) ++
                [rowtot | totalscolumn && not (null as)] ++
                [rowavg | average_ && not (null as)]
    addDateColumns spn@(DateSpan s e) = (wbCell (showDateSpan spn) :)
                                       . (wbDate (maybe "" showEFDate s) :)
                                       . (wbDate (maybe "" (showEFDate . modifyEFDay (addDays (-1))) e) :)

    paddedTranspose :: a -> [[a]] -> [[a]]
    paddedTranspose _ [] = [[]]
    paddedTranspose n as1 = take (maximum . map length $ as1) . trans $ as1
        where
          trans ([] : xss)  = (n : map h xss) :  trans ([n] : map t xss)
          trans ((x : xs) : xss) = (x : map h xss) : trans (m xs : map t xss)
          trans [] = []
          h (x:_) = x
          h [] = n
          t (_:xs) = xs
          t [] = [n]
          m (x:xs) = x:xs
          m [] = [n]


multiBalanceRowAsText :: ReportOpts -> PeriodicReportRow a MixedAmount -> [[WideBuilder]]
multiBalanceRowAsText opts = multiBalanceRowAsTextBuilders oneLineNoCostFmt{displayColour=color_ opts} opts []

multiBalanceRowAsCsvText :: ReportOpts -> [DateSpan] -> PeriodicReportRow a MixedAmount -> [[T.Text]]
multiBalanceRowAsCsvText opts colspans = fmap (fmap wbToText) . multiBalanceRowAsTextBuilders machineFmt opts colspans

-- Budget reports

-- A BudgetCell's data values rendered for display - the actual change amount,
-- the budget goal amount if any, and the corresponding goal percentage if possible.
type BudgetDisplayCell = (WideBuilder, Maybe (WideBuilder, Maybe WideBuilder))

-- | A row of rendered budget data cells.
type BudgetDisplayRow  = [BudgetDisplayCell]

-- | An amount render helper for the budget report. Renders each commodity separately.
type BudgetShowAmountsFn   = MixedAmount -> [WideBuilder]

-- | A goal percentage calculating helper for the budget report.
type BudgetCalcPercentagesFn  = Change -> BudgetGoal -> [Maybe Percentage]

-- | Render a budget report as plain text suitable for console output.
budgetReportAsText :: ReportOpts -> BudgetReport -> TL.Text
budgetReportAsText ropts@ReportOpts{..} budgetr = TB.toLazyText $
    TB.fromText title <> TB.fromText "\n\n" <>
      multiBalanceReportTableAsText ropts (budgetReportAsTable ropts budgetr)
  where
    title = "Budget performance in " <> showDateSpan (periodicReportSpan budgetr)
           <> (case conversionop_ of
                 Just ToCost -> ", converted to cost"
                 _           -> "")
           <> (case value_ of
                 Just (AtThen _mc)   -> ", valued at posting date"
                 Just (AtEnd _mc)    -> ", valued at period ends"
                 Just (AtNow _mc)    -> ", current value"
                 Just (AtDate d _mc) -> ", valued at " <> showDate d
                 Nothing             -> "")
           <> ":"

-- | Build a 'Table' from a multi-column balance report.
budgetReportAsTable :: ReportOpts -> BudgetReport -> Table Text Text WideBuilder
budgetReportAsTable ReportOpts{..} (PeriodicReport spans items totrow) =
  maybetransposetable $
  addtotalrow $
    Table
      (Group budgetTableInterRowBorder    $ map Header accts)
      (Group budgetTableInterColumnBorder $ map Header colheadings)
      rows
  where
    budgetTableInterRowBorder    = NoLine
    budgetTableInterColumnBorder = if pretty_ then SingleLine else NoLine

    maybetransposetable
      | transpose_ = \(Table rh ch vals) -> Table ch rh (transpose vals)
      | otherwise  = id

    addtotalrow
      | no_total_ = id
      | otherwise =
        let 
          rowhdrs = Group NoLine $ map Header $ totalRowHeadingBudgetText : replicate (length totalrows - 1) ""
          colhdrs = Header [] -- ignored by concatTables
        in
          (flip (concatTables SingleLine) $ Table rowhdrs colhdrs totalrows)  -- XXX ?

    colheadings = ["Commodity" | layout_ == LayoutBare]
                  ++ map (reportPeriodName balanceaccum_ spans) spans
                  ++ ["  Total" | row_total_]
                  ++ ["Average" | average_]

    (accts, rows, totalrows) =
      (accts'
      ,maybecommcol itemscs  $ showcells  texts
      ,maybecommcol totrowcs $ showtotrow totrowtexts)
      where
        -- If --layout=bare, prepend a commodities column.
        maybecommcol :: [WideBuilder] -> [[WideBuilder]] -> [[WideBuilder]]
        maybecommcol cs
          | layout_ == LayoutBare = zipWith (:) cs
          | otherwise             = id

        showcells, showtotrow :: [[BudgetDisplayCell]] -> [[WideBuilder]]
        (showcells, showtotrow) =
          (maybetranspose . map (zipWith showBudgetDisplayCell widths)       . maybetranspose
          ,maybetranspose . map (zipWith showBudgetDisplayCell totrowwidths) . maybetranspose)
          where
            -- | Combine a BudgetDisplayCell's rendered values into a "[PERCENT of GOAL]" rendering,
            -- respecting the given widths.
            showBudgetDisplayCell :: (Int, Int, Int) -> BudgetDisplayCell -> WideBuilder
            showBudgetDisplayCell (actualwidth, budgetwidth, percentwidth) (actual, mbudget) =
              flip WideBuilder (actualwidth + totalbudgetwidth) $
                toPadded actual <> maybe emptycell showBudgetGoalAndPercentage mbudget

              where
                toPadded (WideBuilder b w) = (TB.fromText . flip T.replicate " " $ actualwidth - w) <> b

                (totalpercentwidth, totalbudgetwidth) =
                  let totalpercentwidth' = if percentwidth == 0 then 0 else percentwidth + 5
                   in ( totalpercentwidth'
                      , if budgetwidth == 0 then 0 else budgetwidth + totalpercentwidth' + 3
                      )

                emptycell :: TB.Builder
                emptycell = TB.fromText $ T.replicate totalbudgetwidth " "

                showBudgetGoalAndPercentage :: (WideBuilder, Maybe WideBuilder) -> TB.Builder
                showBudgetGoalAndPercentage (goal, perc) =
                  let perct = case perc of
                        Nothing  -> T.replicate totalpercentwidth " "
                        Just pct -> T.replicate (percentwidth - wbWidth pct) " " <> wbToText pct <> "% of "
                   in TB.fromText $ " [" <> perct <> T.replicate (budgetwidth - wbWidth goal) " " <> wbToText goal <> "]"

            -- | Build a list of widths for each column.
            -- When --transpose is used, the totals row must be included in this list.
            widths :: [(Int, Int, Int)]
            widths = zip3 actualwidths budgetwidths percentwidths
              where
                actualwidths  = map (maximum' . map first3 ) $ cols
                budgetwidths  = map (maximum' . map second3) $ cols
                percentwidths = map (maximum' . map third3 ) $ cols
                catcolumnwidths = foldl' (zipWith (++)) $ repeat []
                cols = maybetranspose $ catcolumnwidths $ map (cellswidth . rowToBudgetCells) items ++ [cellswidth $ rowToBudgetCells totrow]

                cellswidth :: [BudgetCell] -> [[(Int, Int, Int)]]
                cellswidth row =
                  let cs = budgetCellsCommodities row
                      (showmixed, percbudget) = mkBudgetDisplayFns cs
                      disp = showcell showmixed percbudget
                      budgetpercwidth = wbWidth *** maybe 0 wbWidth
                      cellwidth (am, bm) = let (bw, pw) = maybe (0, 0) budgetpercwidth bm in (wbWidth am, bw, pw)
                   in map (map cellwidth . disp) row

            totrowwidths :: [(Int, Int, Int)]
            totrowwidths
              | transpose_ = drop (length texts) widths
              | otherwise = widths

            maybetranspose
              | transpose_ = transpose
              | otherwise  = id

        (accts', itemscs, texts) = unzip3 $ concat shownitems
          where
            shownitems :: [[(AccountName, WideBuilder, BudgetDisplayRow)]]
            shownitems =
              map (\i ->
                let
                  addacctcolumn = map (\(cs, cvals) -> (renderacct i, cs, cvals))
                  isunbudgetedrow = displayFull (prrName i) == unbudgetedAccountName
                in addacctcolumn $ showrow isunbudgetedrow $ rowToBudgetCells i)
              items
              where
                -- FIXME. Have to check explicitly for which to render here, since
                -- budgetReport sets accountlistmode to ALTree. Find a principled way to do
                -- this.
                renderacct row = case accountlistmode_ of
                  ALTree -> T.replicate ((prrDepth row - 1)*2) " " <> prrDisplayName row
                  ALFlat -> accountNameDrop (drop_) $ prrFullName row

        (totrowcs, totrowtexts)  = unzip  $ concat showntotrow
          where
            showntotrow :: [[(WideBuilder, BudgetDisplayRow)]]
            showntotrow = [showrow False $ rowToBudgetCells totrow]

        -- | Get the data cells from a row or totals row, maybe adding 
        -- the row total and/or row average depending on options.
        rowToBudgetCells :: PeriodicReportRow a BudgetCell -> [BudgetCell]
        rowToBudgetCells (PeriodicReportRow _ as rowtot rowavg) = as
            ++ [rowtot | row_total_ && not (null as)]
            ++ [rowavg | average_   && not (null as)]

        -- | Render a row's data cells as "BudgetDisplayCell"s, and a rendered list of commodity symbols.
        -- Also requires a flag indicating whether this is the special <unbudgeted> row.
        -- (The types make that hard to check here.)
        showrow :: Bool -> [BudgetCell] -> [(WideBuilder, BudgetDisplayRow)]
        showrow isunbudgetedrow cells =
          let
            cs = budgetCellsCommodities cells
            -- #2071 If there are no commodities - because there are no actual or goal amounts -
            -- the zipped list would be empty, causing this row not to be shown.
            -- But rows like this sometimes need to be shown to preserve the account tree structure.
            -- So, ensure 0 will be shown as actual amount(s).
            -- Unfortunately this disables boring parent eliding, as if --no-elide had been used.
            -- (Just turning on --no-elide higher up doesn't work right.)
            -- Note, no goal amount will be shown for these rows,
            -- whereas --no-elide is likely to show a goal amount aggregated from children.
            cs1 = if null cs && not isunbudgetedrow then [""] else cs
            (showmixed, percbudget) = mkBudgetDisplayFns cs1
          in
            zip (map wbFromText cs1) $
            transpose $
            map (showcell showmixed percbudget)
            cells

        budgetCellsCommodities :: [BudgetCell] -> [CommoditySymbol]
        budgetCellsCommodities = S.toList . foldl' S.union mempty . map budgetCellCommodities
          where
            budgetCellCommodities :: BudgetCell -> S.Set CommoditySymbol
            budgetCellCommodities (am, bm) = f am `S.union` f bm
              where f = maybe mempty maCommodities

        -- | Render a "BudgetCell"'s amounts as "BudgetDisplayCell"s (one per commodity).
        showcell :: BudgetShowAmountsFn -> BudgetCalcPercentagesFn -> BudgetCell -> BudgetDisplayRow
        showcell showCommodityAmounts calcCommodityPercentages (mactual, mbudget) =
          zip actualamts budgetinfos
          where
            actual = fromMaybe nullmixedamt mactual
            actualamts = showCommodityAmounts actual
            budgetinfos =
              case mbudget of
                Nothing   -> repeat Nothing
                Just goal -> map Just $ showGoalAmountsAndPercentages goal
                where
                  showGoalAmountsAndPercentages :: MixedAmount -> [(WideBuilder, Maybe WideBuilder)]
                  showGoalAmountsAndPercentages goal = zip amts mpcts
                    where
                      amts  = showCommodityAmounts goal
                      mpcts = map (showrounded <$>) $ calcCommodityPercentages actual goal
                        where showrounded = wbFromText . T.pack . show . roundTo 0

        -- | Make budget info display helpers that adapt to --layout=wide.
        mkBudgetDisplayFns :: [CommoditySymbol] -> (BudgetShowAmountsFn, BudgetCalcPercentagesFn)
        mkBudgetDisplayFns cs = case layout_ of
          LayoutWide width ->
               ( pure . showMixedAmountB oneLineNoCostFmt{displayMaxWidth=width, displayColour=color_}
               , \a -> pure . percentage a)
          _ -> ( showMixedAmountLinesB noCostFmt{displayCommodity=layout_/=LayoutBare, displayCommodityOrder=Just cs, displayMinWidth=Nothing, displayColour=color_}
               , \a b -> map (percentage' a b) cs)
          where
            -- | Calculate the percentage of actual change to budget goal to show, if any.
            -- If valuing at cost, both amounts are converted to cost before comparing.
            -- A percentage will not be shown if:
            --
            -- - actual or goal are not the same, single, commodity
            --
            -- - the goal is zero
            --
            percentage :: Change -> BudgetGoal -> Maybe Percentage
            percentage actual budget =
              case (costedAmounts actual, costedAmounts budget) of
                ([a], [b]) | (acommodity a == acommodity b || amountLooksZero a) && not (amountLooksZero b)
                    -> Just $ 100 * aquantity a / aquantity b
                _   -> Nothing
              where
                costedAmounts = case conversionop_ of
                    Just ToCost -> amounts . mixedAmountCost
                    _           -> amounts

            -- | Like percentage, but accept multicommodity actual and budget amounts,
            -- and extract the specified commodity from both.
            percentage' :: Change -> BudgetGoal -> CommoditySymbol -> Maybe Percentage
            percentage' am bm c = case ((,) `on` find ((==) c . acommodity) . amounts) am bm of
                (Just a, Just b) -> percentage (mixedAmount a) (mixedAmount b)
                _                -> Nothing

-- XXX generalise this with multiBalanceReportAsCsv ?
-- | Render a budget report as CSV. Like multiBalanceReportAsCsv,
-- but includes alternating actual and budget amount columns.
budgetReportAsCsv :: ReportOpts -> BudgetReport -> [[Text]]
budgetReportAsCsv ropts report
  = map (map Ods.cellContent) $
    budgetReportAsSpreadsheet ropts report

budgetReportAsSpreadsheet ::
  ReportOpts -> BudgetReport -> [[Ods.Cell Ods.NumLines Text]]
budgetReportAsSpreadsheet
  ReportOpts{..}
  (PeriodicReport colspans items totrow)
  = (if transpose_ then Ods.transpose else id) $

  -- heading row
  (map headerCell $
  "Account" :
  ["Commodity" | layout_ == LayoutBare ]
   ++ concatMap (\spn -> [showDateSpan spn, "budget"]) colspans
   ++ concat [["Total"  ,"budget"] | row_total_]
   ++ concat [["Average","budget"] | average_]
  ) :

  -- account rows
  concatMap (rowAsTexts prrFullName) items

  -- totals row
  ++ addTotalBorders
        (concat [ rowAsTexts (const totalRowHeadingBudgetCsv) totrow | not no_total_ ])

  where
    cell = Ods.defaultCell
    flattentuples tups = concat [[a,b] | (a,b) <- tups]
    showNorm = maybe Ods.emptyCell (fmap wbToText . cellFromMixedAmount oneLineNoCostFmt)

    rowAsTexts :: (PeriodicReportRow a BudgetCell -> Text)
               -> PeriodicReportRow a BudgetCell
               -> [[Ods.Cell Ods.NumLines Text]]
    rowAsTexts render row@(PeriodicReportRow _ as (rowtot,budgettot) (rowavg, budgetavg))
      | layout_ /= LayoutBare = [cell (render row) : map showNorm vals]
      | otherwise =
            joinNames . zipWith (:) (map cell cs)  -- add symbols and names
          . transpose                   -- each row becomes a list of Text quantities
          . map (map (fmap wbToText) . cellsFromMixedAmount dopts . fromMaybe nullmixedamt)
          $ vals
      where
        cs = S.toList . mconcat . map maCommodities $ catMaybes vals
        dopts = oneLineNoCostFmt{displayCommodity=layout_ /= LayoutBare, displayCommodityOrder=Just cs, displayMinWidth=Nothing}
        vals = flattentuples as
            ++ concat [[rowtot, budgettot] | row_total_]
            ++ concat [[rowavg, budgetavg] | average_]

        joinNames = map (cell (render row) :)


-- tests

tests_Balance = testGroup "Balance" [

   testGroup "balanceReportAsText" [
    testCase "unicode in balance layout" $ do
      j <- readJournal' "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
      let rspec = defreportspec{_rsReportOpts=defreportopts{no_total_=True}}
      TB.toLazyText (balanceReportAsText (_rsReportOpts rspec) (balanceReport rspec{_rsDay=fromGregorian 2008 11 26} j))
        @?=
        TL.unlines
        ["                -100  актив:наличные"
        ,"                 100  расходы:покупки"
        ]
    ]

  ]
