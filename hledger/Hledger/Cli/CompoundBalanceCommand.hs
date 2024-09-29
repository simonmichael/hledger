{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase #-}
{-|

Common helpers for making multi-section balance report commands
like balancesheet, cashflow, and incomestatement.

-}

module Hledger.Cli.CompoundBalanceCommand (
  CompoundBalanceCommandSpec(..)
 ,compoundBalanceCommandMode
 ,compoundBalanceCommand
) where

import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day, addDays)
import System.Console.CmdArgs.Explicit as C (Mode, flagNone, flagReq)
import Hledger.Write.Csv (CSV, printCSV, printTSV)
import Hledger.Write.Html.Attribute (stylesheet_, collapse, lpad, alignright)
import qualified Hledger.Write.Html as Html
import qualified Hledger.Write.Spreadsheet as Spr
import Lucid as L hiding (value_)
import Safe (tailDef)
import Text.Tabular.AsciiWide as Tabular hiding (render)

import Hledger
import Hledger.Cli.Commands.Balance
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (unsupportedOutputFormatError, writeOutputLazyText)
import Data.Function ((&))
import Control.Monad (when)

-- | Description of a compound balance report command,
-- from which we generate the command's cmdargs mode and IO action.
-- A compound balance report command shows one or more sections/subreports,
-- each with its own title and subtotals row, in a certain order,
-- plus a grand totals row if there's more than one section.
-- Examples are the balancesheet, cashflow and incomestatement commands.
--
-- Compound balance reports do sign normalisation: they show all account balances
-- as normally positive, unlike the ordinary BalanceReport and most hledger commands
-- which show income/liability/equity balances as normally negative.
-- Each subreport specifies the normal sign of its amounts, and whether
-- it should be added to or subtracted from the grand total.
--
data CompoundBalanceCommandSpec = CompoundBalanceCommandSpec {
  cbcdoc      :: CommandHelpStr,                  -- ^ the command's name(s) and documentation
  cbctitle    :: String,                          -- ^ overall report title
  cbcqueries  :: [CBCSubreportSpec DisplayName],  -- ^ subreport details
  cbcaccum    :: BalanceAccumulation              -- ^ how to accumulate balances (per-period, cumulative, historical)
                                                  --   (overrides command line flags)
}

-- | Generate a cmdargs option-parsing mode from a compound balance command
-- specification.
compoundBalanceCommandMode :: CompoundBalanceCommandSpec -> Mode RawOpts
compoundBalanceCommandMode CompoundBalanceCommandSpec{..} =
  hledgerCommandMode
   cbcdoc
   ([flagNone ["sum"] (setboolopt "sum")
      "show sum of posting amounts (default)"
   ,flagNone ["valuechange"] (setboolopt "valuechange")
      "show total change of period-end historical balance value (caused by deposits, withdrawals, market price fluctuations)"
    ,flagNone ["gain"] (setboolopt "gain")
      "show unrealised capital gain/loss (historical balance value minus cost basis)"
   ,flagNone ["budget"] (setboolopt "budget")
      "show sum of posting amounts compared to budget goals defined by periodic transactions"

   ,flagNone ["change"] (setboolopt "change")
       ("accumulate amounts from column start to column end (in multicolumn reports)"
           ++ defaultMarker PerPeriod)
    ,flagNone ["cumulative"] (setboolopt "cumulative")
       ("accumulate amounts from report start (specified by e.g. -b/--begin) to column end"
           ++ defaultMarker Cumulative)
    ,flagNone ["historical","H"] (setboolopt "historical")
       ("accumulate amounts from journal start to column end (includes postings before report start date)"
           ++ defaultMarker Historical)
    ]
    ++ flattreeflags True ++
    [flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
    ,flagNone ["declared"] (setboolopt "declared") "include non-parent declared accounts (best used with -E)"
    ,flagNone ["average","A"] (setboolopt "average") "show a row average column (in multicolumn reports)"
    ,flagNone ["row-total","T"] (setboolopt "row-total") "show a row total column (in multicolumn reports)"
    ,flagNone ["summary-only"] (setboolopt "summary-only") "display only row summaries (e.g. row total, average) (in multicolumn reports)"
    ,flagNone ["no-total","N"] (setboolopt "no-total") "omit the final total row"
    ,flagNone ["no-elide"] (setboolopt "no-elide") "don't squash boring parent accounts (in tree mode)"
    ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
    ,flagReq  ["base-url"] (\s opts -> Right $ setopt "base-url" s opts) "URLPREFIX" "add anchors to table cells with resepct to this base URL"
    ,flagNone ["sort-amount","S"] (setboolopt "sort-amount") "sort by amount instead of account code/name"
    ,flagNone ["percent", "%"] (setboolopt "percent") "express values in percentage of each column's total"
    ,flagReq  ["layout"] (\s opts -> Right $ setopt "layout" s opts) "ARG"
      (unlines
        ["how to show multi-commodity amounts:"
        ,"'wide[,WIDTH]': all commodities on one line"
        ,"'tall'        : each commodity on a new line"
        ,"'bare'        : bare numbers, symbols in a column"
        ])
    ,outputFormatFlag ["txt","html","csv","tsv","json"]
    ,outputFileFlag
    ])
    cligeneralflagsgroups1
    (hiddenflags ++
      [ flagNone ["commodity-column"] (setboolopt "commodity-column")
        "show commodity symbols in a separate column, amounts as bare numbers, one row per commodity"
      ])
    ([], Just $ argsFlag "[QUERY]")
 where
   defaultMarker :: BalanceAccumulation -> String
   defaultMarker bacc | bacc == cbcaccum = " (default)"
                      | otherwise        = ""

-- | Generate a runnable command from a compound balance command specification.
compoundBalanceCommand :: CompoundBalanceCommandSpec -> (CliOpts -> Journal -> IO ())
compoundBalanceCommand CompoundBalanceCommandSpec{..} opts@CliOpts{reportspec_=rspec, rawopts_=rawopts} j = do
    writeOutputLazyText opts $ render $ styleAmounts styles cbr
  where
    styles = journalCommodityStylesWith HardRounding j
    ropts@ReportOpts{..} = _rsReportOpts rspec
    -- use the default balance type for this report, unless the user overrides
    mbalanceAccumulationOverride = balanceAccumulationOverride rawopts
    balanceaccumulation = fromMaybe cbcaccum mbalanceAccumulationOverride
    -- Set balance type in the report options.
    ropts' = ropts{balanceaccum_=balanceaccumulation}

    title =
         maybe "" (<>" ") mintervalstr
      <> T.pack cbctitle
      <> " "
      <> titledatestr
      <> maybe "" (" "<>) mtitleclarification
      <> valuationdesc
      where

        -- XXX #1078 the title of ending balance reports
        -- (Historical) should mention the end date(s) shown as
        -- column heading(s) (not the date span of the transactions).
        -- Also the dates should not be simplified (it should show
        -- "2008/01/01-2008/12/31", not "2008").
        titledatestr = case balanceaccumulation of
            Historical -> showEndDates enddates
            _          -> showDateSpan requestedspan
          where
            enddates = map (addDays (-1)) . mapMaybe spanEnd $ cbrDates cbr  -- these spans will always have a definite end date
            requestedspan = fst $ reportSpan j rspec

        mintervalstr = showInterval interval_

        -- when user overrides, add an indication to the report title
        -- Do we need to deal with overridden BalanceCalculation?
        mtitleclarification = case (balancecalc_, balanceaccumulation, mbalanceAccumulationOverride) of
            (CalcValueChange, PerPeriod,  _              ) -> Just "(Period-End Value Changes)"
            (CalcValueChange, Cumulative, _              ) -> Just "(Cumulative Period-End Value Changes)"
            (CalcGain,        PerPeriod,  _              ) -> Just "(Incremental Gain)"
            (CalcGain,        Cumulative, _              ) -> Just "(Cumulative Gain)"
            (CalcGain,        Historical, _              ) -> Just "(Historical Gain)"
            (_,               _,          Just PerPeriod ) -> Just "(Balance Changes)"
            (_,               _,          Just Cumulative) -> Just "(Cumulative Ending Balances)"
            (_,               _,          Just Historical) -> Just "(Historical Ending Balances)"
            _                                              -> Nothing

        valuationdesc =
          (case conversionop_ of
               Just ToCost -> ", converted to cost"
               _           -> "")
          <> (case value_ of
               Just (AtThen _mc)       -> ", valued at posting date"
               Just (AtEnd _mc) | changingValuation -> ""
               Just (AtEnd _mc)        -> ", valued at period ends"
               Just (AtNow _mc)        -> ", current value"
               Just (AtDate today _mc) -> ", valued at " <> showDate today
               Nothing                 -> "")

        changingValuation = case (balancecalc_, balanceaccum_) of
            (CalcValueChange, PerPeriod)  -> True
            (CalcValueChange, Cumulative) -> True
            _                             -> False

    -- make a CompoundBalanceReport.
    cbr' = compoundBalanceReport rspec{_rsReportOpts=ropts'} j cbcqueries
    cbr  = cbr'{cbrTitle=title}

    -- render appropriately
    render = case outputFormatFromOpts opts of
      "txt"  -> compoundBalanceReportAsText ropts'
      "csv"  -> printCSV . compoundBalanceReportAsCsv ropts'
      "tsv"  -> printTSV . compoundBalanceReportAsCsv ropts'
      "html" -> L.renderText . compoundBalanceReportAsHtml ropts'
      "json" -> toJsonText
      x      -> error' $ unsupportedOutputFormatError x

-- | Show a simplified description of an Interval.
showInterval :: Interval -> Maybe T.Text
showInterval = \case
  NoInterval -> Nothing
  Days 1     -> Just "Daily"
  Weeks 1    -> Just "Weekly"
  Weeks 2    -> Just "Biweekly"
  Months 1   -> Just "Monthly"
  Months 2   -> Just "Bimonthly"
  Months 3   -> Just "Quarterly"
  Months 6   -> Just "Half-yearly"
  Months 12  -> Just "Yearly"
  Quarters 1 -> Just "Quarterly"
  Quarters 2 -> Just "Half-yearly"
  Years 1    -> Just "Yearly"
  Years 2    -> Just "Biannual"
  _          -> Just "Periodic"

-- | Summarise one or more (inclusive) end dates, in a way that's
-- visually different from showDateSpan, suggesting discrete end dates
-- rather than a continuous span.
showEndDates :: [Day] -> T.Text
showEndDates es = case es of
  -- cf showPeriod
  (e:_:_) -> showDate e <> ".." <> showDate (last es)
  [e]     -> showDate e
  []      -> ""

-- | Render a compound balance report as plain text suitable for console output.
{- Eg:
Balance Sheet

             ||  2017/12/31    Total  Average
=============++===============================
 Assets      ||
-------------++-------------------------------
 assets:b    ||           1        1        1
-------------++-------------------------------
             ||           1        1        1
=============++===============================
 Liabilities ||
-------------++-------------------------------
-------------++-------------------------------
             ||
=============++===============================
 Total       ||           1        1        1

-}
compoundBalanceReportAsText :: ReportOpts -> CompoundPeriodicReport DisplayName MixedAmount -> TL.Text
compoundBalanceReportAsText ropts (CompoundPeriodicReport title _colspans subreports totalsrow) =
  TB.toLazyText $
    TB.fromText title <> TB.fromText "\n\n" <>
    multiBalanceReportTableAsText ropts bigtablewithtotalsrow
  where
    bigtable =
      case map (subreportAsTable ropts) subreports of
        []   -> Tabular.empty
        r:rs -> foldl' (concatTables tableInterSubreportBorder) r rs
    bigtablewithtotalsrow =
      if no_total_ ropts || length subreports == 1
      then bigtable
      else concatTables tableGrandTotalsTopBorder bigtable totalstable
        where
          -- Append the report's grand column totals at the bottom of the table.
          -- Note "row" is confusingly overloaded here; *Report rows, Table rows,
          -- and visually apparent table rows are all distinct.
          -- With multiple currencies, in some layout modes, the column totals (a single report row)
          -- occupy multiple lines, which currently we put into multiple table rows,
          -- for convenience I guess, borderless so they look like a single visual row.
          --
          -- multiBalanceRowAsText gets a matrix of each line of each column total rendered as text
          -- (actually as WideBuilders), in line-major-order:
          --  [
          --   [COL1LINE1, COL2LINE1]
          --   [COL1LINE2, COL2LINE2]
          --  ]
          coltotalslines = multiBalanceRowAsText ropts totalsrow
          totalstable = Table
            (Group NoLine $ map Header $ "Net:" : replicate (length coltotalslines - 1) "")  -- row headers
            (Header [])     -- column headers, concatTables will discard these
            coltotalslines  -- cell values         

    -- | Convert a named multi balance report to a table suitable for
    -- concatenating with others to make a compound balance report table.
    subreportAsTable ropts1 (title1, r, _) = tablewithtitle
      where
        tablewithtitle = Table
          (Group tableSubreportTitleBottomBorder [Header title1, lefthdrs])  -- row headers
          tophdrs     -- column headers
          ([]:cells)  -- cell values
          where
            Table lefthdrs tophdrs cells = multiBalanceReportAsTable ropts1 r

    tableSubreportTitleBottomBorder = SingleLine
    tableInterSubreportBorder       = DoubleLine
    tableGrandTotalsTopBorder       = DoubleLine

-- | Render a compound balance report as CSV.
-- Subreports' CSV is concatenated, with the headings rows replaced by a
-- subreport title row, and an overall title row, one headings row, and an
-- optional overall totals row is added.
compoundBalanceReportAsCsv :: ReportOpts -> CompoundPeriodicReport DisplayName MixedAmount -> CSV
compoundBalanceReportAsCsv ropts (CompoundPeriodicReport title colspans subreports totalrow) =
    addtotals $
      padRow title
      : ( "Account"
        : ["Commodity" | layout_ ropts == LayoutBare]
        ++ map (reportPeriodName (balanceaccum_ ropts) colspans) colspans
        ++ (if row_total_ ropts then ["Total"] else [])
        ++ (if average_ ropts then ["Average"] else [])
        )
      : concatMap (subreportAsCsv ropts) subreports
  where
    -- | Add a subreport title row and drop the heading row.
    subreportAsCsv ropts1 (subreporttitle, multibalreport, _) =
      padRow subreporttitle :
      tailDef [] (multiBalanceReportAsCsv ropts1 multibalreport)
    padRow s = take numcols $ s : repeat ""
      where
        numcols
          | null subreports = 1
          | otherwise =
            (1 +) $ -- account name column
            (if layout_ ropts == LayoutBare then (1+) else id) $
            (if row_total_ ropts then (1+) else id) $
            (if average_ ropts then (1+) else id) $
            maximum $ -- depends on non-null subreports
            map (length . prDates . second3) subreports
    addtotals
      | no_total_ ropts || length subreports == 1 = id
      | otherwise = (++ map ("Net:" : ) (multiBalanceRowAsCsvText ropts colspans totalrow))

-- | Render a compound balance report as HTML.
compoundBalanceReportAsHtml :: ReportOpts -> CompoundPeriodicReport DisplayName MixedAmount -> Html ()
compoundBalanceReportAsHtml ropts cbr =
  let
    CompoundPeriodicReport title colspans subreports totalrow = cbr
    colspanattr = colspan_ $ T.pack $ show $ sum [
      1,
      length colspans,
      if row_total_ ropts then 1 else 0,
      if average_ ropts then 1 else 0,
      if layout_ ropts == LayoutBare then 1 else 0
      ]
    leftattr = style_ "text-align:left"
    blankrow = tr_ $ td_ [colspanattr] $ toHtmlRaw ("&nbsp;"::String)

    titlerows =
      [tr_ $ th_ [colspanattr, leftattr] $ h2_ $ toHtml title
      ,tr_ $ do
          th_ ""
          when (layout_ ropts == LayoutBare) $ th_ "Commodity"
          mconcat $ map (th_ [style_ alignright] . toHtml . reportPeriodName (balanceaccum_ ropts) colspans) colspans
          when (row_total_ ropts) $ th_ "Total"
          when (average_   ropts) $ th_ "Average"
      ]

    -- Make rows for a subreport: its title row, not the headings row,
    -- the data rows, any totals row, and a blank row for whitespace.
    subreportrows :: (T.Text, MultiBalanceReport, Bool) -> [Html ()]
    subreportrows (subreporttitle, mbr, _increasestotal) =
      let
        (_,bodyrows,mtotalsrows) = multiBalanceReportHtmlRows ropts mbr
      in
           [tr_ $ th_ [colspanattr, leftattr, class_ "account"] $ toHtml subreporttitle]
        ++ bodyrows
        ++ mtotalsrows
        ++ [blankrow]

    totalrows =
      if no_total_ ropts || length subreports == 1 then []
      else
        multiBalanceRowAsCellBuilders oneLineNoCostFmt ropts colspans Total totalrow
                             -- make a table of rendered lines of the report totals row
        & map (map (fmap wbToText))
        & zipWith (:) (Spr.defaultCell "Net:" : repeat Spr.emptyCell)
                             -- insert a headings column, with Net: on the first line only
        & addTotalBorders    -- marking the first for special styling
        & map (Html.formatRow . map (fmap L.toHtml))
                             -- convert to a list of HTML totals rows

  in do
    link_ [rel_ "stylesheet", href_ "hledger.css"]
    stylesheet_ [
      ("table",collapse),
      ("th, td",lpad),
      ("th.account, td.account","padding-left:0;"),
      ("td:nth-child(1)", "white-space:nowrap"),
      ("tr:nth-child(even) td", "background-color:#eee")
      ]
    table_ $ mconcat $
         titlerows
      ++ [blankrow]
      ++ concatMap subreportrows subreports
      ++ totalrows

