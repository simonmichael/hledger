{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
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
import Data.Maybe
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar
import System.Console.CmdArgs.Explicit as C
import Hledger.Read.CsvReader (CSV, printCSV)
import Lucid as L hiding (value_)
import Text.Tabular as T

import Hledger
import Hledger.Cli.Commands.Balance
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (unsupportedOutputFormatError, writeOutput)

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
  cbcdoc      :: CommandDoc,          -- ^ the command's name(s) and documentation
  cbctitle    :: String,              -- ^ overall report title
  cbcqueries  :: [CBCSubreportSpec],  -- ^ subreport details
  cbctype     :: BalanceType          -- ^ the "balance" type (change, cumulative, historical)
                                      --   this report shows (overrides command line flags)
}

-- | Generate a cmdargs option-parsing mode from a compound balance command
-- specification.
compoundBalanceCommandMode :: CompoundBalanceCommandSpec -> Mode RawOpts
compoundBalanceCommandMode CompoundBalanceCommandSpec{..} =
  hledgerCommandMode
    cbcdoc
    [flagNone ["change"] (setboolopt "change")
       ("show balance change in each period" ++ defType PeriodChange)
    ,flagNone ["cumulative"] (setboolopt "cumulative")
       ("show balance change accumulated across periods (in multicolumn reports)"
           ++ defType CumulativeChange
       )
    ,flagNone ["historical","H"] (setboolopt "historical")
       ("show historical ending balance in each period (includes postings before report start date)"
           ++ defType HistoricalBalance
       )
    ,flagNone ["flat"] (setboolopt "flat") "show accounts as a list"
    ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
    ,flagNone ["no-total","N"] (setboolopt "no-total") "omit the final total row"
    ,flagNone ["tree"] (setboolopt "tree") "show accounts as a tree; amounts include subaccounts (default in simple reports)"
    ,flagNone ["average","A"] (setboolopt "average") "show a row average column (in multicolumn reports)"
    ,flagNone ["row-total","T"] (setboolopt "row-total") "show a row total column (in multicolumn reports)"
    ,flagNone ["no-elide"] (setboolopt "no-elide") "don't squash boring parent accounts (in tree mode)"
    ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
    ,flagNone ["pretty-tables"] (setboolopt "pretty-tables") "use unicode when displaying tables"
    ,flagNone ["sort-amount","S"] (setboolopt "sort-amount") "sort by amount instead of account code/name"
    ,flagNone ["percent", "%"] (setboolopt "percent") "express values in percentage of each column's total"
    ,outputFormatFlag ["txt","html","csv","json"]
    ,outputFileFlag
    ]
    [generalflagsgroup1]
    hiddenflags
    ([], Just $ argsFlag "[QUERY]")
 where
   defType :: BalanceType -> String
   defType bt | bt == cbctype = " (default)"
              | otherwise    = ""

-- | Generate a runnable command from a compound balance command specification.
compoundBalanceCommand :: CompoundBalanceCommandSpec -> (CliOpts -> Journal -> IO ())
compoundBalanceCommand CompoundBalanceCommandSpec{..} opts@CliOpts{reportopts_=ropts@ReportOpts{..}, rawopts_=rawopts} j = do
    today <- getCurrentDay
    let
      -- use the default balance type for this report, unless the user overrides
      mBalanceTypeOverride =
        choiceopt parse rawopts where
          parse = \case
            "historical" -> Just HistoricalBalance
            "cumulative" -> Just CumulativeChange
            "change"     -> Just PeriodChange
            _            -> Nothing
      balancetype = fromMaybe cbctype mBalanceTypeOverride
      -- Set balance type in the report options.
      -- Also, use tree mode (by default, at least?) if --cumulative/--historical
      -- are used in single column mode, since in that situation we will be using
      -- balanceReportFromMultiBalanceReport which does not support eliding boring parents,
      -- and tree mode hides this.. or something.. XXX
      ropts' = ropts{
        balancetype_=balancetype,
        accountlistmode_=if not (flat_ ropts) && interval_==NoInterval && balancetype `elem` [CumulativeChange, HistoricalBalance] then ALTree else accountlistmode_,
        no_total_=if percent_ && length cbcqueries > 1 then True else no_total_
      }

      title =
        cbctitle
        ++ " "
        ++ titledatestr
        ++ maybe "" (' ':) mtitleclarification
        ++ valuationdesc
        where

          -- XXX #1078 the title of ending balance reports
          -- (HistoricalBalance) should mention the end date(s) shown as
          -- column heading(s) (not the date span of the transactions).
          -- Also the dates should not be simplified (it should show
          -- "2008/01/01-2008/12/31", not "2008").
          titledatestr = case balancetype of
              HistoricalBalance -> showEndDates enddates
              _                 -> showDateSpan requestedspan
            where
              enddates = map (addDays (-1)) . mapMaybe spanEnd $ cbrDates cbr  -- these spans will always have a definite end date
              requestedspan = queryDateSpan date2_ (queryFromOpts today ropts')
                                  `spanDefaultsFrom` journalDateSpan date2_ j

          -- when user overrides, add an indication to the report title
          mtitleclarification = flip fmap mBalanceTypeOverride $ \t ->
            case t of
              PeriodChange      -> "(Balance Changes)"
              CumulativeChange  -> "(Cumulative Ending Balances)"
              HistoricalBalance -> "(Historical Ending Balances)"

          valuationdesc = case value_ of
            Just (AtCost _mc)       -> ", valued at cost"
            Just (AtThen _mc)       -> error' unsupportedValueThenError  -- TODO
            Just (AtEnd _mc)        -> ", valued at period ends"
            Just (AtNow _mc)        -> ", current value"
            Just (AtDefault _mc) | multiperiod   -> ", valued at period ends"
            Just (AtDefault _mc)    -> ", current value"
            Just (AtDate today _mc) -> ", valued at "++showDate today
            Nothing                 -> ""
            where multiperiod = interval_ /= NoInterval

      -- make a CompoundBalanceReport.
      cbr' = compoundBalanceReport today ropts' j cbcqueries
      cbr  = cbr'{cbrTitle=title}

    -- render appropriately
    writeOutput opts $ case outputFormatFromOpts opts of
        "txt"  -> compoundBalanceReportAsText ropts' cbr
        "csv"  -> printCSV (compoundBalanceReportAsCsv ropts cbr) ++ "\n"
        "html" -> (++"\n") $ TL.unpack $ L.renderText $ compoundBalanceReportAsHtml ropts cbr
        "json" -> (++"\n") $ TL.unpack $ toJsonText cbr
        x      -> error' $ unsupportedOutputFormatError x

-- | Summarise one or more (inclusive) end dates, in a way that's
-- visually different from showDateSpan, suggesting discrete end dates
-- rather than a continuous span.
showEndDates :: [Day] -> String
showEndDates es = case es of
  -- cf showPeriod
  (e:_:_) -> showdate e ++ ".." ++ showdate (last es)
  [e]     -> showdate e
  []      -> ""
  where
    showdate = show

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
compoundBalanceReportAsText :: ReportOpts -> CompoundBalanceReport -> String
compoundBalanceReportAsText ropts
  (CompoundPeriodicReport title _colspans subreports (PeriodicReportRow _ coltotals grandtotal grandavg)) =
    title ++ "\n\n" ++
    balanceReportTableAsText ropts bigtable'
  where
    bigtable =
      case map (subreportAsTable ropts) subreports of
        []   -> T.empty
        r:rs -> foldl' concatTables r rs
    bigtable'
      | no_total_ ropts || length subreports == 1 =
          bigtable
      | otherwise =
          bigtable
          +====+
          row "Net:" (
            coltotals
            ++ (if row_total_ ropts then [grandtotal] else [])
            ++ (if average_ ropts   then [grandavg]   else [])
            )

    -- | Convert a named multi balance report to a table suitable for
    -- concatenating with others to make a compound balance report table.
    subreportAsTable ropts (title, r, _) = t
      where
        -- convert to table
        Table lefthdrs tophdrs cells = balanceReportAsTable ropts r
        -- tweak the layout
        t = Table (T.Group SingleLine [Header title, lefthdrs]) tophdrs ([]:cells)

-- | Add the second table below the first, discarding its column headings.
concatTables (Table hLeft hTop dat) (Table hLeft' _ dat') =
    Table (T.Group DoubleLine [hLeft, hLeft']) hTop (dat ++ dat')

-- | Render a compound balance report as CSV.
-- Subreports' CSV is concatenated, with the headings rows replaced by a
-- subreport title row, and an overall title row, one headings row, and an
-- optional overall totals row is added.
compoundBalanceReportAsCsv :: ReportOpts -> CompoundBalanceReport -> CSV
compoundBalanceReportAsCsv ropts (CompoundPeriodicReport title colspans subreports (PeriodicReportRow _ coltotals grandtotal grandavg)) =
  addtotals $
  padRow title :
  ("Account" :
   map showDateSpanMonthAbbrev colspans
   ++ (if row_total_ ropts then ["Total"] else [])
   ++ (if average_ ropts then ["Average"] else [])
   ) :
  concatMap (subreportAsCsv ropts) subreports
  where
    -- | Add a subreport title row and drop the heading row.
    subreportAsCsv ropts (subreporttitle, multibalreport, _) =
      padRow subreporttitle :
      tail (multiBalanceReportAsCsv ropts multibalreport)
    padRow s = take numcols $ s : repeat ""
      where
        numcols
          | null subreports = 1
          | otherwise =
            (1 +) $ -- account name column
            (if row_total_ ropts then (1+) else id) $
            (if average_ ropts then (1+) else id) $
            maximum $ -- depends on non-null subreports
            map (length . prDates . second3) subreports
    addtotals
      | no_total_ ropts || length subreports == 1 = id
      | otherwise = (++
          ["Net:" :
           map showMixedAmountOneLineWithoutPrice (
             coltotals
             ++ (if row_total_ ropts then [grandtotal] else [])
             ++ (if average_ ropts   then [grandavg]   else [])
             )
          ])

-- | Render a compound balance report as HTML.
compoundBalanceReportAsHtml :: ReportOpts -> CompoundBalanceReport -> Html ()
compoundBalanceReportAsHtml ropts cbr =
  let
    CompoundPeriodicReport title colspans subreports (PeriodicReportRow _ coltotals grandtotal grandavg) = cbr
    colspanattr = colspan_ $ TS.pack $ show $
      1 + length colspans + (if row_total_ ropts then 1 else 0) + (if average_ ropts then 1 else 0)
    leftattr = style_ "text-align:left"
    blankrow = tr_ $ td_ [colspanattr] $ toHtmlRaw ("&nbsp;"::String)

    titlerows =
         [tr_ $ th_ [colspanattr, leftattr] $ h2_ $ toHtml title]
      ++ [thRow $
          "" :
          map showDateSpanMonthAbbrev colspans
          ++ (if row_total_ ropts then ["Total"] else [])
          ++ (if average_ ropts then ["Average"] else [])
          ]

    thRow :: [String] -> Html ()
    thRow = tr_ . mconcat . map (th_ . toHtml)

    -- Make rows for a subreport: its title row, not the headings row,
    -- the data rows, any totals row, and a blank row for whitespace.
    subreportrows :: (String, MultiBalanceReport, Bool) -> [Html ()]
    subreportrows (subreporttitle, mbr, _increasestotal) =
      let
        (_,bodyrows,mtotalsrow) = multiBalanceReportHtmlRows ropts mbr
      in
           [tr_ $ th_ [colspanattr, leftattr] $ toHtml subreporttitle]
        ++ bodyrows
        ++ maybe [] (:[]) mtotalsrow
        ++ [blankrow]

    totalrows | no_total_ ropts || length subreports == 1 = []
              | otherwise =
                  let defstyle = style_ "text-align:right"
                  in
                    [tr_ $ mconcat $
                         th_ [class_ "", style_ "text-align:left"] "Net:"
                       : [th_ [class_ "amount coltotal", defstyle] (toHtml $ showMixedAmountOneLineWithoutPrice a) | a <- coltotals]
                      ++ (if row_total_ ropts then [th_ [class_ "amount coltotal", defstyle] $ toHtml $ showMixedAmountOneLineWithoutPrice $ grandtotal] else [])
                      ++ (if average_ ropts   then [th_ [class_ "amount colaverage", defstyle] $ toHtml $ showMixedAmountOneLineWithoutPrice $ grandavg] else [])
                    ]

  in do
    style_ (TS.unlines [""
      ,"td { padding:0 0.5em; }"
      ,"td:nth-child(1) { white-space:nowrap; }"
      ,"tr:nth-child(even) td { background-color:#eee; }"
      ])
    link_ [rel_ "stylesheet", href_ "hledger.css"]
    table_ $ mconcat $
         titlerows
      ++ [blankrow]
      ++ concatMap subreportrows subreports
      ++ totalrows

