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

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}

module Hledger.Cli.Commands.Balance (
  balancemode
 ,balance
 ,balanceReportAsText
 ,balanceReportItemAsText
 ,multiBalanceReportAsText
 ,multiBalanceReportAsCsv
 ,multiBalanceReportAsHtml
 ,multiBalanceReportHtmlRows
 ,balanceReportAsTable
 ,balanceReportTableAsText
 ,tests_Balance
) where

import Data.Default (def)
import Data.List (intersperse, transpose)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time (fromGregorian)
import System.Console.CmdArgs.Explicit as C
import Lucid as L
import Text.Tabular.AsciiWide as Tab

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Hledger.Read.CsvReader (CSV, printCSV)


-- | Command line options for this command.
balancemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Balance.txt")
  (
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
      "show change of value of period-end historical balances"
    ,flagNone ["change"] (setboolopt "change")
      "accumulate amounts from column start to column end (in multicolumn reports, default)"
    ,flagNone ["cumulative"] (setboolopt "cumulative")
      "accumulate amounts from report start (specified by e.g. -b/--begin) to column end"
    ,flagNone ["historical","H"] (setboolopt "historical")
      "accumulate amounts from journal start to column end (includes postings before report start date)\n "
    ]
    ++ flattreeflags True ++
    [flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "omit N leading account name parts (in flat mode)"
    ,flagNone ["average","A"] (setboolopt "average") "show a row average column (in multicolumn reports)"
    ,flagNone ["row-total","T"] (setboolopt "row-total") "show a row total column (in multicolumn reports)"
    ,flagNone ["no-total","N"] (setboolopt "no-total") "omit the final total row"
    ,flagNone ["no-elide"] (setboolopt "no-elide") "don't squash boring parent accounts (in tree mode); don't show only 2 commodities per amount"
    ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
    ,flagNone ["pretty-tables"] (setboolopt "pretty-tables") "use unicode to display prettier tables"
    ,flagNone ["sort-amount","S"] (setboolopt "sort-amount") "sort by amount instead of account code/name (in flat mode). With multiple columns, sorts by the row total, or by row average if that is displayed."
    ,flagNone ["percent", "%"] (setboolopt "percent") "express values in percentage of each column's total"
    ,flagNone ["invert"] (setboolopt "invert") "display all amounts with reversed sign"
    ,flagNone ["transpose"] (setboolopt "transpose") "transpose rows and columns"
    ,outputFormatFlag ["txt","html","csv","json"]
    ,outputFileFlag
    ]
  )
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The balance command, prints a balance report.
balance :: CliOpts -> Journal -> IO ()
balance opts@CliOpts{reportspec_=rspec} j = case balancecalc_ of
    CalcBudget -> do  -- single or multi period budget report
      let reportspan = reportSpan j rspec
          budgetreport = budgetReport rspec (balancingopts_ $ inputopts_ opts) reportspan j
          render = case fmt of
            "txt"  -> budgetReportAsText ropts
            "json" -> (<>"\n") . toJsonText
            "csv"  -> printCSV . budgetReportAsCsv ropts
            _      -> error' $ unsupportedOutputFormatError fmt
      writeOutputLazyText opts $ render budgetreport

    _ | multiperiod -> do  -- multi period balance report
        let report = multiBalanceReport rspec j
            render = case fmt of
              "txt"  -> multiBalanceReportAsText ropts
              "csv"  -> printCSV . multiBalanceReportAsCsv ropts
              "html" -> (<>"\n") . L.renderText . multiBalanceReportAsHtml ropts
              "json" -> (<>"\n") . toJsonText
              _      -> const $ error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        writeOutputLazyText opts $ render report

    _ -> do  -- single period simple balance report
        let report = balanceReport rspec j -- simple Ledger-style balance report
            render = case fmt of
              "txt"  -> \ropts -> TB.toLazyText . balanceReportAsText ropts
              "csv"  -> \ropts -> printCSV . balanceReportAsCsv ropts
              -- "html" -> \ropts -> (<>"\n") . L.renderText . multiBalanceReportAsHtml ropts . balanceReportAsMultiBalanceReport ropts
              "json" -> const $ (<>"\n") . toJsonText
              _      -> error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        writeOutputLazyText opts $ render ropts report
  where
    ropts@ReportOpts{..} = rsOpts rspec
    multiperiod = interval_ /= NoInterval
    fmt         = outputFormatFromOpts opts

-- XXX this allows rough HTML rendering of a flat BalanceReport, but it can't handle tree mode etc.
-- -- | Convert a BalanceReport to a MultiBalanceReport.
-- balanceReportAsMultiBalanceReport :: ReportOpts -> BalanceReport -> MultiBalanceReport 
-- balanceReportAsMultiBalanceReport _ropts (britems, brtotal) = 
--   let
--     mbrrows = 
--       [PeriodicReportRow{
--           prrName    = flatDisplayName brfullname
--         , prrAmounts = [bramt]
--         , prrTotal   = bramt
--         , prrAverage = bramt
--         }
--       | (brfullname, _, _, bramt) <- britems
--       ]
--   in
--     PeriodicReport{
--         prDates  = [nulldatespan]
--       , prRows   = mbrrows
--       , prTotals = PeriodicReportRow{
--            prrName=()
--           ,prrAmounts=[brtotal]
--           ,prrTotal=brtotal
--           ,prrAverage=brtotal
--           }
--       }

-- XXX should all the per-report, per-format rendering code live in the command module,
-- like the below, or in the report module, like budgetReportAsText/budgetReportAsCsv ?

-- rendering single-column balance reports

-- | Render a single-column balance report as CSV.
balanceReportAsCsv :: ReportOpts -> BalanceReport -> CSV
balanceReportAsCsv opts (items, total) =
  ["account","balance"] :
  [[accountNameDrop (drop_ opts) a, wbToText $ showMixedAmountB (balanceOpts False opts) b] | (a, _, _, b) <- items]
  ++
  if no_total_ opts
  then []
  else [["total", wbToText $ showMixedAmountB (balanceOpts False opts) total]]

-- | Render a single-column balance report as plain text.
balanceReportAsText :: ReportOpts -> BalanceReport -> TB.Builder
balanceReportAsText opts ((items, total)) =
    unlinesB lines
    <> unlinesB (if no_total_ opts then [] else [overline, totalLines])
  where
    (lines, sizes) = unzip $ map (balanceReportItemAsText opts) items
    -- abuse renderBalanceReportItem to render the total with similar format
    (totalLines, _) = renderBalanceReportItem opts ("",0,total)
    -- with a custom format, extend the line to the full report width;
    -- otherwise show the usual 20-char line for compatibility
    overlinewidth = case format_ opts of
        OneLine       ((FormatField _ _ _ TotalField):_) -> 20
        TopAligned    ((FormatField _ _ _ TotalField):_) -> 20
        BottomAligned ((FormatField _ _ _ TotalField):_) -> 20
        _ -> sum (map maximum' $ transpose sizes)
    overline   = TB.fromText $ T.replicate overlinewidth "-"

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
balanceReportItemAsText opts (_, accountName, depth, amt) =
  renderBalanceReportItem opts (accountName, depth, amt)

-- | Render a balance report item using the given StringFormat, generating one or more lines of text.
renderBalanceReportItem :: ReportOpts -> (AccountName, Int, MixedAmount) -> (TB.Builder, [Int])
renderBalanceReportItem opts (acctname, depth, total) =
  case format_ opts of
      OneLine       comps -> renderRow' $ render True  True  comps
      TopAligned    comps -> renderRow' $ render True  False comps
      BottomAligned comps -> renderRow' $ render False False comps
  where
    renderRow' is = ( renderRowB def{tableBorders=False, borderSpaces=False}
                      . Tab.Group NoLine $ map Header is
                    , map cellWidth is )

    render topaligned oneline = map (maybeConcat . renderComponent topaligned opts (acctname, depth, total))
      where maybeConcat (Cell a xs) =
                if oneline then Cell a [WideBuilder (mconcat . intersperse (TB.fromText ", ") $ map wbBuilder xs) width]
                           else Cell a xs
              where width = sumStrict (map ((+2) . wbWidth) xs) -2


-- | Render one StringFormat component for a balance report item.
renderComponent :: Bool -> ReportOpts -> (AccountName, Int, MixedAmount) -> StringFormatComponent -> Cell
renderComponent _ _ _ (FormatLiteral s) = textCell TopLeft s
renderComponent topaligned opts (acctname, depth, total) (FormatField ljust mmin mmax field) = case field of
    DepthSpacerField -> Cell align [WideBuilder (TB.fromText $ T.replicate d " ") d]
                        where d = maybe id min mmax $ depth * fromMaybe 1 mmin
    AccountField     -> textCell align $ formatText ljust mmin mmax acctname
    TotalField       -> Cell align . pure $ showMixedAmountB dopts total
    _                -> Cell align [mempty]
  where
    align = if topaligned then (if ljust then TopLeft    else TopRight)
                          else (if ljust then BottomLeft else BottomRight)
    dopts = (balanceOpts True opts){displayOneLine=False, displayMinWidth=mmin, displayMaxWidth=mmax}

-- rendering multi-column balance reports

-- | Render a multi-column balance report as CSV.
-- The CSV will always include the initial headings row,
-- and will include the final totals row unless --no-total is set.
multiBalanceReportAsCsv :: ReportOpts -> MultiBalanceReport -> CSV
multiBalanceReportAsCsv opts@ReportOpts{average_, row_total_}
    (PeriodicReport colspans items (PeriodicReportRow _ coltotals tot avg)) =
  maybetranspose $
  ("account" : map showDateSpan colspans
   ++ ["total"   | row_total_]
   ++ ["average" | average_]
  ) :
  [accountNameDrop (drop_ opts) (displayFull a) :
   map (wbToText . showMixedAmountB (balanceOpts False opts))
   (amts
    ++ [rowtot | row_total_]
    ++ [rowavg | average_])
  | PeriodicReportRow a amts rowtot rowavg <- items]
  ++
  if no_total_ opts
  then []
  else ["total" :
        map (wbToText . showMixedAmountB (balanceOpts False opts)) (
          coltotals
          ++ [tot | row_total_]
          ++ [avg | average_]
          )]
  where
    maybetranspose | transpose_ opts = transpose
                   | otherwise = id

-- | Render a multi-column balance report as HTML.
multiBalanceReportAsHtml :: ReportOpts -> MultiBalanceReport -> Html ()
multiBalanceReportAsHtml ropts mbr =
  let
    (headingsrow,bodyrows,mtotalsrow) = multiBalanceReportHtmlRows ropts mbr
  in
    table_ $ mconcat $
         [headingsrow]
      ++ bodyrows
      ++ maybeToList mtotalsrow

-- | Render the HTML table rows for a MultiBalanceReport.
-- Returns the heading row, 0 or more body rows, and the totals row if enabled.
multiBalanceReportHtmlRows :: ReportOpts -> MultiBalanceReport -> (Html (), [Html ()], Maybe (Html ()))
multiBalanceReportHtmlRows ropts mbr =
  let
    headingsrow:rest | transpose_ ropts = error' "Sorry, --transpose with HTML output is not yet supported"  -- PARTIAL:
                     | otherwise = multiBalanceReportAsCsv ropts mbr
    (bodyrows, mtotalsrow) | no_total_ ropts = (rest,      Nothing)
                           | otherwise       = (init rest, Just $ last rest)
  in
    (multiBalanceReportHtmlHeadRow ropts headingsrow
    ,map (multiBalanceReportHtmlBodyRow ropts) bodyrows
    ,multiBalanceReportHtmlFootRow ropts <$> mtotalsrow -- TODO pad totals row with zeros when there are
    )

-- | Render one MultiBalanceReport heading row as a HTML table row.
multiBalanceReportHtmlHeadRow :: ReportOpts -> [T.Text] -> Html ()
multiBalanceReportHtmlHeadRow _ [] = mempty  -- shouldn't happen
multiBalanceReportHtmlHeadRow ropts (acct:rest) =
  let
    defstyle = style_ ""
    (amts,tot,avg)
      | row_total_ ropts && average_ ropts = (init $ init rest, [last $ init rest], [last rest])
      | row_total_ ropts                   = (init rest,        [last rest],        [])
      |                     average_ ropts = (init rest,        [],                 [last rest])
      | otherwise                          = (rest,             [],                 [])
  in
    tr_ $ mconcat $
          td_ [class_ "account"]              (toHtml acct)
       : [td_ [class_ "", defstyle]           (toHtml a) | a <- amts]
      ++ [td_ [class_ "rowtotal", defstyle]   (toHtml a) | a <- tot]
      ++ [td_ [class_ "rowaverage", defstyle] (toHtml a) | a <- avg]

-- | Render one MultiBalanceReport data row as a HTML table row.
multiBalanceReportHtmlBodyRow :: ReportOpts -> [T.Text] -> Html ()
multiBalanceReportHtmlBodyRow _ [] = mempty  -- shouldn't happen
multiBalanceReportHtmlBodyRow ropts (label:rest) =
  let
    defstyle = style_ "text-align:right"
    (amts,tot,avg)
      | row_total_ ropts && average_ ropts = (init $ init rest, [last $ init rest], [last rest])
      | row_total_ ropts                   = (init rest,        [last rest],        [])
      |                     average_ ropts = (init rest,        [],                 [last rest])
      | otherwise                          = (rest,             [],                 [])
  in
    tr_ $ mconcat $
          td_ [class_ "account", style_ "text-align:left"]  (toHtml label)
       : [td_ [class_ "amount", defstyle]            (toHtml a) | a <- amts]
      ++ [td_ [class_ "amount rowtotal", defstyle]   (toHtml a) | a <- tot]
      ++ [td_ [class_ "amount rowaverage", defstyle] (toHtml a) | a <- avg]

-- | Render one MultiBalanceReport totals row as a HTML table row.
multiBalanceReportHtmlFootRow :: ReportOpts -> [T.Text] -> Html ()
multiBalanceReportHtmlFootRow _ropts [] = mempty
-- TODO pad totals row with zeros when subreport is empty
--  multiBalanceReportHtmlFootRow ropts $
--     ""
--   : repeat nullmixedamt zeros
--  ++ (if row_total_ ropts then [nullmixedamt] else [])
--  ++ (if average_ ropts   then [nullmixedamt]   else [])
multiBalanceReportHtmlFootRow ropts (acct:rest) =
  let
    defstyle = style_ "text-align:right"
    (amts,tot,avg)
      | row_total_ ropts && average_ ropts = (init $ init rest, [last $ init rest], [last rest])
      | row_total_ ropts                   = (init rest,        [last rest],        [])
      |                     average_ ropts = (init rest,        [],                 [last rest])
      | otherwise                          = (rest,             [],                 [])
  in
    tr_ $ mconcat $
          th_ [style_ "text-align:left"]             (toHtml acct)
       : [th_ [class_ "amount coltotal", defstyle]   (toHtml a) | a <- amts]
      ++ [th_ [class_ "amount coltotal", defstyle]   (toHtml a) | a <- tot]
      ++ [th_ [class_ "amount colaverage", defstyle] (toHtml a) | a <- avg]

--thRow :: [String] -> Html ()
--thRow = tr_ . mconcat . map (th_ . toHtml)

-- | Render a multi-column balance report as plain text suitable for console output.
multiBalanceReportAsText :: ReportOpts -> MultiBalanceReport -> TL.Text
multiBalanceReportAsText ropts@ReportOpts{..} r = TB.toLazyText $
    TB.fromText title
    <> TB.fromText "\n\n"
    <> balanceReportTableAsText ropts (balanceReportAsTable ropts r)
  where
    title = mtitle <> " in " <> showDateSpan (periodicReportSpan r) <> valuationdesc <> ":"

    mtitle = case (balancecalc_, balanceaccum_) of
        (CalcValueChange, PerPeriod  ) -> "Period-end value changes"
        (CalcValueChange, Cumulative ) -> "Cumulative period-end value changes"
        (_,               PerPeriod  ) -> "Balance changes"
        (_,               Cumulative ) -> "Ending balances (cumulative)"
        (_,               Historical)  -> "Ending balances (historical)"
    valuationdesc =
        (case cost_ of
            Cost   -> ", converted to cost"
            NoCost -> "")
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

-- | Build a 'Table' from a multi-column balance report.
balanceReportAsTable :: ReportOpts -> MultiBalanceReport -> Table T.Text T.Text MixedAmount
balanceReportAsTable opts@ReportOpts{average_, row_total_, balanceaccum_}
    (PeriodicReport spans items (PeriodicReportRow _ coltotals tot avg)) =
   maybetranspose $
   addtotalrow $
   Table
     (Tab.Group NoLine $ map Header accts)
     (Tab.Group NoLine $ map Header colheadings)
     (map rowvals items)
  where
    totalscolumn = row_total_ && balanceaccum_ `notElem` [Cumulative, Historical]
    colheadings = map (reportPeriodName balanceaccum_ spans) spans
                  ++ ["  Total" | totalscolumn]
                  ++ ["Average" | average_]
    accts = map renderacct items
    renderacct row =
        T.replicate ((prrDepth row - 1) * 2) " " <> prrDisplayName row
    rowvals (PeriodicReportRow _ as rowtot rowavg) = as
                             ++ [rowtot | totalscolumn]
                             ++ [rowavg | average_]
    addtotalrow | no_total_ opts = id
                | otherwise      = (+----+ (row "" $
                                    coltotals
                                    ++ [tot | totalscolumn && not (null coltotals)]
                                    ++ [avg | average_   && not (null coltotals)]
                                    ))
    maybetranspose | transpose_ opts = \(Table rh ch vals) -> Table ch rh (transpose vals)
                   | otherwise       = id

-- | Given a table representing a multi-column balance report (for example,
-- made using 'balanceReportAsTable'), render it in a format suitable for
-- console output. Amounts with more than two commodities will be elided
-- unless --no-elide is used.
balanceReportTableAsText :: ReportOpts -> Table T.Text T.Text MixedAmount -> TB.Builder
balanceReportTableAsText ropts@ReportOpts{..} =
    Tab.renderTableB def{tableBorders=False, prettyTable=pretty_tables_}
        (Tab.textCell TopLeft) (Tab.textCell TopRight) $
        Cell TopRight . pure . showMixedAmountB (balanceOpts True ropts)

-- | Amount display options to use for balance reports
balanceOpts :: Bool -> ReportOpts -> AmountDisplayOpts
balanceOpts isTable ReportOpts{..} = oneLine
    { displayColour   = isTable && color_
    , displayMaxWidth = if isTable && not no_elide_ then Just 32 else Nothing
    }

tests_Balance = tests "Balance" [

   tests "balanceReportAsText" [
    test "unicode in balance layout" $ do
      j <- readJournal' "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
      let rspec = defreportspec{rsOpts=defreportopts{no_total_=True}}
      TB.toLazyText (balanceReportAsText (rsOpts rspec) (balanceReport rspec{rsToday=fromGregorian 2008 11 26} j))
        @?=
        TL.unlines
        ["                -100  актив:наличные"
        ,"                 100  расходы:покупки"
        ]
    ]

  ]
