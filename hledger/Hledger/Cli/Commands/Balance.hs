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

There are three kinds of multi-column balance report, indicated by the heading:

* A \"period balance\" (or \"flow\") report (the default) shows the change of account
  balance in each period, which is equivalent to the sum of postings in each
  period. Here, checking's balance increased by 10 in Feb:

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

* periodic:      accounts with postings and non-zero period balance in any period

* cumulative:    accounts with non-zero cumulative balance in any period

* historical:    accounts with non-zero historical balance in any period

With @-E/--empty@:

* single-column: accounts with postings in report period

* periodic:      accounts with postings in report period

* cumulative:    accounts with postings in report period

* historical:    accounts with non-zero starting balance +
                 accounts with postings in report period

/Which periods (columns) to show in balance reports/

An empty period/column is one where no report account has any postings.
A zero period/column is one where no report account has a non-zero period balance.

Currently,

by default:

* single-column: N/A

* periodic:      all periods within the overall report period,
                 except for leading and trailing empty periods

* cumulative:    all periods within the overall report period,
                 except for leading and trailing empty periods

* historical:    all periods within the overall report period,
                 except for leading and trailing empty periods

With @-E/--empty@:

* single-column: N/A

* periodic:      all periods within the overall report period

* cumulative:    all periods within the overall report period

* historical:    all periods within the overall report period

/What to show in empty cells/

An empty periodic balance report cell is one which has no corresponding postings.
An empty cumulative/historical balance report cell is one which has no corresponding
or prior postings, ie the account doesn't exist yet.
Currently, empty cells show 0.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

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

import Data.List
import Data.Maybe
--import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (fromGregorian)
import System.Console.CmdArgs.Explicit as C
import Lucid as L
import Text.Printf (printf)
import Text.Tabular as T
import Text.Tabular.AsciiWide (renderWidth)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Hledger.Read.CsvReader (CSV, printCSV)


-- | Command line options for this command.
balancemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Balance.txt")
  ([flagNone ["change"] (setboolopt "change")
      "show balance change in each period (default)"
   ,flagNone ["cumulative"] (setboolopt "cumulative")
      "show balance change accumulated across periods (in multicolumn reports)"
   ,flagNone ["historical","H"] (setboolopt "historical")
      "show historical ending balance in each period (includes postings before report start date)\n "
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
   ,flagNone ["budget"] (setboolopt "budget") "show performance compared to budget goals defined by periodic transactions"
   ,outputFormatFlag ["txt","html","csv","json"]
   ,outputFileFlag
   ]
  )
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The balance command, prints a balance report.
balance :: CliOpts -> Journal -> IO ()
balance opts@CliOpts{rawopts_=rawopts,reportspec_=rspec} j = do
    let ropts@ReportOpts{..} = rsOpts rspec
        budget      = boolopt "budget" rawopts
        multiperiod = interval_ /= NoInterval
        fmt         = outputFormatFromOpts opts

    if budget then do  -- single or multi period budget report
      let reportspan = reportSpan j rspec
          budgetreport = dbg4 "budgetreport" $ budgetReport rspec assrt reportspan j
            where
              assrt = not $ ignore_assertions_ $ inputopts_ opts
          render = case fmt of
            "txt"  -> budgetReportAsText ropts
            "json" -> (++"\n") . TL.unpack . toJsonText
            _      -> const $ error' $ unsupportedOutputFormatError fmt
      writeOutput opts $ render budgetreport

    else
      if multiperiod then do  -- multi period balance report
        let report = multiBalanceReport rspec j
            render = case fmt of
              "txt"  -> multiBalanceReportAsText ropts
              "csv"  -> (++"\n") . printCSV . multiBalanceReportAsCsv ropts
              "html" -> (++"\n") . TL.unpack . L.renderText . multiBalanceReportAsHtml ropts
              "json" -> (++"\n") . TL.unpack . toJsonText
              _      -> const $ error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        writeOutput opts $ render report

      else do  -- single period simple balance report
        let report = balanceReport rspec j -- simple Ledger-style balance report
            render = case fmt of
              "txt"  -> balanceReportAsText
              "csv"  -> \ropts r -> (++ "\n") $ printCSV $ balanceReportAsCsv ropts r
              "json" -> const $ (++"\n") . TL.unpack . toJsonText
              _      -> const $ error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
        writeOutput opts $ render ropts report

-- rendering single-column balance reports

-- | Render a single-column balance report as CSV.
balanceReportAsCsv :: ReportOpts -> BalanceReport -> CSV
balanceReportAsCsv opts (items, total) =
  ["account","balance"] :
  [[T.unpack a, showMixedAmountOneLineWithoutPrice False b] | (a, _, _, b) <- items]
  ++
  if no_total_ opts
  then []
  else [["total", showMixedAmountOneLineWithoutPrice False total]]

-- | Render a single-column balance report as plain text.
balanceReportAsText :: ReportOpts -> BalanceReport -> String
balanceReportAsText opts ((items, total)) = unlines $
    concat lines ++ if no_total_ opts then [] else overline : totallines
  where
    lines = map (balanceReportItemAsText opts) items
    -- abuse renderBalanceReportItem to render the total with similar format
    acctcolwidth = maximum' [T.length fullname | (fullname, _, _, _) <- items]
    totallines = map rstrip $ renderBalanceReportItem opts (T.replicate (acctcolwidth+1) " ", 0, total)
    -- with a custom format, extend the line to the full report width;
    -- otherwise show the usual 20-char line for compatibility
    overlinewidth = fromMaybe (maximum' . map length $ concat lines) . overlineWidth $ format_ opts
    overline   = replicate overlinewidth '-'

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
balanceReportItemAsText :: ReportOpts -> BalanceReportItem -> [String]
balanceReportItemAsText opts (_, accountName, depth, amt) =
  renderBalanceReportItem opts (
    accountName,
    depth,
    normaliseMixedAmountSquashPricesForDisplay amt
    )

-- | Render a balance report item using the given StringFormat, generating one or more lines of text.
renderBalanceReportItem :: ReportOpts -> (AccountName, Int, MixedAmount) -> [String]
renderBalanceReportItem opts (acctname, depth, total) =
  lines $ case format_ opts of
      OneLine       _ comps -> concatOneLine      $ render1 comps
      TopAligned    _ comps -> concatBottomPadded $ render comps
      BottomAligned _ comps -> concatTopPadded    $ render comps
  where
    render1 = map (renderComponent1 opts (acctname, depth, total))
    render  = map (renderComponent opts (acctname, depth, total))

-- | Render one StringFormat component for a balance report item.
renderComponent :: ReportOpts -> (AccountName, Int, MixedAmount) -> StringFormatComponent -> String
renderComponent _ _ (FormatLiteral s) = s
renderComponent opts (acctname, depth, total) (FormatField ljust min max field) = case field of
  DepthSpacerField -> formatString ljust Nothing max $ replicate d ' '
                      where d = case min of
                                 Just m  -> depth * m
                                 Nothing -> depth
  AccountField     -> formatString ljust min max (T.unpack acctname)
  TotalField       -> fitStringMulti min max True False $ showMixedAmountWithoutPrice (color_ opts) total
  _                -> ""

-- | Render one StringFormat component for a balance report item.
-- This variant is for use with OneLine string formats; it squashes
-- any multi-line rendered values onto one line, comma-and-space separated,
-- while still complying with the width spec.
renderComponent1 :: ReportOpts -> (AccountName, Int, MixedAmount) -> StringFormatComponent -> String
renderComponent1 _ _ (FormatLiteral s) = s
renderComponent1 opts (acctname, depth, total) (FormatField ljust min max field) = case field of
  AccountField     -> formatString ljust min max ((intercalate ", " . lines) (indented (T.unpack acctname)))
                      where
                        -- better to indent the account name here rather than use a DepthField component
                        -- so that it complies with width spec. Uses a fixed indent step size.
                        indented = ((replicate (depth*2) ' ')++)
  TotalField       -> fitStringMulti min max True False $ ((intercalate ", " . map strip . lines) (showamt total))
    where
      showamt = showMixedAmountWithoutPrice (color_ opts)
  _                -> ""

-- rendering multi-column balance reports

-- | Render a multi-column balance report as CSV.
-- The CSV will always include the initial headings row,
-- and will include the final totals row unless --no-total is set.
multiBalanceReportAsCsv :: ReportOpts -> MultiBalanceReport -> CSV
multiBalanceReportAsCsv opts@ReportOpts{average_, row_total_}
    (PeriodicReport colspans items (PeriodicReportRow _ coltotals tot avg)) =
  maybetranspose $
  ("Account" : map showDateSpan colspans
   ++ ["Total"   | row_total_]
   ++ ["Average" | average_]
  ) :
  [T.unpack (displayFull a) :
   map (showMixedAmountOneLineWithoutPrice False)
   (amts
    ++ [rowtot | row_total_]
    ++ [rowavg | average_])
  | PeriodicReportRow a amts rowtot rowavg <- items]
  ++
  if no_total_ opts
  then []
  else ["Total:" :
        map (showMixedAmountOneLineWithoutPrice False) (
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
multiBalanceReportHtmlHeadRow :: ReportOpts -> [String] -> Html ()
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
multiBalanceReportHtmlBodyRow :: ReportOpts -> [String] -> Html ()
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
multiBalanceReportHtmlFootRow :: ReportOpts -> [String] -> Html ()
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
multiBalanceReportAsText :: ReportOpts -> MultiBalanceReport -> String
multiBalanceReportAsText ropts@ReportOpts{..} r =
    title ++ "\n\n" ++ (balanceReportTableAsText ropts $ balanceReportAsTable ropts r)
  where
    multiperiod = interval_ /= NoInterval
    title = printf "%s in %s%s:"
      (case balancetype_ of
        PeriodChange       -> "Balance changes"
        CumulativeChange   -> "Ending balances (cumulative)"
        HistoricalBalance  -> "Ending balances (historical)")
      (showDateSpan $ periodicReportSpan r)
      (case value_ of
        Just (AtCost _mc)   -> ", valued at cost"
        Just (AtThen _mc)   -> error' unsupportedValueThenError  -- TODO -- ", valued at period ends"  -- handled like AtEnd for now  -- PARTIAL:
        Just (AtEnd _mc)    -> ", valued at period ends"
        Just (AtNow _mc)    -> ", current value"
        -- XXX duplicates the above
        Just (AtDefault _mc) | multiperiod -> ", valued at period ends"
        Just (AtDefault _mc) -> ", current value"
        Just (AtDate d _mc) -> ", valued at "++showDate d
        Nothing             -> "")

-- | Build a 'Table' from a multi-column balance report.
balanceReportAsTable :: ReportOpts -> MultiBalanceReport -> Table String String MixedAmount
balanceReportAsTable opts@ReportOpts{average_, row_total_, balancetype_}
    (PeriodicReport spans items (PeriodicReportRow _ coltotals tot avg)) =
   maybetranspose $
   addtotalrow $
   Table
     (T.Group NoLine $ map Header accts)
     (T.Group NoLine $ map Header colheadings)
     (map rowvals items)
  where
    totalscolumn = row_total_ && balancetype_ `notElem` [CumulativeChange, HistoricalBalance]
    colheadings = map (reportPeriodName balancetype_ spans) spans
                  ++ ["  Total" | totalscolumn]
                  ++ ["Average" | average_]
    accts = map renderacct items
    renderacct row =
        replicate ((prrDepth row - 1) * 2) ' ' ++ T.unpack (prrDisplayName row)
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
balanceReportTableAsText :: ReportOpts -> Table String String MixedAmount -> String
balanceReportTableAsText ReportOpts{..} =
    trimBorder . renderWidth pretty_tables_ id id showamt . leftAlignRowHeaders
  where
    showamt = showMixedOneLine showAmountWithoutPrice Nothing mmax color_
    mmax = if no_elide_ then Nothing else Just 22


tests_Balance = tests "Balance" [

   tests "balanceReportAsText" [
    test "unicode in balance layout" $ do
      j <- readJournal' "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
      let rspec = defreportspec
      balanceReportAsText (rsOpts rspec) (balanceReport rspec{rsToday=fromGregorian 2008 11 26} j)
        @?=
        unlines
        ["                -100  актив:наличные"
        ,"                 100  расходы:покупки"
        ,"--------------------"
        ,"                   0"
        ]
    ]

  ]
