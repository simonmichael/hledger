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
An empty cumulative/historical balance report cell is one which has no correponding
or prior postings, ie the account doesn't exist yet.
Currently, empty cells show 0.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
 ,tests_Hledger_Cli_Commands_Balance
) where

import Data.List
import Data.Maybe
--import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Console.CmdArgs.Explicit as C
import Lucid as L
import Text.CSV
import Test.HUnit
import Text.Printf (printf)
import Text.Tabular as T
--import Text.Tabular.AsciiWide

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils


-- | Command line options for this command.
balancemode = (defCommandMode $ ["balance"] ++ aliases) { -- also accept but don't show the common bal alias
  modeHelp = "show accounts and balances" `withAliases` aliases
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagNone ["change"] (\opts -> setboolopt "change" opts)
        "show balance change in each period (default)"
     ,flagNone ["cumulative"] (\opts -> setboolopt "cumulative" opts)
        "show balance change accumulated across periods (in multicolumn reports)"
     ,flagNone ["historical","H"] (\opts -> setboolopt "historical" opts)
        "show historical ending balance in each period (includes postings before report start date)\n "
     ,flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show accounts as a tree; amounts include subaccounts (default in simple reports)"
     ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list; amounts exclude subaccounts except when account is depth-clipped (default in multicolumn reports)\n "
     ,flagNone ["average","A"] (\opts -> setboolopt "average" opts) "show a row average column (in multicolumn reports)"
     ,flagNone ["row-total","T"] (\opts -> setboolopt "row-total" opts) "show a row total column (in multicolumn reports)"
     ,flagNone ["no-total","N"] (\opts -> setboolopt "no-total" opts) "omit the final total row"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "omit N leading account name parts (in flat mode)"
     ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "don't squash boring parent accounts (in tree mode)"
     ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
     ,flagNone ["pretty-tables"] (\opts -> setboolopt "pretty-tables" opts) "use unicode to display prettier tables"
     ,flagNone ["sort-amount","S"] (\opts -> setboolopt "sort-amount" opts) "sort by amount instead of account code/name (in flat mode). With multiple columns, sorts by the row total, or by row average if that is displayed."
     ,flagNone ["budget"] (setboolopt "budget") "show performance compared to budget goals defined by periodic transactions"
     ,flagNone ["show-unbudgeted"] (setboolopt "show-unbudgeted") "with --budget, show unbudgeted accounts also"
     ,flagNone ["invert"] (setboolopt "invert") "display all amounts with reversed sign"
     ]
     ++ outputflags
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["b","bal"]

-- | The balance command, prints a balance report.
balance :: CliOpts -> Journal -> IO ()
balance opts@CliOpts{rawopts_=rawopts,reportopts_=ropts} j = do
  d <- getCurrentDay
  case lineFormatFromOpts ropts of
    Left err -> error' $ unlines [err]
    Right _ -> do
      let format   = outputFormatFromOpts opts
          interval = interval_ ropts
      case interval of
        NoInterval -> do
          -- single column balance report
          let report
                | balancetype_ ropts `elem` [HistoricalBalance, CumulativeChange]
                  = let ropts' | flat_ ropts = ropts
                               | otherwise   = ropts{accountlistmode_=ALTree}
                    in balanceReportFromMultiBalanceReport ropts' (queryFromOpts d ropts) j
                          -- for historical balances we must use balanceReportFromMultiBalanceReport (also forces --no-elide)
                | otherwise = balanceReport ropts (queryFromOpts d ropts) j -- simple Ledger-style balance report 
              render = case format of
                "csv"  -> \ropts r -> (++ "\n") $ printCSV $ balanceReportAsCsv ropts r
                "html" -> \_ _ -> error' "Sorry, HTML output is not yet implemented for this kind of report."  -- TODO
                _      -> balanceReportAsText
          writeOutput opts $ render ropts report
          
        _ | boolopt "budget" rawopts -> do
          -- multi column budget report
          reportspan <- reportSpan j ropts
          let budgetreport     = dbg1 "budgetreport"     $ budgetReport ropts assrt showunbudgeted reportspan d j
                where
                  showunbudgeted = boolopt "show-unbudgeted" rawopts
                  assrt          = not $ ignore_assertions_ $ inputopts_ opts
              render = case format of
                "csv"  -> const $ error' "Sorry, CSV output is not yet implemented for this kind of report."  -- TODO
                "html" -> const $ error' "Sorry, HTML output is not yet implemented for this kind of report."  -- TODO
                _      -> budgetReportAsText ropts
          writeOutput opts $ render budgetreport
          
          | otherwise -> do
          -- multi column balance report
          let report = multiBalanceReport ropts (queryFromOpts d ropts) j
              render = case format of
                "csv"  -> (++ "\n") . printCSV . multiBalanceReportAsCsv ropts
                "html" ->  (++ "\n") . TL.unpack . L.renderText . multiBalanceReportAsHtml ropts
                _      -> multiBalanceReportAsText ropts
          writeOutput opts $ render report

-- rendering single-column balance reports

-- | Find the best commodity to convert to when asked to show the
-- market value of this commodity on the given date. That is, the one
-- in which it has most recently been market-priced, ie the commodity
-- mentioned in the most recent applicable historical price directive
-- before this date.
-- defaultValuationCommodity :: Journal -> Day -> CommoditySymbol -> Maybe CommoditySymbol
-- defaultValuationCommodity j d c = mpamount <$> commodityValue j d c

-- | Render a single-column balance report as CSV.
balanceReportAsCsv :: ReportOpts -> BalanceReport -> CSV
balanceReportAsCsv opts (items, total) =
  ["account","balance"] :
  [[T.unpack (maybeAccountNameDrop opts a), showMixedAmountOneLineWithoutPrice b] | (a, _, _, b) <- items]
  ++
  if no_total_ opts
  then []
  else [["total", showMixedAmountOneLineWithoutPrice total]]

-- | Render a single-column balance report as plain text.
balanceReportAsText :: ReportOpts -> BalanceReport -> String
balanceReportAsText opts ((items, total)) = unlines $ concat lines ++ t
  where
      fmt = lineFormatFromOpts opts
      lines = case fmt of
                Right fmt -> map (balanceReportItemAsText opts fmt) items
                Left err  -> [[err]]
      t = if no_total_ opts
           then []
           else
             case fmt of
               Right fmt ->
                let
                  -- abuse renderBalanceReportItem to render the total with similar format
                  acctcolwidth = maximum' [T.length fullname | (fullname, _, _, _) <- items]
                  totallines = map rstrip $ renderBalanceReportItem opts fmt (T.replicate (acctcolwidth+1) " ", 0, total)
                  -- with a custom format, extend the line to the full report width;
                  -- otherwise show the usual 20-char line for compatibility
                  overlinewidth | isJust (format_ opts) = maximum' $ map length $ concat lines
                                | otherwise             = defaultTotalFieldWidth
                  overline   = replicate overlinewidth '-'
                in overline : totallines
               Left _ -> []

tests_balanceReportAsText = [
  "balanceReportAsText" ~: do
  -- "unicode in balance layout" ~: do
    j <- readJournal'
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    let opts = defreportopts
    balanceReportAsText opts (balanceReport opts (queryFromOpts (parsedate "2008/11/26") opts) j) `is`
      unlines
      ["                -100  актив:наличные"
      ,"                 100  расходы:покупки"
      ,"--------------------"
      ,"                   0"
      ]
 ]

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
balanceReportItemAsText :: ReportOpts -> StringFormat -> BalanceReportItem -> [String]
balanceReportItemAsText opts fmt (_, accountName, depth, amt) =
  renderBalanceReportItem opts fmt (
    maybeAccountNameDrop opts accountName,
    depth,
    normaliseMixedAmountSquashPricesForDisplay amt
    )

-- | Render a balance report item using the given StringFormat, generating one or more lines of text.
renderBalanceReportItem :: ReportOpts -> StringFormat -> (AccountName, Int, MixedAmount) -> [String]
renderBalanceReportItem opts fmt (acctname, depth, total) =
  lines $
  case fmt of
    OneLine comps       -> concatOneLine      $ render1 comps
    TopAligned comps    -> concatBottomPadded $ render comps
    BottomAligned comps -> concatTopPadded    $ render comps
  where
    render1 = map (renderComponent1 opts (acctname, depth, total))
    render  = map (renderComponent opts (acctname, depth, total))

defaultTotalFieldWidth = 20

-- | Render one StringFormat component for a balance report item.
renderComponent :: ReportOpts -> (AccountName, Int, MixedAmount) -> StringFormatComponent -> String
renderComponent _ _ (FormatLiteral s) = s
renderComponent opts (acctname, depth, total) (FormatField ljust min max field) = case field of
  DepthSpacerField -> formatString ljust Nothing max $ replicate d ' '
                      where d = case min of
                                 Just m  -> depth * m
                                 Nothing -> depth
  AccountField     -> formatString ljust min max (T.unpack acctname)
  TotalField       -> fitStringMulti min max True False $ showamt total
    where
      showamt | color_ opts = cshowMixedAmountWithoutPrice
              | otherwise   = showMixedAmountWithoutPrice
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
      showamt | color_ opts = cshowMixedAmountWithoutPrice
              | otherwise   = showMixedAmountWithoutPrice
  _                -> ""

-- rendering multi-column balance reports

-- | Render a multi-column balance report as CSV.
-- The CSV will always include the initial headings row,
-- and will include the final totals row unless --no-total is set.
multiBalanceReportAsCsv :: ReportOpts -> MultiBalanceReport -> CSV
multiBalanceReportAsCsv opts (MultiBalanceReport (colspans, items, (coltotals,tot,avg))) =
  ("Account" : map showDateSpan colspans
   ++ (if row_total_ opts then ["Total"] else [])
   ++ (if average_ opts then ["Average"] else [])
  ) :
  [T.unpack (maybeAccountNameDrop opts a) :
   map showMixedAmountOneLineWithoutPrice
   (amts
    ++ (if row_total_ opts then [rowtot] else [])
    ++ (if average_ opts then [rowavg] else []))
  | (a, _, _, amts, rowtot, rowavg) <- items]
  ++
  if no_total_ opts
  then []
  else [["Total:"]
        ++ map showMixedAmountOneLineWithoutPrice (
           coltotals
           ++ (if row_total_ opts then [tot] else [])
           ++ (if average_ opts then [avg] else [])
           )]

-- | Render a multi-column balance report as HTML.
multiBalanceReportAsHtml :: ReportOpts -> MultiBalanceReport -> Html ()
multiBalanceReportAsHtml ropts mbr =
  let
    (headingsrow,bodyrows,mtotalsrow) = multiBalanceReportHtmlRows ropts mbr
  in
    table_ $ mconcat $
         [headingsrow]
      ++ bodyrows
      ++ maybe [] (:[]) mtotalsrow

-- | Render the HTML table rows for a MultiBalanceReport.
-- Returns the heading row, 0 or more body rows, and the totals row if enabled.
multiBalanceReportHtmlRows :: ReportOpts -> MultiBalanceReport -> (Html (), [Html ()], Maybe (Html ()))
multiBalanceReportHtmlRows ropts mbr =
  let
    headingsrow:rest = multiBalanceReportAsCsv ropts mbr
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
multiBalanceReportAsText opts r =
    printf "%s in %s:\n\n" desc (showDateSpan $ multiBalanceReportSpan r)
      ++ balanceReportTableAsText opts tabl
  where
    tabl = balanceReportAsTable opts r
    desc = case balancetype_ opts of
        PeriodChange -> "Balance changes"
        CumulativeChange -> "Ending balances (cumulative)"
        HistoricalBalance -> "Ending balances (historical)"

-- | Build a 'Table' from a multi-column balance report.
balanceReportAsTable :: ReportOpts -> MultiBalanceReport -> Table String String MixedAmount
balanceReportAsTable opts (MultiBalanceReport (colspans, items, (coltotals,tot,avg))) =
   addtotalrow $ 
   Table
     (T.Group NoLine $ map Header accts)
     (T.Group NoLine $ map Header colheadings)
     (map rowvals items)
  where
    mkDate = case balancetype_ opts of
       PeriodChange -> showDateSpanMonthAbbrev
       _            -> maybe "" (showDate . prevday) . spanEnd
    colheadings = map mkDate colspans
                  ++ (if row_total_ opts then ["  Total"] else [])
                  ++ (if average_ opts then ["Average"] else [])
    accts = map renderacct items
    renderacct (a,a',i,_,_,_)
      | tree_ opts = replicate ((i-1)*2) ' ' ++ T.unpack a'
      | otherwise  = T.unpack $ maybeAccountNameDrop opts a
    rowvals (_,_,_,as,rowtot,rowavg) = as
                             ++ (if row_total_ opts then [rowtot] else [])
                             ++ (if average_ opts then [rowavg] else [])
    addtotalrow | no_total_ opts = id
                | otherwise      = (+----+ (row "" $
                                    coltotals
                                    ++ (if row_total_ opts && not (null coltotals) then [tot] else [])
                                    ++ (if average_ opts && not (null coltotals)   then [avg] else [])
                                    ))

-- | Given a table representing a multi-column balance report (for example,
-- made using 'balanceReportAsTable'), render it in a format suitable for
-- console output.
balanceReportTableAsText :: ReportOpts -> Table String String MixedAmount -> String
balanceReportTableAsText ropts = tableAsText ropts showamt
  where
    showamt | color_ ropts = cshowMixedAmountOneLineWithoutPrice
            | otherwise    =  showMixedAmountOneLineWithoutPrice


tests_Hledger_Cli_Commands_Balance = TestList
  tests_balanceReportAsText
