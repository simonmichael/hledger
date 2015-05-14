{-|

A ledger-compatible @balance@ command, with additional support for
multi-column reports.

Here is a description/specification for the balance command.  See also
"Hledger.Reports" -> \"Balance reports\".


/Basic balance report/

With no reporting interval (@--monthly@ etc.), hledger's balance
command emulates ledger's, showing accounts indented according to
hierarchy, along with their total amount posted (including subaccounts).

Here's an example. With @data/sample.journal@, which defines the following account tree:

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

module Hledger.Cli.Balance (
  balancemode
 ,balance
 ,balanceReportAsText
 ,periodBalanceReportAsText
 ,cumulativeBalanceReportAsText
 ,historicalBalanceReportAsText
 ,tests_Hledger_Cli_Balance
) where

import System.Console.CmdArgs.Explicit as C
import Text.CSV
import Test.HUnit
import Text.Printf (printf)
import Text.Tabular as T
import Text.Tabular.AsciiArt

import Hledger
import Hledger.Data.OutputFormat
import Hledger.Cli.Options
import Hledger.Cli.Utils


-- | Command line options for this command.
balancemode = (defCommandMode $ ["balance"] ++ aliases) { -- also accept but don't show the common bal alias
  modeHelp = "show accounts and balances" `withAliases` aliases
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show accounts as a tree (default in simple reports)"
     ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list (default in multicolumn mode)"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "tree mode: use this custom line format"
     ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "tree mode: don't squash boring parent accounts"
     ,flagNone ["historical","H"] (\opts -> setboolopt "historical" opts) "multicolumn mode: show historical ending balances"
     ,flagNone ["cumulative"] (\opts -> setboolopt "cumulative" opts) "multicolumn mode: show accumulated ending balances"
     ,flagNone ["average","A"] (\opts -> setboolopt "average" opts) "multicolumn mode: show a row average column"
     ,flagNone ["row-total","T"] (\opts -> setboolopt "row-total" opts) "multicolumn mode: show a row total column"
     ,flagNone ["no-total","N"] (\opts -> setboolopt "no-total" opts) "don't show the final total row"
     ]
     ++ outputflags
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["bal"]

-- | The balance command, prints a balance report.
balance :: CliOpts -> Journal -> IO ()
balance opts@CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  case lineFormatFromOpts ropts of
    Left err -> error' $ unlines [err]
    Right _ -> do
      let format   = outputFormatFromOpts opts
          interval = intervalFromOpts ropts
          baltype  = balancetype_ ropts
      case interval of
        NoInterval -> do
          let report = balanceReport ropts (queryFromOpts d ropts) j
              render = case format of
                "csv" -> \ropts r -> (++ "\n") $ printCSV $ balanceReportAsCsv ropts r
                _     -> balanceReportAsText
          writeOutput opts $ render ropts report
        _ -> do
          let report = multiBalanceReport ropts (queryFromOpts d ropts) j
              render = case format of
                "csv" -> \ropts r -> (++ "\n") $ printCSV $ multiBalanceReportAsCsv ropts r
                _     -> case baltype of
                  PeriodBalance     -> periodBalanceReportAsText
                  CumulativeBalance -> cumulativeBalanceReportAsText
                  HistoricalBalance -> historicalBalanceReportAsText
          writeOutput opts $ render ropts report

-- single-column balance reports

-- | Render a single-column balance report as CSV.
balanceReportAsCsv :: ReportOpts -> BalanceReport -> CSV
balanceReportAsCsv opts (items, total) =
  ["account","balance"] :
  [[a, showMixedAmountWithoutPrice b] | ((a, _, _), b) <- items]
  ++
  if no_total_ opts
  then []
  else [["total", showMixedAmountOneLineWithoutPrice total]]

-- | Render a single-column balance report as plain text.
balanceReportAsText :: ReportOpts -> BalanceReport -> String
balanceReportAsText opts ((items, total)) = unlines $ concat lines ++ t
  where
      lines = case lineFormatFromOpts opts of
                Right f -> map (balanceReportItemAsText opts f) items
                Left err -> [[err]]
      t = if no_total_ opts
           then []
           else ["--------------------"
                 -- TODO: This must use the format somehow
                ,padleft 20 $ showMixedAmountWithoutPrice total
                ]

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
This implementation turned out to be a bit convoluted but implements the following algorithm for formatting:

- If there is a single amount, print it with the account name directly:
- Otherwise, only print the account name on the last line.

    a         USD 1   ; Account 'a' has a single amount
              EUR -1
    b         USD -1  ; Account 'b' has two amounts. The account name is printed on the last line.
-}
-- | Render one balance report line item as plain text suitable for console output.
balanceReportItemAsText :: ReportOpts -> [OutputFormat] -> BalanceReportItem -> [String]
balanceReportItemAsText opts format ((_, accountName, depth), Mixed amounts) =
    -- 'amounts' could contain several quantities of the same commodity with different price.
    -- In order to combine them into single value (which is expected) we take the first price and
    -- use it for the whole mixed amount. This could be suboptimal. XXX
    let Mixed normAmounts = normaliseMixedAmountSquashPricesForDisplay (Mixed amounts) in
    case normAmounts of
      [] -> []
      [a] -> [formatBalanceReportItem opts (Just accountName) depth a format]
      (as) -> multiline as
    where
      multiline :: [Amount] -> [String]
      multiline []     = []
      multiline [a]    = [formatBalanceReportItem opts (Just accountName) depth a format]
      multiline (a:as) = (formatBalanceReportItem opts Nothing depth a format) : multiline as

formatBalanceReportItem :: ReportOpts -> Maybe AccountName -> Int -> Amount -> [OutputFormat] -> String
formatBalanceReportItem _ _ _ _ [] = ""
formatBalanceReportItem opts accountName depth amount (fmt:fmts) =
  s ++ (formatBalanceReportItem opts accountName depth amount fmts)
  where
    s = case fmt of
         FormatLiteral l -> l
         FormatField ljust min max field  -> formatField opts accountName depth amount ljust min max field

formatField :: ReportOpts -> Maybe AccountName -> Int -> Amount -> Bool -> Maybe Int -> Maybe Int -> HledgerFormatField -> String
formatField opts accountName depth total ljust min max field = case field of
        AccountField     -> formatValue ljust min max $ maybe "" (maybeAccountNameDrop opts) accountName
        DepthSpacerField -> case min of
                               Just m  -> formatValue ljust Nothing max $ replicate (depth * m) ' '
                               Nothing -> formatValue ljust Nothing max $ replicate depth ' '
        TotalField       -> formatValue ljust min max $ showAmountWithoutPrice total
        _                  -> ""

-- multi-column balance reports

-- | Render a multi-column balance report as CSV.
multiBalanceReportAsCsv :: ReportOpts -> MultiBalanceReport -> CSV
multiBalanceReportAsCsv opts (MultiBalanceReport (colspans, items, (coltotals,tot,avg))) =
  ("account" : "short account" : "indent" : map showDateSpan colspans
   ++ (if row_total_ opts then ["total"] else [])
   ++ (if average_ opts then ["average"] else [])
  ) :
  [a : a' : show i :
   map showMixedAmountOneLineWithoutPrice
   (amts
    ++ (if row_total_ opts then [rowtot] else [])
    ++ (if average_ opts then [rowavg] else []))
  | ((a,a',i), amts, rowtot, rowavg) <- items]
  ++
  if no_total_ opts
  then []
  else [["totals", "", ""]
        ++ map showMixedAmountOneLineWithoutPrice (
           coltotals
           ++ (if row_total_ opts then [tot] else [])
           ++ (if average_ opts then [avg] else [])
           )]

-- | Render a multi-column period balance report as plain text suitable for console output.
periodBalanceReportAsText :: ReportOpts -> MultiBalanceReport -> String
periodBalanceReportAsText opts r@(MultiBalanceReport (colspans, items, (coltotals,tot,avg))) =
  unlines $
  ([printf "Balance changes in %s:" (showDateSpan $ multiBalanceReportSpan r)] ++) $
  trimborder $ lines $
   render id (" "++) showMixedAmountOneLineWithoutPrice $
    addtotalrow $
     Table
     (T.Group NoLine $ map (Header . padright acctswidth) accts)
     (T.Group NoLine $ map Header colheadings)
     (map rowvals items')
  where
    trimborder = ("":) . (++[""]) . drop 1 . init . map (drop 1 . init)
    colheadings = map showDateSpan colspans
                  ++ (if row_total_ opts then ["  Total"] else [])
                  ++ (if average_ opts then ["Average"] else [])
    items' | empty_ opts = items
           | otherwise   = items -- dbg1 "2" $ filter (any (not . isZeroMixedAmount) . snd) $ dbg1 "1" items
    accts = map renderacct items'
    renderacct ((a,a',i),_,_,_)
      | tree_ opts = replicate ((i-1)*2) ' ' ++ a'
      | otherwise  = maybeAccountNameDrop opts a
    acctswidth = maximum $ map length $ accts
    rowvals (_,as,rowtot,rowavg) = as
                                   ++ (if row_total_ opts then [rowtot] else [])
                                   ++ (if average_ opts then [rowavg] else [])
    addtotalrow | no_total_ opts = id
                | otherwise      = (+----+ (row "" $
                                    coltotals
                                    ++ (if row_total_ opts then [tot] else [])
                                    ++ (if average_ opts then [avg] else [])
                                    ))

-- | Render a multi-column cumulative balance report as plain text suitable for console output.
cumulativeBalanceReportAsText :: ReportOpts -> MultiBalanceReport -> String
cumulativeBalanceReportAsText opts r@(MultiBalanceReport (colspans, items, (coltotals,tot,avg))) =
  unlines $
  ([printf "Ending balances (cumulative) in %s:" (showDateSpan $ multiBalanceReportSpan r)] ++) $
  trimborder $ lines $
   render id (" "++) showMixedAmountOneLineWithoutPrice $
    addtotalrow $
     Table
       (T.Group NoLine $ map (Header . padright acctswidth) accts)
       (T.Group NoLine $ map Header colheadings)
       (map rowvals items)
  where
    trimborder = ("":) . (++[""]) . drop 1 . init . map (drop 1 . init)
    colheadings = map (maybe "" (showDate . prevday) . spanEnd) colspans
                  ++ (if row_total_ opts then ["  Total"] else [])
                  ++ (if average_ opts then ["Average"] else [])
    accts = map renderacct items
    renderacct ((a,a',i),_,_,_)
      | tree_ opts = replicate ((i-1)*2) ' ' ++ a'
      | otherwise  = maybeAccountNameDrop opts a
    acctswidth = maximum $ map length $ accts
    rowvals (_,as,rowtot,rowavg) = as
                                   ++ (if row_total_ opts then [rowtot] else [])
                                   ++ (if average_ opts then [rowavg] else [])
    addtotalrow | no_total_ opts = id
                | otherwise      = (+----+ (row "" $
                                    coltotals
                                    ++ (if row_total_ opts then [tot] else [])
                                    ++ (if average_ opts then [avg] else [])
                                    ))

-- | Render a multi-column historical balance report as plain text suitable for console output.
historicalBalanceReportAsText :: ReportOpts -> MultiBalanceReport -> String
historicalBalanceReportAsText opts r@(MultiBalanceReport (colspans, items, (coltotals,tot,avg))) =
  unlines $
  ([printf "Ending balances (historical) in %s:" (showDateSpan $ multiBalanceReportSpan r)] ++) $
  trimborder $ lines $
   render id (" "++) showMixedAmountOneLineWithoutPrice $
    addtotalrow $
     Table
       (T.Group NoLine $ map (Header . padright acctswidth) accts)
       (T.Group NoLine $ map Header colheadings)
       (map rowvals items)
  where
    trimborder = ("":) . (++[""]) . drop 1 . init . map (drop 1 . init)
    colheadings = map (maybe "" (showDate . prevday) . spanEnd) colspans
                  ++ (if row_total_ opts then ["  Total"] else [])
                  ++ (if average_ opts then ["Average"] else [])
    accts = map renderacct items
    renderacct ((a,a',i),_,_,_)
      | tree_ opts = replicate ((i-1)*2) ' ' ++ a'
      | otherwise  = maybeAccountNameDrop opts a
    acctswidth = maximum $ map length $ accts
    rowvals (_,as,rowtot,rowavg) = as
                             ++ (if row_total_ opts then [rowtot] else [])
                             ++ (if average_ opts then [rowavg] else [])
    addtotalrow | no_total_ opts = id
                | otherwise      = (+----+ (row "" $
                                    coltotals
                                    ++ (if row_total_ opts then [tot] else [])
                                    ++ (if average_ opts then [avg] else [])
                                    ))

-- | Figure out the overall date span of a multicolumn balance report.
multiBalanceReportSpan :: MultiBalanceReport -> DateSpan
multiBalanceReportSpan (MultiBalanceReport ([], _, _))       = DateSpan Nothing Nothing
multiBalanceReportSpan (MultiBalanceReport (colspans, _, _)) = DateSpan (spanStart $ head colspans) (spanEnd $ last colspans)


tests_Hledger_Cli_Balance = TestList
  tests_balanceReportAsText
