Internal/api/developer-ish changes in the hledger-lib (and hledger) packages.
For user-visible changes, see the hledger package changelog.

# ec4d131d2
- lib: Default to PeriodChange report when using ValueChangeReport. (Stephen Morgan)

- lib,cli: Restore old --change option for per-period summation, use --sum for basic ValueChange balance report. (Stephen Morgan)

- lib: transactionFile

- lib: Do not include price directives in journalDateSpan. Only include price directives after the last transaction/posting date if using --value=end. (Stephen Morgan)
  Also enlarges the reportspan to encompass full intervals for budget
  goals.

- lib,cli: Add --valuechange report type for calculating change of value of accounts, restore --value=end behaviour to that of hledger-1.19. (Stephen Morgan)

- lib,cli: Add --periodic option to indicate PeriodChange accumulation (renamed from --change). (Stephen Morgan)

- lib: (amount|mixedAmount)(Looks|Is)Zero functions now check whether both the quantity and the cost are zero. This is usually what you want, but if you do only want to check whether the quantity is zero, you can run mixedAmountStripPrices (or similar) before this. (Stephen Morgan)
  (multiply|divide)(Mixed)?Amount now also multiply or divide the
  TotalPrice if it is present, and the old
  (multiply|divide)(Mixed)?AmountAndPrice functions are removed.

- lib: normaliseHelper now uses a strict Map for combining amounts internally, closing a big space leak. (Stephen Morgan)
  This also now combines Amounts with TotalPrices in the same commodity
  when normalising; amounts with TotalPrices were previously never
  combined.

- lib: Distinguish between an Amount having quantity (or rounded quantity 0), and having both quantity and totalprice 0 (or rounded to 0). (Stephen Morgan)

- lib: Make fields of Amount, AmountPrice, AmountStyle, and DigitGroupStyle strict. (Stephen Morgan)

- lib: Include sign in TotalPrice in Amount, rather than relying on the sign of aquantity. (Stephen Morgan)
  Journal entries still require a positive @@ price, but now the sign is
  set after parsing, rather than when converting in amountToCost.

  The reason for this change is that, if we're going to perform arithmetic
  on Amount with TotalCost, then the presence of aquantity=0 means that
  amountToCost would render the total cost as 0, because signum 0 == 0.
  This makes journal entries like the following impossible to balance:
  2000-01-01
      a  0 @@ 10 A
      b  -10 A

- lib: Consume list immediately in commodityStylesFromAmounts. (Stephen Morgan)
  This reduced the maximum heap size per thread from ~850K to ~430K in a
  real-world register test.

- add, print --match: prioritise infix matches
  If the search description occurs in full within the other description,
  that match gets a +0.5 score boost.

- add, lib: debug output, refactor similar transactions lookup
  add --debug=1 shows the top hits for similar past transactions.

  added:
  Hledger.Cli.Utils.journalSimilarTransaction
   provides --debug=1 output

  changed:
  Hledger.Cli.Commands.Add.transactionsSimilarTo -> Hledger.Data.Journal.journalTransactionsSimilarTo
   now takes an extra number-of-results argument

- Make sure to round up for the 'to' date (Arnout Engelen)

- lib: avoid shady Not (Or []) in account type queries

- bs: don't let just a Cash declaration hide Asset accounts
  Clarify the account type queries a bit, and don't let a
  declaration of account as Cash cause the fallback regex
  for Asset accounts to be ignored.

- bs: just declaring a Cash account no longer hides Asset accounts
  Since Cash is also an Asset, declaring an account as Cash also disabled the fallback regexp for Asset accounts.

- lib: refactor: more consistent amount precision helpers
  Hledger.Data.Amount:
  renamed:
  setAmountPrecision -> amountSetPrecision
  setFullPrecision -> amountSetFullPrecision
  setMixedAmountPrecision -> mixedAmountSetPrecision
  added:
  mixedAmountSetFullPrecision

- cli: rename --infer-value to --infer-market-price
  For clarity; infer-value was too vague. The old spelling remains
  supported for compatibility, but is now deprecated.
  When typing, --infer-market or even --infer (for now) is sufficient.

- ui: transaction: show all decimal places (like print, cf #931)
  On the accounts screen and register screen we round amounts according
  to commodity styles, but when you drill down to a transaction you
  probably want to see the unrounded amounts.

- lib: refactor: rename journal/transaction mapping helpers
  Hledger.Data.Journal:
  changed:
  mapJournalTransactions -> journalMapTransactions
  mapJournalPostings -> journalMapPostings
  removed:
  mapTransactionPostings

  Hledger.Data.Transaction:
  added:
  transactionMapPostings

- print: always show all decimal places  (#931)
  Ensures parseable and more sensible-looking output in more cases, and behaves more like Ledger's print.

  There is still an issue with adding trailing zeroes, which would be nice to prevent.

- print: don't add decimal places in assertion/assignment amounts either (#1465)

- print: show all decimal places in assertion/assignment amounts (#1465)
  Overriding the commodity style, per https://hledger.org/hledger.html#commodity-display-style.

- doc: Remove references to --value=cost, replacing them with --cost. (Stephen Morgan)
  Since this is option is now just an alias for -B/--cost, and since it
  may be removed soon, we make it undocumented, though it will still
  behave as before. --value=cost,COMM is unsupported as well.

- lib,cli,ui: Separate costing from valuation; each can now be specified independently. (Stephen Morgan)
  You can now combine costing and valuation, for example "--cost
  --value=then" will first convert to costs, and then value according to
  the "--value=then" strategy. Any valuation strategy can be used with or
  without costing.

  If multiple valuation and costing strategies are specified on the
  command line, then if any of them include costing
  (-B/--cost/--value=cost) then amounts will be converted to cost, and for
  valuation strategy the rightmost will be used.

  --value=cost is deprecated, but still supported and is equivalent to
  --cost/-B. --value=cost,COMM is no longer supported, but this behaviour can be
  achieved with "--cost --value=then,COMM".

- lib,cli,ui: Introduce *ApplyCostValuation functions, which perform both costing and valuation. (Stephen Morgan)
  This currently is given a dummy NoCost argument and is equivalent to
  "maybe id (*ApplyValuation ...)", but provides a constant interface so
  that internal behaviour can be changed freely.

- lib: Use mapMixedAmount more, decreasing reliance on internal representation of Mixed. (Stephen Morgan)

- lib: bal --value=end without --historical should not report on fluctuation in value of historical postings. (Stephen Morgan)

- lib: matchesDescription, matchesPayeeWIP

- lib: journalPayeesUsed, journalPayeesDeclaredOrUsed

- lib: Journal: avoid a build failure if monad-extras is installed

- lib: Expose textConcatBottomPadded. (Stephen Morgan)

- lib: Rename alignCell to textCell, minor cleanups. (Stephen Morgan)

- lib: Update textConcatTopPadded to use Tabular.AsciiWide, same as concatTopPadded. (Stephen Morgan)

- lib: Refactor unlinesB to be cleaner and more efficient. (Stephen Morgan)

- journal: allow commodity directive to set style of no-symbol commodity (#1461)
  (cherry picked from commit c5571f6468ab11ffe3cd3f86a86f0b3253be10d1)

- lib: Calculate value at posting date for register --value=then -M. (Stephen Morgan)

- lib: Make sure to add a newline to the end of aregister report. (Stephen Morgan)

- csv: handle more sign variations, eg a sign by itself
  simplifySign now covers a few more sign combinations that might arise.
  And in particular, it strips a standalone sign with no number,
  which simplifies sign flipping with amount-in/amount-out.

- lib: add level argument to fix traceAtWith

- lib: Make sure AtEnd valuation behaviour is consistent between single- and multi-period reports (#1424). (Stephen Morgan)

- lib,cli: Extend AtThen valuation to all report types. (Stephen Morgan)
  Also adds a postingDate argument to amountApplyValuation, and re-orders
  the ValuationType and (Transaction/Posting) arguments to
  (transaction/posting)ApplyValuation, to be consistent with
  amountApplyValuation.

- new price search that really finds the shortest path (#1443)
  This one should also reliably prevent runaway searches in the event of more bugs, giving up after 1000 iterations.

- lib: valuation: simplify price search code
  This version tries counting recursions instead of path length, but I
  think they are the same.

- lib: valuation: don't hang when finding prices (fixes #1439)
  Searching for prices during valuation no longer now properly excludes
  price loops, avoiding near infinite looping with certain
  configurations of market prices. Also we now always use a direct price
  when available, rather than searching unnecessarily.

  Price searching progress info, useful for troubleshooting, is now
  displayed with --debug=2.

  There could still be some corner cases we don't handle correctly. We
  now give up with an error message if the searched price chains get too
  long (> 1000). More importantly, we should also give up if the search
  iterates too many times, but this is not done yet.

- lib: Make consistent naming scheme for showMixedAmount* functions, add conversion between old API and new API in the documentation. (Stephen Morgan)

- doc: Fix some outdated documentation. (Stephen Morgan)

- lib,cli: Assorted fixes for older GHC. (Stephen Morgan)

- lib,cli,ui: Replace some uses of String with Text, get rid of some unpacks, clean up showMixed options. (Stephen Morgan)

- lib: Use Text and Text builder only in postingAsLines. (Stephen Morgan)

- lib,cli,ui: Use WideBuilder for Tabular.AsciiWide. (Stephen Morgan)
  Move WideBuilder to Text.WideString.

- lib,cli,ui: Implement all showMixed* functions in terms of DisplayAmountOpts and WideBuilder. (Stephen Morgan)

- lib: Use AmountDisplayOpts for showAmount*, reducing need for many different named functions. (Stephen Morgan)

- lib: Implement showAmountHelper using AmountBuilder. (Stephen Morgan)

- lib: showamountquantity shows directly, rather than parsing string output of show instance for Quantity. (Stephen Morgan)

- lib: Remove unused optional width argument for StringFormat. (Stephen Morgan)

- lib,cli: Use Text Builder for Balance commands. (Stephen Morgan)

- lib,cli: Use Text for CompoundPeriodicReport titles. (Stephen Morgan)

- lib,cli: Use Text for CSV values. (Stephen Morgan)

- lib,cli,ui: Use Text for showDate and related. (Stephen Morgan)

- lib,cli: Make showTransaction return Text rather than String. (Stephen Morgan)

- lib: Remove unused label on TranspactionReport and AccountTransactionsReport. (Stephen Morgan)

- lib,cli: Use Text Builder for Account Transaction Reports. (Stephen Morgan)

- lib: Add wrap convenience function. (Stephen Morgan)

- cli: Using Text Builder for posting reports. (Stephen Morgan)

- journal: Ignore AUTO commodity when strict checking (aragaer)
  AUTO commodity is a placeholder for postings with missing amounts. It
  should be ignored when doing a strict commodity check.

  Fixes #1419

- check: also check "accounts"/"commodities" on demand
  (cherry picked from commit 0c2bf54f2955e3a25fd0282acc42608f957abaea)

- check: add "payees" check requiring payee declarations

- bin: Update bin scripts for current hledger-lib. (Stephen Morgan)
  (cherry picked from commit bc4aef17b7fa13ec0754b93325e1c5e5ee04f1e7)

- Removed: 
  valuationTypeIsCost
  valuationTypeIsDefaultValue
  ValuationType's AtDefault constructor
  
- Changed:
  *ApplyValuation functions take two less arguments

- journal: allow commodity directive to set style of no-symbol commodity (#1461)
  (cherry picked from commit c5571f6468ab11ffe3cd3f86a86f0b3253be10d1)
  (cherry picked from commit 034c317496e99271ebabc53af112ad88e054b7ab)

- lib: Calculate value at posting date for register --value=then -M. (Stephen Morgan)

- bin: Update bin scripts for current hledger-lib. (Stephen Morgan)
  (cherry picked from commit bc4aef17b7fa13ec0754b93325e1c5e5ee04f1e7)
  (cherry picked from commit a64d1aa6d0bcaf643bbe2607238026b4d26a3637)

- lib,cli: Revert --value=end PeriodChange behaviour to hledger-1.19, i.e. calculating the value of the change, rather than the change of the value. (Stephen Morgan)

- new price search that really finds the shortest path (#1443)
  This one should also reliably prevent runaway searches in the event of more bugs, giving up after 1000 iterations.

  (cherry picked from commit 3d7d5c0db7509299acf3d33530728f834345959a)

- lib: valuation: simplify price search code
  This version tries counting recursions instead of path length, but I
  think they are the same.

  (cherry picked from commit 7c9303a15c64859f11aec8fa75546793827e3086)

- lib: valuation: don't hang when finding prices (fixes #1439)
  Searching for prices during valuation no longer now properly excludes
  price loops, avoiding near infinite looping with certain
  configurations of market prices. Also we now always use a direct price
  when available, rather than searching unnecessarily.

  Price searching progress info, useful for troubleshooting, is now
  displayed with --debug=2.

  There could still be some corner cases we don't handle correctly. We
  now give up with an error message if the searched price chains get too
  long (> 1000). More importantly, we should also give up if the search
  iterates too many times, but this is not done yet.

  (cherry picked from commit 73678393b1ec9ea414d798ade9da6e5666c079c2)

- journal: Ignore AUTO commodity when strict checking (aragaer)
  AUTO commodity is a placeholder for postings with missing amounts. It
  should be ignored when doing a strict commodity check.

  Fixes #1419

  (cherry picked from commit 2084b845e09d2249a5d0e120805798730eeb4b6d)

# 1.20.4 2021-01-29

- See hledger.

# 1.20.3 2021-01-14

- See hledger.

# 1.20.2 2020-12-28

- Fix the info manuals' node structure.

- Drop unused parsec dependency.

# 1.20.1 2020-12-15

- renamed: updateReportSpecFromOpts -> updateReportSpec[With]

# 1.20 2020-12-05

- added: journalApplyAliases, transactionApplyAliases, postingApplyAliases

- a new more robust price lookup implementation, fgl library dropped (#1402)

- Reverted a stripAnsi change in 1.19.1 that caused a 3x slowdown of amount rendering 
  in terminal reports. (#1350)

- Amount and table rendering has been improved, so that stripAnsi is no longer needed.
  This speeds up amount rendering in the terminal, speeding up some reports by 10% or more since 1.19.
  (Stephen Morgan)

- global commodity display styles can now be set in InputOpts or Journal,
  overriding all others (declared or inferred). This is used by the import
  command and probably command-line options in future.

- Journal keeps a new piece of parsing state, a decimal mark character,
  which can optionally be set to force the number format expected by all
  amount parsers.

- Remove Empty Query constructor, which does nothing and has done so for a very long time. (Stephen Morgan)

- In ReportOpts, store query terms term-by-term in a list in querystring_. (Stephen Morgan)
  This helps deal with tricky quoting issues, as we no longer have to make
  sure everything is quoted properly before merging it into a string.

- Implement concat(Top|Bottom)Padded in terms of renderRow, allowing them to be width aware. (Stephen Morgan)

- Expand Tabular.AsciiWide to allow multiline, custom-width,
  vertically/horizontally-aligned cells, and optional table borders.
  (Stephen Morgan)

- Introduce showMixed*Unnormalised, eliminate most direct calls of strWidth. (Stephen Morgan)

- showMixedAmountElided now makes better use of space, showing as many
  Amounts possible as long as they and the elision string fit within
  32 characters. (Stephen Morgan)

- Add Functor instance for CompoundPeriodicReport. (Stephen Morgan)

- Generalise CBCSubreportSpec to allow more subreport control. (Stephen Morgan)

- Export some MultiBalanceReport helper functions. (Stephen Morgan)

- Make Default instances clearer, remove Default instance for Bool. (Stephen Morgan)

- Many ReportOpts-related changes, such as the addition of ReportSpec, aimed
  at preventing runtime errors (from parsing: regexps, dates, format strings;
  from not having today's date set; etc.)
  ReportSpec holds a ReportOpts, the day of the report, and the Query generated from these.

- StringFormat now takes an optional overline width, which is
  currently only used by defaultBalanceLineFormat. (Stephen Morgan)

- quoteIfNeeded should not escape the backslashes in unicode code points. (Stephen Morgan)

- Export OrdPlus and constructors. (Stephen Morgan)

- Debug output now uses pretty-simple instead pretty-show.
  This hopefully gives overall nicer debug output (eg in colour), 
  including for values which don't have Read-able Show output.
  This means that we can start removing custom Show instances 
  that were a workaround for pretty-show. Eg account names
  in debug output no longer show their colons as underscores.

  Here's some old pretty-show output:

   CsvRules
     { rdirectives = [ ( "skip" , "1" ) ]
     , rcsvfieldindexes = [ ( "date" , 1 ) , ( "amount" , 2 ) ]
     , rassignments = [ ( "amount" , "%2" ) , ( "date" , "%1" ) ]
     , rconditionalblocks = []
     }

  And the new pretty-simple output:

   CsvRules
     { rdirectives=
       [ ( "skip", "1" ) ]
     , rcsvfieldindexes=
       [ ( "date", 1 ), ( "amount", 2 ) ]
     , rassignments=
       [ ( "amount", "%2" ), ( "date", "%1" ) ]
     , rconditionalblocks= []
     }

  We require pretty-simple 4.0.0.0 to get this compact output.
  It's a little less compact than pretty-show, but not too bad.
  Non-compact pretty-simple output would be:

   CsvRules
       { rdirectives=
           [
               ( "skip"
               , "1B"
               )
           ]
       , rcsvfieldindexes=
           [
               ( "date"
               , 1
               )
           ,
               ( "amount"
               , 2
               )
           ]
       , rassignments=
           [
               ( "amount"
               , "%2"
               )
           ,
               ( "date"
               , "%1"
               )
           ]
       , rconditionalblocks=[]
       }


# 1.19.1 2020-09-07

- Allow megaparsec 9

- stripAnsi: correctly strip ansi sequences with no
  numbers/semicolons. (Stephen Morgan)

- Added case-insensitive accountNameToAccountRegexCI,
  accountNameToAccountOnlyRegexCI, made the default account type
  queries case insensitive again. (#1341)

# 1.19 2020-09-01

- Added a missing lower bound for aeson, making cabal installs more
  reliable. (#1268)

- The Regex type alias has been replaced by the Regexp ADT, which
  contains both the compiled regular expression (so is guaranteed to
  be usable at runtime) and the original string (so can be serialised,
  printed, compared, etc.) A Regexp also knows whether is it case
  sensitive or case insensitive. The Hledger.Utils.Regex API has
  changed. (#1312, #1330).

- Typeable and Data instances are no longer derived for hledger's
  data types; they were redundant/no longer needed.

- NFData instances are no longer derived for hledger's data types.
  This speeds up a full build by roughly 7%. But it means we can't
  deep-evaluate hledger values, or time hledger code with Criterion.
  https://github.com/simonmichael/hledger/pull/1330#issuecomment-684075129
  has some ideas on this.

- Query no longer has a custom Show instance

- Hledger.Utils.String: quoteIfNeeded now actually escapes quotes in
  strings. escapeQuotes was dropped. (Stephen Morgan)

- Hledger.Utils.Tree: dropped some old utilities

- Some fromIntegral calls have been replaced with safer code, removing
  some potential for integer wrapping bugs (#1325, #1326)

- Parsing numbers with more than 255 decimal places now gives an error
  instead of silently misparsing (#1326)

- Digit groups are now limited to at most 255 digits each. (#1326)

- Exponents are parsed as Integer rather than Int.
  This means exponents greater than 9223372036854775807 or less than
  -9223372036854775808 are now parsed correctly, in theory. (In
  practice, very large exponents will cause hledger to eat all your
  memory, so avoid them for now.) (#1326)

- AmountStyle's asprecision is now a sum type with Word8, instead of
  an Int with magic values.

- DigitGroupStyle uses Word8 instead of Int.

- Partial helper function parsedate has been dropped, use fromGregorian instead.

- Partial helper function mkdatespan has been dropped.

- Helper function transaction now takes a Day instead of a date string. (Stephen Morgan)

- Old CPP directives made redundant by version bounds have been
  removed. (Stephen Morgan)

- Smart dates are now represented by the SmartDate type, and are
  always well formed. (Stephen Morgan)

- accountTransactionsReport (used for hledger aregister and
  hledger-ui/hledger-web registers) now filters transactions more
  thoroughly, so eg transactions dated outside the report period will
  not be shown. Previously the transaction would be shown if it had
  any posting dated inside the report period. Possibly some other
  filter criteria now get applied that didn't before. I think on
  balance this will give slightly preferable results.

- The old BalanceReport code has been dropped at last, replaced by
  MultiBalanceReport so that all balance reports now use the same
  code. (Stephen Morgan, #1256).

  - The large multiBalanceReport function has been split up and refactored
    extensively.
  - Tabular data formerly represented as [[MixedAmount]] is now HashMap
    AccountName (Map DateSpan Account). Reports with many columns are now faster.
  - Calculating starting balances no longer calls the whole balanceReport,
    just the first few functions.
  - displayedAccounts is completely rewritten. Perhaps one subtle thing to
    note is that in tree mode it no longer excludes nodes with zero inclusive
    balance unless they also have zero exclusive balance.
  - Simon's note: "I'll mark the passing of the old multiBalanceReport, into
    which I poured many an hour :). It is in a way the heart (brain ?) of
    hledger - the key feature of ledgerlikes (balance report) and a key
    improvement introduced by hledger (tabular multiperiod balance reports)
    ...
    Thanks @Xitian9, great work."

# 1.18.1 2020-06-21

- fix some doc typos (Martin Michlmayr)

# 1.18 2020-06-07

- added: getHledgerCliOpts', takes an explicit argument list

- added: toJsonText

- changed: isNegativeMixedAmount now gives an answer for
  multi-commodity amounts which are all negative

- changed: multiBalanceReport now gets the query from ReportOpts (Dmitry Astapov)

- renamed:
  isZeroAmount                -> amountLooksZero
  isReallyZeroAmount          -> amountIsZero
  isZeroMixedAmount           -> mixedAmountLooksZero
  isReallyZeroMixedAmount     -> mixedAmountIsZero
  isReallyZeroMixedAmountCost dropped

- renamed: finaliseJournal -> journalFinalise

- renamed: fixedlotpricep -> lotpricep, now also parses non-fixed lot prices

- dropped: transactionPostingBalances

- dropped: outputflags no longer exported by Hledger.Cli.CliOptions

- fixed: documentation for journalExpenseAccountQuery (Pavan Rikhi)

# 1.17.1 2020-03-19

- require newer Decimal, math-functions libs to ensure consistent
  rounding behaviour, even when built with old GHCs/snapshots. 
  hledger uses banker's rounding (rounds to nearest even number, eg
  0.5 displayed with zero decimal places is "0").

- added: debug helpers traceAt, traceAtWith

- Journal is now a Semigroup, not a Monoid (since <> is right-biased). (Stephen Morgan)

# 1.17.0.1 2020-03-01

- fix org heading comments and doctest setup comment that were
  breaking haddock (and in some cases, installation)

# 1.17 2020-03-01

- Reader-finding utilities have moved from Hledger.Read to
  Hledger.Read.JournalReader so the include directive can use them.

- Reader changes:
  - rExperimental flag removed
  - old rParser renamed to rReadFn
  - new rParser field provides the actual parser.
    This seems to require making Reader a higher-kinded type, unfortunately.

- Hledger.Tabular.AsciiWide now renders smoother outer borders in
  pretty (unicode) mode.
  Also, a fix for table edges always using single-width intersections
  and support for double horizontal lines with single vertical lines. (Eric Mertens)

- Hledger.Utils.Parse: restofline can go to eof also

- Hledger.Read cleanup

- Hledger.Read.CsvReader cleanup
  Exports added: CsvRecord, CsvValue, csvFileFor.
  Exports removed: expandIncludes, parseAndValidateCsvRules, transactionFromCsvRecord

- more cleanup of amount canonicalisation helpers (#1187)
  Stop exporting journalAmounts, overJournalAmounts, traverseJournalAmounts.
  Rename journalAmounts helper to journalStyleInfluencingAmounts.

- export mapMixedAmount

- Don't store leaf name in PeriodReport. (Stephen Morgan)
  Calculate at the point of consumption instead.

- Generalise PeriodicReport to be polymorphic in the account labels. (Stephen Morgan)

- Use records instead of tuples in PeriodicReport. (Stephen Morgan)

- Use PeriodicReport in place of MultiBalanceReport. (Stephen Morgan)

- Calculate MultiReportBalance columns more efficiently. (Stephen Morgan)
  Only calculate posting date once for each posting, and calculate their
  columns instead of checking each DateSpan separately.

- Moved JSON instances from hledger-web to hledger-lib (Hledger.Data.Json),
  and added ToJSON instances for all (?) remaining data types, up to Ledger.

- Dropped nullassertion's "assertion" alias, fixing a warning.
  Perhaps we'll stick with the null* naming convention. 


# 1.16.2 2020-01-14

- add support for megaparsec 8 (#1175)

# 1.16.1 2019-12-03

- Drop unnecessary mtl-compat dependency

- Fix building with GHC 8.0, 8.2

# 1.16 2019-12-01

- drop support for GHC 7.10, due to MonadFail hassles in JournalReader.hs

- add support for GHC 8.8, base-compat 0.11 (#1090)

  We are now using the new fail from the MonadFail class, which we
  always import qualified as Fail.fail, from base-compat-batteries
  Control.Monad.Fail.Compat to work with old GHC versions. If old fail
  is needed (shouldn't be) it should be imported qualified as
  Prelude.Fail, using imports such as:

      import Prelude hiding (fail)
      import qualified Prelude (fail)
      import Control.Monad.State.Strict hiding (fail)
      import "base-compat-batteries" Prelude.Compat hiding (fail)
      import qualified "base-compat-batteries" Control.Monad.Fail.Compat as Fail

- hledger and hledger-lib unit tests have been ported to tasty.

- The doctest suite has been disabled for now since it doesn't run
  well with cabal (#1139)
  
# 1.15.2 2019-09-05

Changes:

- postingApplyValuation, mixedAmountApplyValuation, amountApplyValuation
  take an argument, the report end date if one was specified.

# 1.15.1 2019-09-02

- fix failing doctests

# 1.15 2019-09-01

Removals include:

- journalPrices
- BalanceHistoryReport
- postingValueAtDate

Additions include:

- MarketPrice (more pure form of PriceDirective without the amount style information)
- PriceOracle (efficient lookup of exchange rates)
- ValuationType (ways to convert amount value)
- aliasnamep (export)
- setNaturalPrecisionUpTo
- dbgNWith, ptraceAtWith
- postingTransformAmount, postingToCost, postingValue
- amountToCost, mixedAmountToCost
- valueTypeFromOpts
- mapJournalTransactions, mapJournalPostings, mapTransactionPostings
- journalStartDate, journalEndDate
- journalPriceOracle
- marketPriceReverse
- priceDirectiveToMarketPrice
- mixedAmountApplyValuation
- mixedAmountValueAtDate

Changes include:

- Price -> AmountPrice,  AKA "transaction price"
- old MarketPrice -> PriceDirective
- TransactionsReport/AccountTransactionsReport split into separate files
- journalTransactionsReport -> transactionsReport
- accountTransactionsReportItems: rewrite using catMaybes and mapAccumL (Henning Thielemann)
- optionally save the current date in ReportOpts
- Hledger.Cli tests now have correct prefix; add Cli.Utils tests
- MultiBalanceReport now returns zero for row totals when in cumulative or historical mode (#329)


# 1.14.1 2019-03-20

- require easytest <0.3 to fix build issue

# 1.14 2019-03-01

- added:  
  transaction, [v]post*, balassert* constructors, for tests etc.  

- renamed:  
  porigin -> poriginal  

- refactored:  
  transaction balancing & balance assertion checking (#438)

# 1.13.1 (2019/02/02)

- stop depending on here to avoid haskell-src-meta/stackage blockage.

# 1.13 (2019/02/01)

- in Journal's jtxns field, forecasted txns are appended rather than prepended

- API changes:

  added:
  +setFullPrecision
  +setMinimalPrecision
  +expectParseStateOn
  +embedFileRelative
  +hereFileRelative

  changed:
  - amultiplier -> aismultiplier
  - Amount fields reordered for clearer debug output
  - tpreceding_comment_lines -> tprecedingcomment, reordered
  - Hledger.Data.TransactionModifier.transactionModifierToFunction -> modifyTransactions
  - Hledger.Read.Common.applyTransactionModifiers -> Hledger.Data.Journal.journalModifyTransactions

  - HelpTemplate -> CommandDoc


# 1.12 (2018/12/02)

-   switch to megaparsec 7 (Alex Chen)
    We now track the stack of include files in Journal ourselves, since
    megaparsec dropped this feature.

-   add 'ExceptT' layer to our parser monad again (Alex Chen)
    We previously had a parser type, 'type ErroringJournalParser = ExceptT
    String ...' for throwing parse errors without allowing further
    backtracking. This parser type was removed under the assumption that it
    would be possible to write our parser without this capability. However,
    after a hairy backtracking bug, we would now prefer to have the option to
    prevent backtracking.

    -   Define a 'FinalParseError' type specifically for the 'ExceptT' layer
    -   Any parse error can be raised as a "final" parse error
    -   Tracks the stack of include files for parser errors, anticipating the
        removal of the tracking of stacks of include files in megaparsec 7
        -   Although a stack of include files is also tracked in the 'StateT
            Journal' layer of the parser, it seems easier to guarantee correct
            error messages in the 'ExceptT FinalParserError' layer
        -   This does not make the 'StateT Journal' stack redundant because the
            'ExceptT FinalParseError' stack cannot be used to detect cycles of
            include files

-   more support for location-aware parse errors when re-parsing (Alex Chen)

-   make 'includedirectivep' an 'ErroringJournalParser' (Alex Chen)

-   drop Ord instance breaking GHC 8.6 build (Peter Simons)

-   flip the arguments of (divide\|multiply)\[Mixed\]Amount

-   showTransaction: fix a case showing multiple missing amounts
    showTransaction could sometimes hide the last posting's amount even if
    one of the other posting amounts was already implicit, producing invalid
    transaction output.

-   plog, plogAt: add missing newline

-   split up journalFinalise, reorder journal finalisation steps (#893) (Jesse Rosenthal)
    The `journalFinalise` function has been split up, allowing more granular
    control.

-   journalSetTime --> journalSetLastReadTime

-   journalSetFilePath has been removed, use journalAddFile instead

# 1.11.1 (2018/10/06)

-   add, lib: fix wrong transaction rendering in balance assertion errors
    and when using the add command

# 1.11 (2018/9/30)

-   compilation now works when locale is unset (#849)

-   all unit tests have been converted from HUnit+test-framework to easytest

-   doctests now run quicker by default, by skipping reloading between tests.
    This can be disabled by passing --slow to the doctests test suite
    executable.

-   doctests test suite executable now supports --verbose, which shows
    progress output as tests are run if doctest 0.16.0+ is installed
    (and hopefully is harmless otherwise).

-   doctests now support file pattern arguments, provide more informative output.
    Limiting to just the file(s) you're interested can make doctest start
    much quicker. With one big caveat: you can limit the starting files,
    but it always imports and tests all other local files those import.

-   a bunch of custom Show instances have been replaced with defaults,
    for easier troubleshooting. These were sometimes obscuring
    important details, eg in test failure output. Our new policy is:
    stick with default derived Show instances as far as possible, but
    when necessary adjust them to valid haskell syntax so pretty-show
    can pretty-print them (eg when they contain Day values, cf
    https://github.com/haskell/time/issues/101). By convention, when
    fields are shown in less than full detail, and/or in double-quoted
    pseudo syntax, we show a double period (..) in the output.

-   Amount has a new Show instance. Amount's show instance hid
    important details by default, and showing more details required
    increasing the debug level, which was inconvenient. Now it has a
    single show instance which shows more information, is fairly
    compact, and is pretty-printable.

        ghci> usd 1
        OLD:
        Amount {acommodity="$", aquantity=1.00, ..}
        NEW:
        Amount {acommodity = "$", aquantity = 1.00, aprice = NoPrice, astyle = AmountStyle "L False 2 Just '.' Nothing..", amultiplier = False}

    MixedAmount's show instance is unchanged, but showMixedAmountDebug
    is affected by this change:

        ghci> putStrLn $ showMixedAmountDebug $ Mixed [usd 1]
        OLD:
        Mixed [Amount {acommodity="$", aquantity=1.00, aprice=, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}}]
        NEW:
        Mixed [Amount {acommodity="$", aquantity=1.00, aprice=, astyle=AmountStyle "L False 2 Just '.' Nothing.."}]

-   Same-line & next-line comments of transactions, postings, etc.
    are now parsed a bit more precisely (followingcommentp).
    Previously, parsing no comment gave the same result as an empty
    comment (a single newline); now it gives an empty string.\
    Also, and perhaps as a consequence of the above, when there's no
    same-line comment but there is a next-line comment, we'll insert an
    empty first line, since otherwise next-line comments would get moved
    up to the same line when rendered.

-   Hledger.Utils.Test exports HasCallStack

-   queryDateSpan, queryDateSpan' now intersect date AND'ed date spans
    instead of unioning them, and docs are clearer.

-   pushAccount -> pushDeclaredAccount

-   jaccounts -> jdeclaredaccounts

-   AutoTransaction.hs -> PeriodicTransaction.hs & TransactionModifier.hs

-   Hledger.Utils.Debug helpers have been renamed/cleaned up

# 1.10 (2018/6/30)

-   build cleanly with all supported GHC versions again (7.10 to 8.4)

-   support/use latest base-compat (#794)

-   support/require megaparsec 6.4+

-   extensive refactoring and cleanup of parsers and related types and utilities

-   readJournalFile(s) cleanup, these now use InputOpts

-   doctests now run a bit faster (#802)

# 1.9.1 (2018/4/30)

-   new generic PeriodicReport, and some report-related type aliases

-   new BudgetReport

-   make (readJournal\|tryReader)s?WithOpts the default api, dropping "WithOpts"

-   automated postings and command line account aliases happen earlier
    in journal processing (see hledger changelog)

# 1.9 (2018/3/31)

-   support ghc 8.4, latest deps

-   when the system text encoding is UTF-8, ignore any UTF-8 BOM prefix
    found when reading files.

-   CompoundBalanceReport amounts are now normally positive.
    The bs/bse/cf/is commands now show normal income, liability and equity
    balances as positive. Negative numbers now indicate a contra-balance
    (eg an overdrawn checking account), a net loss, a negative net worth,
    etc. This makes these reports more like conventional financial
    statements, and easier to read and share with others. (experimental)

-   splitSpan now returns no spans for an empty datespan

-   don't count periodic/modifier txns in Journal debug output

-   lib/ui/web/api: move embedded manual files to extra-source-files

-   Use skipMany/skipSome for parsing spacenonewline (Moritz Kiefer)
    This avoids allocating the list of space characters only to then
    discard it.

-   rename, clarify purpose of balanceReportFromMultiBalanceReport

-   fix some hlint warnings

-   add some easytest tests

# 1.5 (2017/12/31)

-   -V/--value uses today's market prices by default, not those of last transaction date. #683, #648)

-   csv: allow balance assignment (balance assertion only, no amount) in csv records (Nadrieril)

-   journal: allow space as digit group separator character, #330 (Mykola Orliuk)

-   journal: balance assertion errors now show line of failed assertion posting, #481 (Sam Jeeves)

-   journal: better errors for directives, #402 (Mykola Orliuk)

-   journal: better errors for included files, #660 (Mykola Orliuk)

-   journal: commodity directives in parent files are inherited by included files, #487 (Mykola Orliuk)

-   journal: commodity directives limits precision even after -B, #509 (Mykola Orliuk)

-   journal: decimal point/digit group separator chars are now inferred from an applicable commodity directive or default commodity directive. #399, #487 (Mykola Orliuk)

-   journal: numbers are parsed more strictly (Mykola Orliuk)

-   journal: support Ledger-style automated postings, enabled with --auto flag (Dmitry Astapov)

-   journal: support Ledger-style periodic transactions, enabled with --forecast flag (Dmitry Astapov)

-   period expressions: fix "nth day of {week,month}", which could generate wrong intervals (Dmitry Astapov)

-   period expressions: month names are now case-insensitive (Dmitry Astapov)

-   period expressions: stricter checking for invalid expressions (Mykola Orliuk)

-   period expressions: support "every 11th Nov" (Dmitry Astapov)

-   period expressions: support "every 2nd Thursday of month" (Dmitry Astapov)

-   period expressions: support "every Tuesday", short for "every <n>th day of week" (Dmitry Astapov)

-   remove upper bounds on all but hledger* and base (experimental)
    It's rare that my deps break their api or that newer versions must
    be avoided, and very common that they release new versions which I
    must tediously and promptly test and release hackage revisions for
    or risk falling out of stackage. Trying it this way for a bit.

# 1.4 (2017/9/30)

-   add readJournalFile\[s\]WithOpts, with simpler arguments and support
    for detecting new transactions since the last read.

-   query: add payee: and note: query terms, improve description/payee/note docs (Jakub Zárybnický, Simon Michael, #598, #608)

-   journal, cli: make trailing whitespace significant in regex account aliases
    Trailing whitespace in the replacement part of a regular expression
    account alias is now significant. Eg, converting a parent account to
    just an account name prefix: --alias '/:acct:/=:acct'

-   timedot: allow a quantity of seconds, minutes, days, weeks, months
    or years to be logged as Ns, Nm, Nd, Nw, Nmo, Ny

-   csv: switch the order of generated postings, so account1 is first.
    This simplifies things and facilitates future improvements.

-   csv: show the "creating/using rules file" message only with --debug

-   csv: fix multiple includes in one rules file

-   csv: add "newest-first" rule for more robust same-day ordering

-   deps: allow ansi-terminal 0.7

-   deps: add missing parsec lower bound, possibly related to #596, fpco/stackage#2835

-   deps: drop oldtime flag, require time 1.5+

-   deps: remove ghc < 7.6 support, remove obsolete CPP conditionals

-   deps: fix test suite with ghc 8.2

# 1.3.1 (2017/8/25)

-   Fix a bug with -H showing nothing for empty periods (#583, Nicholas Niro)
    This patch fixes a bug that happened when using the -H option on
    a period without any transaction. Previously, the behavior was no
    output at all even though it should have shown the previous ending balances
    of past transactions. (This is similar to previously using -H with -E,
    but with the extra advantage of not showing empty accounts)

-   allow megaparsec 6 (#594)

-   allow megaparsec-6.1 (Hans-Peter Deifel)

-   fix test suite with Cabal 2 (#596)

# 1.3 (2017/6/30)

journal: The "uncleared" transaction/posting status, and associated UI flags
and keys, have been renamed to "unmarked" to remove ambiguity and
confusion. This means that we have dropped the `--uncleared` flag,
and our `-U` flag now matches only unmarked things and not pending
ones. See the issue and linked mail list discussion for more
background. (#564)

csv: assigning to the "balance" field name creates balance
assertions (#537, Dmitry Astapov).

csv: Doubled minus signs are handled more robustly (fixes #524, Nicolas Wavrant, Simon Michael)

Multiple "status:" query terms are now OR'd together. (#564)

deps: allow megaparsec 5.3.

# 1.2 (2017/3/31)

## journal format

A pipe character can optionally be used to delimit payee names in
transaction descriptions, for more accurate querying and pivoting by
payee. Eg, for a description like `payee name | additional notes`,
the two parts will be accessible as pseudo-fields/tags named `payee`
and `note`.

Some journal parse errors now show the range of lines involved, not just the first.

## ledger format

The experimental `ledger:` reader based on the WIP ledger4 project has
been disabled, reducing build dependencies.

## Misc

Fix a bug when tying the knot between postings and their parent transaction, reducing memory usage by about 10% (#483) (Mykola Orliuk)

Fix a few spaceleaks (#413) (Moritz Kiefer)

Add Ledger.Parse.Text to package.yaml, fixing a potential build failure.

Allow megaparsec 5.2 (#503)

Rename optserror -> usageError, consolidate with other error functions

# 1.1 (2016/12/31)

## journal format

-   balance assignments are now supported (#438, #129, #157, #288)

    This feature also brings a slight performance drop (\~5%);
    optimisations welcome.

-   also recognise `*.hledger` files as hledger journal format

## ledger format

-   use ledger-parse from the ledger4 project as an alternate reader for C++ Ledger journals

    The idea is that some day we might get better compatibility with Ledger files this way.
    Right now this reader is not very useful and will be used only if you explicitly select it with a `ledger:` prefix.
    It parses transaction dates, descriptions, accounts and amounts, and ignores everything else.
    Amount parsing is delegated to hledger's journal parser, and malformed amounts might be silently ignored.

    This adds at least some of the following as new dependencies for hledger-lib:
    parsers, parsec, attoparsec, trifecta.

## misc

-   update base lower bound to enforce GHC 7.10+

    hledger-lib had a valid install plan with GHC 7.8, but currently requires GHC 7.10 to compile.
    Now we require base 4.8+ everywhere to ensure the right GHC version at the start.

-   Hledger.Read api cleanups

-   rename dbgIO to dbg0IO, consistent with dbg0, and document a bug in dbg*IO

-   make readJournalFiles \[f\] equivalent to readJournalFile f (#437)

-   more general parser types enabling reuse outside of IO (#439)

# 1.0.1 (2016/10/27)

-   allow megaparsec 5.0 or 5.1

# 1.0 (2016/10/26)

## timedot format

-   new "timedot" format for retroactive/approximate time logging.

    Timedot is a plain text format for logging dated, categorised
    quantities (eg time), supported by hledger. It is convenient
    for approximate and retroactive time logging, eg when the
    real-time clock-in/out required with a timeclock file is too
    precise or too interruptive. It can be formatted like a bar
    chart, making clear at a glance where time was spent.

## timeclock format

-   renamed "timelog" format to "timeclock", matching the emacs package

-   sessions can no longer span file boundaries (unclocked-out

    sessions will be auto-closed at the end of the file).

-   transaction ids now count up rather than down (#394)

-   timeclock files no longer support default year directives

-   removed old code for appending timeclock transactions to journal transactions.

    A holdover from the days when both were allowed in one file.

## csv format

-   fix empty field assignment parsing, rule parse errors after megaparsec port (#407) (Hans-Peter Deifel)

## journal format

-   journal files can now include timeclock or timedot files (#320)

    (but not yet CSV files).

-   fixed an issue with ordering of same-date transactions included from other files

-   the "commodity" directive and "format" subdirective are now supported, allowing

    full control of commodity style (#295) The commodity directive's
    format subdirective can now be used to override the inferred
    style for a commodity, eg to increase or decrease the
    precision. This is at least a good workaround for #295.

-   Ledger-style "apply account"/"end apply account" directives are now used to set a default parent account.

-   the Ledger-style "account" directive is now accepted (and ignored).

-   bracketed posting dates are more robust (#304)

    Bracketed posting dates were fragile; they worked only if you
    wrote full 10-character dates. Also some semantics were a bit
    unclear. Now they should be robust, and have been documented
    more clearly. This is a legacy undocumented Ledger syntax, but
    it improves compatibility and might be preferable to the more
    verbose "date:" tags if you write posting dates often (as I do).
    Internally, bracketed posting dates are no longer considered to
    be tags. Journal comment, tag, and posting date parsers have
    been reworked, all with doctests.

-   balance assertion failure messages are clearer

-   with --debug=2, more detail about balance assertions is shown.

## misc

-   file parsers have been ported from Parsec to Megaparsec \o/ (#289, #366) (Alexey Shmalko, Moritz Kiefer)

-   most hledger types have been converted from String to Text, reducing memory usage by 30%+ on large files

-   file parsers have been simplified for easier troubleshooting (#275).

    The journal/timeclock/timedot parsers, instead of constructing
    opaque journal update functions which are later applied to build
    the journal, now construct the journal directly by modifying the
    parser state. This is easier to understand and debug. It also
    rules out the possibility of journal updates being a space
    leak. (They weren't, in fact this change increased memory usage
    slightly, but that has been addressed in other ways). The
    ParsedJournal type alias has been added to distinguish
    "being-parsed" journals and "finalised" journals.

-   file format detection is more robust.

    The Journal, Timelog and Timedot readers' detectors now check
    each line in the sample data, not just the first one. I think the
    sample data is only about 30 chars right now, but even so this
    fixed a format detection issue I was seeing.
    Also, we now always try parsing stdin as journal format (not just sometimes).

-   all file formats now produce transaction ids, not just journal (#394)

-   git clone of the hledger repo on windows now works (#345)

-   added missing benchmark file (#342)

-   our stack.yaml files are more compatible across stack versions (#300)

-   use newer file-embed to fix ghci working directory dependence (<https://github.com/snoyberg/file-embed/issues/18>)

-   report more accurate dates in account transaction report when postings have their own dates

    (affects hledger-ui and hledger-web registers).
    The newly-named "transaction register date" is the date to be
    displayed for that transaction in a transaction register, for
    some current account and filter query. It is either the
    transaction date from the journal ("transaction general date"),
    or if postings to the current account and matched by the
    register's filter query have their own dates, the earliest of
    those posting dates.

-   simplify account transactions report's running total.

    The account transactions report used for hledger-ui and -web
    registers now gives either the "period total" or "historical
    total", depending strictly on the --historical flag. It doesn't
    try to indicate whether the historical total is the accurate
    historical balance (which depends on the user's report query).

-   reloading a file now preserves the effect of options, query arguments etc.

-   reloading a journal should now reload all included files as well.

-   the Hledger.Read.* modules have been reorganised for better reuse.

    Hledger.Read.Utils has been renamed Hledger.Read.Common
    and holds low-level parsers & utilities; high-level read
    utilities are now in Hledger.Read.

-   clarify amount display style canonicalisation code and terminology a bit.

    Individual amounts still have styles; from these we derive
    the standard "commodity styles". In user docs, we might call
    these "commodity formats" since they can be controlled by the
    "format" subdirective in journal files.

-   Journal is now a monoid

-   expandPath now throws a proper IO error

-   more unit tests, start using doctest

0.27 (2015/10/30)

-   The main hledger types now derive NFData, which makes it easier to
    time things with criterion.

-   Utils has been split up more.

-   Utils.Regex: regular expression compilation has been memoized, and
    memoizing versions of regexReplace\[CI\] have been added, since
    compiling regular expressions every time seems to be quite
    expensive (#244).

-   Utils.String: strWidth is now aware of multi-line strings (#242).

-   Read: parsers now use a consistent p suffix.

-   New dependencies: deepseq, uglymemo.

-   All the hledger packages' cabal files are now generated from
    simpler, less redundant yaml files by hpack, in principle. In
    practice, manual fixups are still needed until hpack gets better,
    but it's still a win.

0.26 (2015/7/12)

-   allow year parser to handle arbitrarily large years
-   Journal's Show instance reported one too many accounts
-   some cleanup of debug trace helpers
-   tighten up some date and account name parsers (don't accept leading spaces; hadddocks)
-   drop regexpr dependency

0.25.1 (2015/4/29)

-   support/require base-compat >0.8 (#245)

0.25 (2015/4/7)

-   GHC 7.10 compatibility (#239)

0.24.1 (2015/3/15)

-   fix JournalReader "ctx" compilation warning
-   add some type signatures in Utils to help make ghci-web

0.24 (2014/12/25)

-   fix combineJournalUpdates folding order
-   fix a regexReplaceCI bug
-   fix a splitAtElement bug with adjacent separators
-   mostly replace slow regexpr with regex-tdfa (fixes #189)
-   use the modern Text.Parsec API
-   allow transformers 0.4*
-   regexReplace now supports backreferences
-   Transactions now remember their parse location in the journal file
-   export Regexp types, disambiguate CsvReader's similarly-named type
-   export failIfInvalidMonth/Day (fixes #216)
-   track the commodity of zero amounts when possible
    (useful eg for hledger-web's multi-commodity charts)
-   show posting dates in debug output
-   more debug helpers

0.23.3 (2014/9/12)

-   allow transformers 0.4*

0.23.2 (2014/5/8)

-   postingsReport: also fix date sorting of displayed postings (#184)

0.23.1 (2014/5/7)

-   postingsReport: with disordered journal entries, postings before the
    report start date could get wrongly included. (#184)

0.23 (2014/5/1)

-   orDatesFrom -> spanDefaultsFrom

0.22.2 (2014/4/16)

-   display years before 1000 with four digits, not three
-   avoid pretty-show to build with GHC < 7.4
-   allow text 1.1, drop data-pprint to build with GHC 7.8.x

0.22.1 (2014/1/6) and older: see http://hledger.org/release-notes or doc/CHANGES.md.
