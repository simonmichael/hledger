Internal/api/developer-ish changes in the hledger-lib (and hledger) packages.
For user-visible changes, see the hledger package changelog.

# aa20f34b

- lib, cli, ui: start using Control.Monad.Fail, allow base-compat 0.11
  fail is moving out of Monad and into it's own MonadFail class.
  This will be enforced in GHC 8.8 (I think).

  base-compat/base-compat-batteries 0.11.0 have adapted to this,
  and are approaching stackage nightly
  (https://github.com/commercialhaskell/stackage/issues/4802).

  hledger is now ready to build with base-compat-batteries 0.11.0, once
  all of our deps do (eg aeson). We are still compatible with the older
  0.10.x and GHC 7.10.3 as well.

  For now we are using both fails:

  - new fail (from Control.Monad.Fail), used in our parsers, imported
    via base-compat-batteries Control.Monad.Fail.Compat to work with
    older GHC versions.

  - old fail (from GHC.Base, exported by Prelude, Control.Monad,
    Control.Monad.State.Strict, Prelude.Compat, ...), used in easytest's
    Test, since I couldn't find their existing fail implementation to update.

  To reduce (my) confusion, these are imported carefully, consistently,
  and qualified everywhere as Fail.fail and Prelude.fail, with clashing
  re-exports suppressed, like so:

      import Prelude hiding (fail)
      import qualified Prelude (fail)
      import Control.Monad.State.Strict hiding (fail)
      import "base-compat-batteries" Prelude.Compat hiding (fail)
      import qualified "base-compat-batteries" Control.Monad.Fail.Compat as Fail

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
    one of the other posting amounts was already implcit, producing invalid
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
