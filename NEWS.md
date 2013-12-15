---
title: hledger news
---

# News

## 2013/12/13 hledger 0.22

**New:**

- balance: with a reporting interval (monthly, yearly etc.), the
  [balance command](MANUAL.html#balance) will now show a multi-column report, showing either
  the per-period changes in balance (by default),
  the period ending balances starting from zero (`--cumulative`),
  or the actual period ending balances (`--historical`).
  A more detailed specification of the balance command's behaviour
  has been added to [Hledger.Cli.Balance](http://hackage.haskell.org/package/hledger/docs/Hledger-Cli-Balance.html).

- csv: rules files can now include other rules files, useful for factoring out common rules

- queries: `sym:REGEXP` matches commodity symbols

- register: `--average/-A` shows a running average, like ledger

- in period expressions, `-` (hyphen) can be used as a more compact
  synonym for `from` and `to`.  Eg: `-p 2012/12/1-2013/2/1` or `date:aug-`.

- the add-on script examples in extra/ have been updated; get the
  hledger source and add .../hledger/extra/ to your PATH to make them
  available.  They include:

        hledger-accountnames.hs - print account names
        hledger-balance-csv.hs  - print a balance report as CSV
        hledger-equity.hs       - print an entry matching all account balances (like ledger)
        hledger-print-unique.hs - print only journal entries unique descriptions
        hledger-register-csv.hs - print a register report as CSV

**Improved:**

- balancesheet: now shows just assets and liabilities, not equity

- print: comment positions (same line or next line) are now preserved

- queries: `amt` now uses the = operator by default, eg `amt:50` is
  equivalent to `amt:=50`

- command line processing has been overhauled and made more
  consistent, and now has tests and debug output.  More flags now work
  both before and after COMMAND: `-f`, `--rule-file`, `--alias`,
  `--help`, `--debug`, `--version`.  Command line help, command
  aliases, API docs and code have been improved.

- `--debug` now takes an optional numeric argument to set the debug level
  higher than 1, for more verbose debug output in a few cases.

**Fixed:**

- csv: CSV data containing non-ascii characters is now supported

- build with latest versions of dependencies (text, warp, http-conduit etc.)

**Release contributors:**

Marko Kocić, Max Bolingbroke, and a big welcome to first-time committer John Wiegley! :)

## 2013/7/10 hledger-web 0.21.3

  - drop yesod-platform dependency, it is not worthwhile. The other
    yesod dependencies are currently without version ranges, so cabal
    install might require --constraint to restrict them in some cases.

## 2013/6/23 hledger 0.21.3

  - csv: fix wrong application of multiple assignments in a conditional block

## 2013/6/4 hledger 0.21.2

  - web: fix a build failure

## 2013/6/3 hledger 0.21.1

  - web: show proper Y-values in register chart (fixes #122)
  - web: avoid trailing commas in register chart values, in case of trouble with IE

## 2013/6/1 hledger 0.21

**Bugs fixed:**

  - parsing: don't fail when a csv amount has trailing whitespace (fixes #113)
  - web: don't show prices in the accounts sidebar (fixes #114)
  - web: show one line per commodity in charts. Needs more polish, but fixes #109.
  - web: bump yesod-platform dependency to avoid a cabal install failure

**Journal reading:**

  - balance assertions are now checked after reading a journal

**web command:**

  - web: support/require yesod 1.2
  - web: show zero-balance accounts in the sidebar (fixes #106)
  - web: use nicer select2 autocomplete widgets in the add form

**Documentation and infrastructure:**

  - add basic cabal test suites for hledger-lib and hledger

## 2013/5/4 hledger 0.20.0.1

  * web: require at least version 1.1.7 of yesod-core to avoid a potential build error
  * Update the bug tracker and source repository links on hackage

## 2013/5/1 hledger 0.20

**Bugs fixed:**

  * balance: a 0.19 regression which showed wrong total balance with `--flat` has been fixed (#94)
  * register: when `--date2` is used, the register is now sorted by the secondary date
  * web: some missing static & template files have been added to the package, fixing cabal-dev and hackage builds (#97, #98)
  * web: some hardcoded static urls have been fixed
  * Dependencies and code have been updated to support the latest
    libraries and GHC versions.  For now, hledger requires GHC 7.2+
    and hledger-web requires GHC 7.4+.

**Journal reading:**

  - DOS-style line-endings are now also supported in journal and rules files.
  - `!` is now accepted in the status field as well as `*`, like ledger
  - The *actual date* and *effective date* terminology has changed to *primary date* and *secondary date*.
    Use `--date2` to select the secondary date for reports. (`--aux-date` or `--effective` are also accepted
    for ledger and backwards compatibility).
  - Per-posting dates are supported, using hledger tags or ledger's posting date syntax
  - Comment and tag handling has been improved

**CSV reading:**

  - CSV conversion rules have a simpler, more flexible [syntax](MANUAL.html#csv-files).
    Existing rules files will need to be updated manually:
    - the filename is now `FILE.csv.rules` instead of `FILE.rules`
    - `FIELD-field N` is now `FIELD %N+1` (or set them all at once with a `fields` rule)
    - `base-currency` is now `currency`
    - `base-account` is now `account1`
    - account-assigning rules:
      add `if` before the list of regexps,
      add indented `account2 ` before the account name
  - parenthesised amounts are parsed as negative

**Querying:**

  - Use `code:` to match the transaction code (check number) field
  - Use `amt:` followed by `<`, `=` or `>` and a number N to match
    amounts by magnitude. Eg `amt:<0` or `amt:=100`. This works only
    with single-commodity amounts (multi-commodity amounts are
    always matched).
  - `tag:` can now match (exact, case sensitive) tag values. Eg `tag:TAG=REGEXP`.

**add comand:**

  - Transaction codes and comments (which may contain tags) can now be entered, following a date or amount respectively. (#45)
  - The current entry may be restarted by entering `<` at any prompt. (#47)
  - Entries are displayed and confirmed before they are written to the journal.
  - Default values may be specified for the first entry by providing them as command line arguments.
  - Miscellaneous UI cleanups

**register command:**

  - The `--related`/`-r` flag shows the other postings in each transaction, like ledger.
  - The `--width`/`-w` option increases or sets the output width.

**web command:**

  - The web command now also starts a browser, and auto-exits when unused, by default ("local ui mode").
    With `--server`, it keeps running and logs requests to the console ("server mode").
  - Bootstrap is now used for styling and layout
  - A favicon is served
  - The search field is wider
  - yesod devel is now supported; it uses `$LEDGER_FILE` or `~/.hledger.journal`
  - the `blaze_html_0_5` build flag has been reversed and renamed to `blaze_html_0_4`

**Add-ons:**

  - The hledger-interest and hledger-irr commands have been released/updated.
  - hledger-chart and hledger-vty remain unmaintained and deprecated.

**Documentation and infrastructure:**

  - The hledger docs and website have been reorganised and updated
  - Manuals for past releases are provided as well as the latest dev version
  - hledger has moved from darcs and darcs hub to git and github (!)
  - The bug tracker has moved from google code to github
  - Feature requests and project planning are now managed on trello
  - A build bot builds against multiple GHC versions on each commit

**Release contributors:**

- Sascha Welter commissioned register enhancements (--related and --width)
- David Patrick contributed a bounty for add enhancements
- Joachim Breitner added support for ! in status field
- Xinruo Sun provided hledger-web build fixes
- Peter Simons provided hledger-web build fixes, and a build bot
- Marko Kocić provided hledger-web fixes

<!-- Days since last release: 109\ -->
<!-- Commits since last release: 105 -->


## 2012/11/24 hledger-web 0.19.3

  * web: fix "Prelude.read: no parse" errors with GHC >= 7.6
  * web & lib refactoring

## 2012/11/16 hledger-web 0.19

  * builds with yesod 1.1.3
  * obeys command-line query options at startup again
  * the autogenerated session file is now a dot file
    (.hledger-web_client_session.aes)

## 2012/11/16 hledger 0.19.1

  * [87](http://bugs.hledger.org/87): fix an arithmetic and transaction balancing bug with multiple
    total-priced amounts ( @@ PRICE )
  * parsing: ignore ledger-style balance assertions ( = BAL ) and fixed
    lot price declarations ( {= PRICE} )


## 2012/10/21 hledger 0.19

  * hledger, hledger-lib: support GHC 7.6 and latest cmdargs, haskeline, split
  * balance report no longer has an O(n^2) slowdown with large numbers of accounts,
    and is generally more speedy. Benchmark on a 2010 macbook:

        +-------------------------------------------++--------------+--------------+--------+
        |                                           || hledger-0.18 | hledger-0.19 | ledger |
        +===========================================++==============+==============+========+
        | -f data/100x100x10.journal     balance    ||         0.21 |         0.07 |   0.09 |
        | -f data/1000x1000x10.journal   balance    ||        10.13 |         0.47 |   0.62 |
        | -f data/1000x10000x10.journal  balance    ||        40.67 |         0.67 |   1.01 |
        | -f data/10000x1000x10.journal  balance    ||        15.01 |         3.22 |   2.36 |
        | -f data/10000x1000x10.journal  balance aa ||         4.77 |         4.40 |   2.33 |
        +-------------------------------------------++--------------+--------------+--------+

  * build version is set with CPP instead of cabal-file-th

## 2012/7/7 hledger 0.18.2

  * web: fix compilation error with -fblaze_html_0_5 flag
  * bump base lower bound to 4.3 to enforce GHC 7 requirement

## 2012/6/29 hledger 0.18.1

  * register, print: fix reverse ordering of same-day transactions
  * balance: respect all query terms, not just acct
  * combine command-line flags like --depth properly with non-flag query patterns
  * web: don't auto-create a missing journal file at startup
  * stats: list included journal files
  * support tilde (~) in journal and rules file paths
  * expose more utilities from CsvReader
  * remove ensureRulesFile debug trace

## 2012/5/29 hledger 0.18

  * web: hledger-web is now based on yesod 1.0
  * web: fix js error breaking second use of add form ([#72](http://code.google.com/p/hledger/issues/detail?id=72))
  * web: make `yesod devel` work
  * the command-line now supports a more powerful [query language](MANUAL.html#queries), consistent with the web UI
  * hledger now fully supports [tags](MANUAL.html#tags) (aka metadata) on both transactions and postings, and querying by tag or tag value
  * new commands `incomestatement`, `balancesheet`, and `cashflow` provide basic financial statements under certain [conditions](http://hledger.org/MANUAL.html#incomestatement)
  * format conversion is now done on demand, and the convert command has been dropped. So instead of
    `hledger convert FILE.csv` just do `hledger -f FILE.csv print` or any other command.
    You can also pipe any supported format into `hledger -f- CMD` and hledger will try to do the right thing.
  * support for GHC 6.12 has been dropped; this release has been tested with GHC 7.0.4, 7.2.2, and 7.4.1
  * unicode is now handled properly on all supported GHC versions
  * API and internal cleanups

## 2012/3/3 hledger-web 0.17.1

  * set more upper bounds to fix cabal install issues with latest packages

## 2012/2/1 hledger 0.17

  * support HP 2011.4.0.0
  * support and require cmdargs 0.9
  * allow non-threaded builds, supporting more debian architectures
  * parsing: give a clearer error when journal file path contains ~
  * parsing: -B/--cost now ignores P historical prices, like ledger
  * parsing: inferred amounts now use the cost commodity if known, like ledger (#69)
  * balance: report differently-priced lots in an account as a single amount, like ledger
  * web: support and require yesod >= 0.9.4
  * web: use the main aeson package again
  * web: fix a regression with dollar signs in hamlet templates
  * web: add form allowed blank account names (#81)
  * chart, vty: hledger-chart and hledger-vty demoted to non-maintained extras for now

## 2011/11/1 HCAR

hledger is a library and end-user tool (with command-line, curses and web
interfaces) for converting, recording, and analyzing financial
transactions, using a simple human-editable plain text file format. It is
a haskell port and friendly fork of John Wiegley's Ledger, licensed under
GNU GPLv3+.

hledger aims to be a reliable, practical tool for daily use. It reports
charts of accounts or account balances, filters transactions by type,
helps you record new transactions, converts CSV data from your bank,
publishes your text journal with a rich web interface, generates simple
charts, and provides an API for use in your own financial scripts and
apps.

In the last six months there have been two major releases. 0.15 focussed
on features and 0.16 focussed on quality. Changes include:

- new modal command-line interface, extensible with hledger-* executables in the path
- more useful web interface, with real account registers and basic charts
- hledger-web no longer needs to create support files, and uses latest yesod & warp
- more ledger compatibility
- misc command enhancements, API improvements, bug fixes, documentation updates
- lines of code increased by 3k to 8k
- project committers increased by 6 to 21

Current plans include:

- Continue the release rhythm of odd-numbered = features, even-numbered =
  quality/stability/polish, and releasing on the first of a month

- In 0.17, clean up the storage layer, allow rcs integration via
  filestore, and read (or convert) more formats

- Keep working towards wider usefulness, improving the web interface and
  providing standard financial reports

## 2011/10/26 hledger-web 0.16.5

  * web: fix a ghc 6.12 incompatibility in Settings.hs

## 2011/10/24 hledger-web 0.16.4

  * web: yet another cabal install fix, fix AppConfig name clash

## 2011/10/4 hledger-web 0.16.3

  * web: another cabal install fix, disable favicon.ico since it's not easily embeddable

## 2011/10/4 hledger-web 0.16.2

  * web: more cabal install fixes (remove bad path, add routes and models) (#63)

## 2011/10/4 hledger 0.16.1

  * parsing: show correct line number for posting parse errors (#67)
  * web: declare static files as extra-source-files to fix cabal install (#63)
  * web: add a threaded flag for debian (#68)
  * web: fewer build warnings by default

## 2011/10/1 hledger 0.16

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/521)

  * cli: strip the -- when calling add-on commands, so their options work (#64)
  * cli: hledger ADDON --version now shows add-on command's version
  * cli: only the add and web commands auto-create the journal file
  * cli: give a non-confusing error if LEDGER_FILE contains a literal tilde
  * add: clearer prompts, more validation, use . to end also
  * add: use unix line endings consistently, avoiding parse error on windows (#51)
  * add: avoid excess whitespace between transactions (#46)
  * balance: ledger compatibility fix: don't elide parent accounts with multiple displayed subaccounts
  * convert: always order converted transactions by date
  * convert: rename currency -> base-currency, in-field, out-field -> amount-in-field, amount-out-field
  * convert: give an error, not a zero when date or amount-in-field/amount-out-field parsing fails
  * register: show more useful range of intervals with --empty and a query pattern
  * print, web: always show both dates, ignoring --effective (#42)
  * web: production builds (the default with cabal) have all web content embedded (dev builds use ./static/) (#63)
  * web: update to yesod 0.9
  * web: obey at least some of the general reporting options, like --cost
  * web: adjust the default base url when a custom port is specified
  * web: prevent an infinite redirect when custom base url has a trailing slash
  * web: fix "not:'multi word'" patterns
  * web: hide old title and search form when adding/editing
  * web: adjust --help to indicate command-line arguments are not expected
  * web: don't bother running cli unit tests at startup

## 2011/9/12 hledger 0.15.2, hledger-web 0.15.3

  * handle multiple filter patterns on the command-line again
  * don't pass an add-on command's name to it as an extra argument
  * don't give a confusing error with -f and no command
  * fix a regression balancing a transaction containing different prices
  * web: fix journal edit form
  * web: fix wrong transaction amount in account register with virtual postings
  * web: fix some invalid html

## 2011/9/2 hledger 0.15.1, hledger-web 0.15.2

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/479)

  * fix a parsec 2 incompatibility
  * web: add missing Hledger.Web.Options to cabal file
  * web: tighten up dependencies to reduce build problems

## 2011/9/1 hledger 0.15

[announcement](https://groups.google.com/forum/#!topic/hledger/-WCfnRFVaf0/discussion)

  * hledger's options are now modal, providing better help (using cmdargs)
  * hledger now lists and runs any hledger-* add-ons found in the user's path
  * case insensitivity of filter patterns has been fixed
  * parsing: `alias`/`end aliases` directives, for renaming accounts, are supported, like ledger's but a bit more powerful; also an `--alias` option for renaming on the fly
  * parsing: the `account` directive now preserves posting type (normal/virtual/balanced virtual)
  * parsing: the `pop` directive is supported as an alias for `end tag`, like ledger
  * parsing: `P` (historical price) directives can contain a (ignored) numeric time zone, like ledger
  * parsing: the leading `!` in directives is now optional and deprecated, like ledger
  * parsing: entries with a negative amount in the first posting now infer the correct balancing amount
  * parsing: bad date checking is more accurate
  * balance: collapsing of boring accounts to one line can be disabled with `--no-elide`
  * balance: fix a wrong precision regression from last release
  * convert: standard input can be converted
  * convert: an alternate rules file can be specified with `--rules`
  * convert: `account2-field` can be used when the CSV file specifies both accounts
  * convert: `description-field` can have a custom format and combine multiple CSV fields
  * convert: `in-field` and `out-field` support CSV files that use two amount columns
  * convert: don't fail when there's no default journal file
  * web: the web interface has been overhauled/cleaned up
  * web: account register views are now transaction-based, like gnucash etc., and show accurate historical balances when possible
  * web: simple balance charts are displayed (using flot)
  * web: more expressive and consistent search patterns, using a new matching engine
  * web: add form uses currently focussed account as default, redirects to itself, formats status messages better
  * web: sidebar now shows empty/boring accounts too
  * web: now uses warp and a newer yesod
  * api simplifications
  * importable Hledger, Hledger.Web, Hledger.Vty and Hledger.Chart modules
  * the basic reports are now provided by hledger-lib for easier reuse
  * new api use examples: `equity.hs`, `uniquify.hs`
  * some old base 3 support has been dropped
  * the old -s flag has been dropped

## 2011/05 HCAR

hledger is a haskell port and friendly fork of John Wiegley's ledger.  It
is a robust command-line accounting tool with a simple human-editable data
format. Given a plain text file describing transactions, of money or any
other commodity, hledger will print the chart of accounts, account
balances, or transactions you're interested in.  It can also help you
record transactions, or convert CSV data from your bank. There are also
curses and web interfaces. The project aims to provide a reliable,
practical day-to-day financial reporting tool, and also a useful library
for building financial apps in haskell.

Since hledger's last HCAR entry in 2009, hledger became cabalised, had 10
non-bugfix releases on hackage, split into multiple packages, acquired a
public mailing list, bug tracker, fairly comprehensive manual,
cross-platform binaries, and has grown to 5k lines of code and 15
committers. 0.14 has just been released, with 5 code committers.

The project is available under the GNU GPLv3 or later, at http://hledger.org .

Current plans are to continue development at a steady pace, to attract
more developers, and to become more useful to a wider range of users, eg
by building in more awareness of standard accounting procedures and by
improving the web and other interfaces.

## 2011/4/22 hledger 0.14

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/383)

  * remove the specific process dependency that caused too many cabal install problems
  * treat arguments as possibly-encoded platform strings, do not assume UTF-8
  * hledger now always reads and writes data as UTF-8, ignoring the system locale (#34)
  * look at the LEDGER_FILE env var for the journal path, otherwise LEDGER, like ledger
  * handle a blank LEDGER_FILE or LEDGER value more gracefully (use the default file path)
  * the default journal file path is now ~/.hledger.journal, to avoid breaking mac filevault (#41)
  * amounts with different prices are now aggregated, like ledger
  * zero amounts now have no sign or commodity, like ledger
  * parsing: assume current year when transaction dates have no year and there is no default year
  * parsing: more careful validation of eg leap years in transaction dates
  * parsing: better international number format support, allowing comma as decimal point and flexible digit groups (#32)
  * parsing: support @@ syntax specifying total price
  * parsing: infer the conversion price in transactions involving two unpriced commodities
  * parsing: support per-posting cleared status
  * parsing: more reporting interval syntax: biweekly, bimonthly, every N days/weeks/months/quarters/years, every Nst/nd/rd/th day of month/week
  * add: avoid offering account names for completion in inappropriate contexts
  * add: remember default account even if user submits a different amount.
  * convert: account-field directive specifies a field containing the base account name
  * convert: effective-date-field directive specifies a field containing the effective date
  * convert: date-format directive specifies custom date formats
  * convert: allow amount fields containing "AMT @@ PRICE"
  * histogram: honour the specified start or end dates
  * print: don't show a trailing space when description is blank
  * web: allow filter patterns with spaces if quoted, like command line
  * web: make edit form more cross-browser compatible, fixing it in firefox (#38)
  * web: move hidden add/edit/import forms below main content to help text-mode browsers a bit (#33)

Release contributors: Simon Michael, Dmitry Astapov, Eric Kow, Max Bolingbroke, Omari Norman.
Stats:
137 days, 113 commits, 11 end-user features and 15 end-user bugfixes since last release.
189 unit & functional tests and 59% unit test coverage (hledger, hledger-lib packages).
5540 lines of code (all packages).

## 2010/12/6 hledger 0.13

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/296)

  * move web, vty, chart commands into separate hledger-web, hledger-vty,
    hledger-chart packages. This both simplifies (no more build flags) and
    complicates (more room for dependency hassles), but I hope overall it
    will be easier and more scalable.
  * all packages but chart are now marked "beta", ie "not finished but
    suitable for everyday use"
  * parsing: ledger compatibility: support D default commodity directive
  * parsing: ledger compatibility: ignore metadata tags on transactions and postings
  * parsing: ledger compatibility: ignore cleared flags at the start of postings
  * parsing: ledger compatibility: ignore C commodity conversion directives
  * parsing: price precisions no longer affect commodities' display precisions
  * add: readline-style editing
  * add: tab-completion for account names
  * add: add the default commodity, if any, to commodity-less amounts (#26)
  * add: misc. commodity/precision/defaults-related bugfixes
  * chart: give a meaningful error message for empty journals
  * chart: update for current Chart lib (0.14)
  * web: support files now live in ./.hledger/web/ and will be auto-created at startup
  * web: page layout is more robust with wide content
  * web: allow editing of included files
  * web: handle multiple filter patterns correctly
  * web: allow single- or double-quoted filter patterns containing spaces
  * web: update for current yesod lib (0.6.*)
  * transaction balancing is now based on display precision (#23)
  * briefer, more informative usage error messages

## 2010/9/6 hledger 0.12.1

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/272)

  * web: fix account filtering breakage
  * installing: tighten up utf8-string dependency

## 2010/9/5 hledger 0.12

  * web: new, better web ui; accounts are now a permanent sidebar; add form uses auto-completing combo fields
  * installing: fix a build error with parsec 3 (#22)
  * installing: require exactly matching hledger-lib version for more robust builds
  * installing: explicit data-object dependency to ensure hledger and hledger-lib use the same time version
  * installing: explicit hamlet dependency for more robust building
  * installing: build threaded and with warnings
  * installing: drop -fweb610 flag
  * installing: add gtk2hs-buildtools dependency needed to build with -fchart
  * installing: require cabal 1.6 or greater
  * add -D/--daily flag
  * register: with --depth, clip account names or aggregate postings rather than excluding them
  * fix !include with deeply nested directories (#21)
  * fix obscured date parse errors with parsec 3
  * handle unicode better in errors
  * fix a ghc 6.12.3 error when running interpreted

Stats: 50 days and 90 commits since last release, now at 5741
lines of code with 136 tests and 41% unit test coverage.

## 2010/07/17 hledger 0.11.1

  * fix --version output

## 2010/07/17 hledger 0.11

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/253)

  * split --help, adding --help-options and --help-all/-H, and make
    it the default command
  * use "journal" instead of "ledger file"; default suffix is
    .journal, default file is \~/.journal
  * auto-create missing journal files rather than giving an error
  * new format-detecting file reader (mixed journal transactions
    and timelog entries are no longer supported)
  * work around for first real-world rounding issue (test zero to 8
    decimal places instead of 10)
  * when reporting a balancing error, convert the error amount to
    cost
  * parsing: support double-quoted commodity symbols, containing
    anything but a newline or double quote
  * parsing: allow minus sign before commodity symbol as well as
    after (also fixes a convert bug)
  * parsing: fix wrong parse error locations within postings
  * parsing: don't let trailing whitespace in a timelog description
    mess up account names
  * add: allow blank descriptions
  * balance: --flat provides a simple non-hierarchical format
  * balance: --drop removes leading account name components from a
    --flat report
  * print, register, balance: fix layout issues with
    mixed-commodity amounts
  * print: display non-simple commodity names with double-quotes
  * stats: layout tweaks, add payee/description count
  * stats: don't break on an empty file
  * stats: -p/--period support; a reporting interval generates
    multiple reports
  * test: drop verbose test runner and testpack dependency
  * web: a new web ui based on yesod, requires ghc 6.12; old ghc
    6.10-compatible version remains as -fweb610
  * web: allow wiki-like journal editing
  * web: warn and keep running if reloading the journal gives an
    error
  * web: --port and --base-url options set the webserver's tcp port
    and base url
  * web: slightly better browser opening on microsoft windows,
    should find a standard firefox install now
  * web: in a web-enabled build on microsoft windows, run the web
    ui by default

Stats: 55 days and 136 commits since last release. Now at 5552
lines of code with 132 tests and 54% unit test coverage.

## 2010/05/23 hledger 0.10

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/242)

  * fix too-loose testpack dependency, missing safe dependency
  * fix ghc 6.12 compatibility with -fweb
  * fix handling of non-ascii arguments with ghc 6.12
  * fix "0.8" in --version output
  * fix an occasional stack overflow error due to infinite
    recursion in Posting/Transaction equality tests
  * the -fwebhappstack build flag is gone for now, to avoid a cabal
    problem
  * parsing: if there is no description, don't require a space
    after the transaction date
  * parsing: balance balanced-virtual postings separately, allow
    them to have an implicit amount
  * parsing: timelog entries now generate balanced transactions,
    using virtual postings
  * parsing: simpler high-level parse error message
  * parsing: clearer bad date errors
  * add: fix wrongful program exit on bad dates
  * print: negative account patterns now exclude transactions
    containing any posting to a matched account
  * vty: rename the ui command to vty for consistency
  * vty: fix restricted account scope when backing up to top level
  * web: fix non-ascii handling with ghc 6.12
  * web: fix a bug possibly affecting reload-on-change
  * consolidate module namespace under Hledger, api cleanups

Stats: 44 days, 81 commits since last release. Now at 4904 lines of
code including tests, 144 tests, 53% coverage.

## 2010/04/10 hledger 0.9

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/239)

  * ghc 6.12 support
  * split off hledger-lib package, containing core types & utils
  * parsing: ignore D, C, N, tag, end tag directives; we should now
    accept any ledger 2.6 file
  * parsing: allow numbers in commodities if double-quoted, like
    ledger
  * parsing: allow transactions with empty descriptions
  * parsing: show a better error for illegal month/day numbers in
    dates
  * parsing: don't ignore trailing junk in a smart date, eg in web
    add form
  * parsing: don't ignore unparsed text following an amount
  * parsing: @ was being treated as a currency symbol
  * add: fix precision handling in default amounts (\#19)
  * add: elide last amount in added transactions
  * convert: keep original description by default, allow
    backreferences in replace pattern
  * convert: basic csv file checking, warn instead of dying when it
    looks wrong
  * convert: allow blank/comment lines at end of rules file
  * print: always show zero amounts as 0, hiding any
    commodity/decimal places/price, like ledger
  * register: fix bad layout with years < 1000
  * register: fix a Prelude.head error with reporting interval,
    --empty, and --depth
  * register: fix a regression, register should not show posting
    comments
  * register: with --empty, intervals should continue to ends of
    the specified period
  * stats: better output when last transaction is in the future
  * stats: show commodity symbols, account tree depth, reorder
    slightly
  * web: -fweb now builds with simpleserver; to get happstack, use
    -fwebhappstack instead
  * web: pre-fill the add form with today's date
  * web: help links, better search form wording
  * web: show a proper error for a bad date in add form (\#17)
  * web: fix for unicode search form values
  * web: fix stack overflow caused by regexpr, and handle requests
    faster (\#14)
  * web: look for more-generic browser executables
  * web: more robust browser starting (\#6)
  * error message cleanups
  * more tests, refactoring, docs

Stats: 58 days, 2 contributors, 102 commits since last release. Now
at 3983 lines of non-test code, 139 tests, 53% coverage.

## 2010/02/11 hledger 0.8

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/210)

  * parsing: in date=date2, use first date's year as a default for
    the second
  * add: ctrl-d doesn't work on windows, suggest ctrl-c instead
  * add: --no-new-accounts option disallows new accounts (Roman
    Cheplyaka)
  * add: re-use the previous transaction's date as default (Roman
    Cheplyaka)
  * add: a command-line argument now filters by account during
    history matching (Roman Cheplyaka)
  * chart: new command, generates balances pie chart (requires
    -fchart flag, gtk2hs) (Roman Cheplyaka, Simon Michael)
  * register: make reporting intervals honour a display expression
    (\#18)
  * web: fix help link
  * web: use today as default when adding with a blank date
  * web: re-enable account/period fields, they seem to be fixed,
    along with file re-reading (\#16)
  * web: get static files from the cabal data dir, or the current
    dir when using make (\#13)
  * web: preserve encoding during add, assuming it's utf-8 (\#15)
  * fix some non-utf8-aware file handling (\#15)
  * filter ledger again for each command, not just once at program
    start
  * refactoring, clearer data types

Stats: 62 days, 2 contributors, 76 commits since last release. Now
at 3464 lines of non-test code, 97 tests, 53% test coverage.

## 2009/12/11 hledger 0.7

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/193)

  * price history support (first cut): P directives now work,
    though differently from ledger. Each posting amount takes its
    fixed unit price from the price history (or
    @) when available. This is simple and useful for things like
    foreign currency expenses (but not investment tracking). Like
    ledger, balance and register don't show amount prices any more, and
    don't separate differently-priced amounts. Unlike ledger, print
    shows all amount prices, and supports -B.
  * --effective option, will use transactions' effective dates if
    any
  * convert: new rules file format, find/create rules file
    automatically, more robust parsing, more useful --debug output
  * print: always sort by date, fix long account name truncation,
    align amounts, show end of line comments, show all amounts for
    clarity (don't elide the final balancing amount)
  * ui: use vty 4, fixes non-ascii and gnome terminal problems
    (issues \#3, \#4)
  * web: allow data entry, react to data file changes, better
    layout, help links, remove histogram command and filter fields for
    now, fix bad localhost redirect, filter form did not work in eg
    firefox (issue \#7), reset link did not work in all browsers
  * parsing: require whitespace between date and status code, allow
    (and ignore) a time in price records, better error messages,
    non-zero exit code on parse failure
  * display non-ascii error messages properly (issue \#5)
  * fix an arithmetic bug that occasionally rejected valid
    transactions
  * fix a regex bug in showtree
  * don't break if HOME is undefined
  * --debug now implies --verbose
  * add functional tests like ledger's, use test-framework for
    speedy running, release shelltestrunner as a separate package
  * many hlint cleanups (Marko Kocić)
  * many site and documentation updates

Stats: 60 days, 1 contributor, 50 commits since last release. Now
at 3377 lines of non-test code, 97 tests, 53% test coverage.

## 2009/06/22 hledger 0.6.1

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/156)

  * avoid use of exitSuccess which was breaking ghc 6.8/base 3
    compatibility (issue \#2)

## 2009/06/13 hledger 0.6

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/1215)

  * now cabal-installable on unix, mac, and windows, with Haskell
    Platform
  * provide experimental platform binaries
  * parsing: fix a silly failure to open ledger file paths
    containing \~
  * parsing: show better errors for unbalanced transaction and
    missing default year
  * parsing: allow parentheses and brackets inside account names,
    as ledger does
  * parsing: fail on empty account name components, don't just
    ignore
  * add: description passed as arguments now affects first
    transaction only
  * add: better handling of virtual postings and default amounts
  * print, register: show virtual accounts bracketed/parenthesised
  * web: improved web ui supporting full patterns & period
    expressions
  * new "stats" command reports some ledger statistics
  * many dev/doc/deployment infrastructure improvements
  * move website into darcs repo, update home page
  * move issue tracker to google code

Release stats:

  * Contributors: Simon Michael
  * Days since last release: 21
  * Commits: 94
  * Lines of non-test code: 2865
  * Tests: 82
  * Test coverage: 53% expressions
  * Known errors: 3 (inconsistent eliding, vty-related failures)
  * Performance: similar
    (http://hledger.org/profs/200906131120.bench)

## 2009/05/23 hledger 0.5.1

  * two fixes: really disable vty flag by default, and include
    ConvertCommand in cabal file

## 2009/05/23 hledger 0.5

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/1181)

  * the vty flag is disabled by default again, to ease installation
    on windows
  * use ledger 3 terminology: a ledger contains transactions which
    contain postings
  * new "add" command prompts for transactions interactively and
    adds them to the ledger
  * new "convert" command transforms bank CSV exports to ledger
    format, with rule-based cleanup
  * new "histogram" command shows transaction counts per day or
    other reporting interval
  * most commands now work properly with UTF8-encoded text (Sergey
    Astanin)
  * invoking as "hours" is now less different: it just uses your
    timelog, not your ledger
  * ..quarterly/-Q option summarises by quarter
  * ..uncleared/-U option looks only at uncleared transactions
  * be more accurate about checking balanced amounts, don't rely on
    display precision
  * enforce balancing for bracketed virtual postings
  * fix bug in eliding of posting amounts
  * don't show trailing spaces on amountless postings
  * parse null input as an empty ledger
  * don't treat comments as part of transaction descriptions
  * require some postings in ledger transactions
  * require a non-empty description in ledger transactions
  * don't fail when matching an empty pattern, as in "not:"
  * make the web server handle the null path
  * code, api and documentation updates
  * add a contributor agreement/list

Release stats:

  * Contributors: Simon Michael, Sergey Astanin
  * Days since last release: 51
  * Commits: 101
  * Lines of non-test code: 2795
  * Tests: 76
  * Known errors: 0

## 2009/05 HCAR

hledger is a (primarily) command-line accounting tool similar to John
Wiegley's "ledger".  It reads a plain text journal file describing money
or commodity transactions, or timelog entries, and generates precise
activity and balance reports.

Since the last report, hledger has reached release 0.4 on Hackage. It has
60 test cases, new features such as basic curses and web-based interfaces,
and has had some performance tuning. It is now quite useful for day to day
reporting of money and time. Also, the project has a new web address
(hledger.org), and has attracted two new committers.

## 2009/04/03 hledger 0.4

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/1097)

  * new "web" command serves reports in a web browser (install with
    -f happs to build this)
  * make the vty-based curses ui a cabal build option, which will
    be ignored on MS windows
  * drop the ..options-anywhere flag, that is now the default
  * patterns now use not: and desc: prefixes instead of \^ and \^\^
  * patterns are now case-insensitive, like ledger
  * !include directives are now relative to the including file (Tim
    Docker)
  * "Y2009" default year directives are now supported, allowing m/d
    dates in ledger
  * individual transactions now have a cleared status
  * unbalanced entries now cause a proper warning
  * balance report now passes all ledger compatibility tests
  * balance report now shows subtotals by default, like ledger 3
  * balance report shows the final zero total when -E is used
  * balance report hides the final total when ..no-total is used
  * ..depth affects print and register reports (aggregating with a
    reporting interval, filtering otherwise)
  * register report sorts transactions by date
  * register report shows zero-amount transactions when -E is used
  * provide more convenient timelog querying when invoked as
    "hours"
  * multi-day timelog sessions are split at midnight
  * unterminated timelog sessions are now counted. Accurate time
    reports at last!
  * the test command gives better ..verbose output
  * ..version gives more detailed version numbers including
    patchlevel for dev builds
  * new make targets include: ghci, haddocktest, doctest, unittest,
    view-api-docs
  * a doctest-style framework for functional/shell tests has been
    added

Release stats:

  * Contributors: Simon Michael, Tim Docker; thanks to the HAppS,
    happstack and testpack developers
  * Days since release: 76
  * Commits: 144
  * Lines of non-test code: 2367
  * Tests: 56
  * Known errors: 0

## 2009/01/17 hledger 0.3

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/67)

  * count timelog sessions on the day they end, like ledger, for
    now
  * when options are repeated, use the last instead of the first
  * builds with ghc 6.10 as well as 6.8
  * a simple ui for interactive report browsing: hledger ui
  * accept smart dates everywhere (YYYYMMDD, Y/M/D, Y, M/D, D, jan,
    today, last week etc.)
  * ..period/-p flag accepting period expressions like "in 2008",
    "weekly from last month"..
  * -W/-M/-Y convenience flags to summarise register weekly,
    monthly, yearly
  * ..depth and -E flags also affect summarised register reports
    (including depth=0)
  * ..display/-d flag supporting date predicates (like "d<[DATE]",
    "d\>=[DATE]")
  * !include directive to include additional ledger files
  * !account directive to set a default parent account
  * Added support for reading historical prices from files
  * timelog and ledger entries can be intermixed in one file
  * modifier and periodic entries can appear anywhere (but are
    still ignored)
  * help and readme improvements
  * runs much faster than 0.2

Release stats:

  * Contributors: Simon Michael, Nick Ingolia, Tim Docker; thanks
    to Corey O'Connor & the vty team
  * Lines of non-test code: 2123
  * Tests: 58
  * Known errors: 1

## 2008/11/23 hledger 0.2

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/826)

  * fix balance report totals when filtering by account
  * fix balance report selection of accounts when filtering by
    account
  * fix a bug with account name eliding in balance report
  * if we happen to be showing a not-yet-auto-balanced entry, hide
    the AUTO marker
  * fix print command filtering by account
  * omit transactions with zero amount from register report
  * Fix bug in parsing of timelogs
  * rename ..showsubs to ..subtotal, like ledger
  * drop ..usage flag
  * don't require quickcheck
  * priced amounts (eg "10h @ $50") and ..basis/..cost/-B flag to
    show them with cost basis
  * easy ..depth option, equivalent to ledger's -d 'l<=N'
  * smarter y/m/d date parsing for -b and -e (any number of digits,
    month and day default to 1, separator can be / - or .)
  * -n flag for balance command
  * ..empty/-E flag
  * build a library, as well as the exe
  * new home page url (http://joyful.com/hledger)
  * publish html and pdf versions of README
  * detect display preferences for each commodity like ledger
  * support amounts with multiple currencies/commodities
  * support ..real/-R flag
  * support -C/..cleared flag to filter by entry status (not
    transaction status)
  * support virtual and balanced virtual transactions
  * parse comment lines beginning with a space, as from M-; in
    emacs ledger-mode
  * allow any non-whitespace in account names, perhaps avoiding
    misleading missing amounts errors
  * clearer error message when we can't balance an entry
  * when we fail because of more than one missing amount in an
    entry, show the full entry
  * document the built-in test runner in ..help
  * add a ..verbose/-v flag, use it to show more test-running
    detail

Release stats:

  * Contributors: Simon Michael, Tim Docker
  * Lines of non-test code: 1350
  * Tests: 43
  * Known errors: 0

## 2008/11 HCAR

hledger is a command-line accounting tool similar to John Wiegley’s ledger tool.

The first release has been published on Hackage, and has attracted some
interest. It can be used for generating simple balance and transaction
reports from a plain-text general ledger. A home page and mail list has
also been created.

Immediate plans are to add some more of the most useful features from 
ledger, so that hledger can be used for day-to-day finances, and to grow
the community of contributors.

## 2008/10/15 hledger 0.1

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/775)

Release stats:

  * Contributors: Simon Michael
