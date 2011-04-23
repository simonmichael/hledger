---
title: hledger news
---

# News

## [2011/4/22 hledger 0.14](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/383)

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

## [2010/12/6 hledger 0.13](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/296)

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

## [2010/9/6 hledger 0.12.1](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/272)

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

## [2010/07/17 hledger 0.11](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/253)

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

## [2010/05/23 hledger 0.10](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/242)

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

## [2010/04/10 hledger 0.9](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/239)

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

## [2010/02/11 hledger 0.8](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/210)

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

## [2009/12/11 hledger 0.7](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/193)

  * price history support (first cut): P directives now work,
    though differently from c++ ledger. Each posting amount takes its
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

## [2009/06/22 hledger 0.6.1](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/156)

  * avoid use of exitSuccess which was breaking ghc 6.8/base 3
    compatibility (issue \#2)

## [2009/06/13 hledger 0.6](http://thread.gmane.org/gmane.comp.finance.ledger.general/1215)

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

## [2009/05/23 hledger 0.5](http://thread.gmane.org/gmane.comp.finance.ledger.general/1181)

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

## [2009/04/03 hledger 0.4](http://thread.gmane.org/gmane.comp.finance.ledger.general/1097)

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

## [2009/01/17 hledger 0.3](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/67)

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

## [2008/11/23 hledger 0.2](http://thread.gmane.org/gmane.comp.finance.ledger.general/826)

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
  * easy ..depth option, equivalent to c++ ledger's -d 'l<=N'
  * smarter y/m/d date parsing for -b and -e (any number of digits,
    month and day default to 1, separator can be / - or .)
  * -n flag for balance command
  * ..empty/-E flag
  * build a library, as well as the exe
  * new home page url (http://joyful.com/hledger)
  * publish html and pdf versions of README
  * detect display preferences for each commodity like c++ ledger
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

## [2008/10/15 hledger 0.1](http://thread.gmane.org/gmane.comp.finance.ledger.general/775)

Release stats:

  * Contributors: Simon Michael
