<!--
      _ _ 
  ___| (_)
 / __| | |
| (__| | |
 \___|_|_|

-->
User-visible changes in the hledger command line tool and library.


# 8ec59eafa

Breaking changes

Features

- The new `demo` command plays brief asciinema "screencasts"
  explaining various features and use cases.

Improvements

- Commands list cleanups

Fixes

Scripts/addons

Docs

# 1.29.1 2023-03-16

Improvements

- Hledger.Cli.Script now also exports

       Control.Applicative
       Control.Concurrent
       Data.Char
       Data.Functor
       System.IO
       System.IO.Error

   and new string helpers

       strip1Char
       stripBy
       strip1By

- Allow building with GHC 9.6.1 (#2011)

Fixes

- The stats report no longer displays "Exact" in front of dates. (#2012)

Docs

- remove duplicate in `hledger close` docs (Yehoshua Pesach Wallach)

# 1.29 2023-03-11

Breaking changes

- Weekly reports are no longer automatically adjusted to start on a
  monday; in some cases you might need to adjust their start date to
  preserve simple week headings (see below).

Features

- In journal format there is now a `tag` directive for declaring tag names,
  and the check command now has a `tags` check to enforce use of declared tag names.

- Periodic transactions and multi-period reports can now start on any date.
  To enable this while still maintaining pretty good backward compatibility,
  hledger now treats inferred dates, and dates where the day is unspecified,
  as "flexible" (which can be automatically adjusted to interval boundaries),
  and dates specified to the day as "exact" (which can not).
  Eg:
  
  - A periodic rule like `~ monthly from 2023-01-15` now works as
    you'd expect instead of raising an error. This also improves
    our ability to read Ledger files.

  - Period options like `-p 'monthly from 2023/1/15'` or `-M -b 2023/1/15`
    now start the report on exactly 1/15 instead of being adjusted to 1/1.
  
  Note: periods using `in` may look partial but are considered to specify exact dates.
  So weekly reports such as `-p 'weekly in 2023-01'`, which previously
  were adjusted to start on a monday, will now start exactly on 2023-01-01.
  This can also cause more verbose column headings.
	To guarantee simple week headings, you must now start such reports
  exactly on a monday, eg `-p 'weekly from 2022-12-26 to 2023-02'`.
  (#1982)

- You can now freely combine @/@@ costs and conversion postings
  in a single transaction. This can help readability, and also allows
  more flexibility when recording cost.  hledger will check that the
  two notations are in agreement, and ignore the redundancy if they are.
  (Conversion postings are postings to accounts with type `V`/`Conversion`
  or name `equity:conversion`/`equity:trade`/`equity:trading`,
  or subaccounts of these. See also COST.)

Improvements

- hledger's commands list has been reorganised for clarity.
  More add-on commands are now recognised and categorised,
  and unrecognised add-on commands are listed in a more compact
  multi-column layout.
  (Simon Michael, Michael Grünewald)

- hledger's commands list and command line help now use ANSI (bold
  headings) when supported.
  
- hledger's commands list and command line help now use a pager
  (respecting $PAGER) for long output except on MS Windows.

- hledger's `--version` output no longer shows `+` for dev builds made
  in dirty repos (it was buggy).

- The add command's Description completions now also include payee names
  (declared with `payee` or recorded in transactions with `|`),
  not just full descriptions.

- aregister now supports HTML output.
  (#1996) (Jonathan Dowland)

- aregister now shows a " (matching query)" hint in report title 
  when extra query args (other than date: or depth:) are used,
  to reduce confusion.

- close now has three modes, `--retain`/`--migrate`/`--open`,
  clarifying its uses and providing more useful defaults.

- register-match is now the `--match` mode of the register command.
  (This command was used by ledger-autosync at one point; if you still
  need it, hopefully `register --match` works similarly.)

- print-unique has been dropped, because it doesn't
  support print's options, it disorders same-day transactions, I don't
  know of any users or use cases, and it could easily be recreated as
  an addon script.

- print's JSON output now also includes source positions for `--forecast` transactions.
  (Chris Lemaire)

- Journal format now allows the empty commodity symbol to be written
  as `""`, so it's now possible to declare market prices for it:
  `P 2022-01-01 "" $100`.  This can be useful for timedot data.

- Inferring costs from equity now happens after transaction balancing,
  not before.  As a result, `--infer-costs` now works in transactions
  where an amount is left blank.

- `account` declarations now reject parenthesised account names,
  reducing confusion.
  (Chris Lemaire)

- Our journal reader now accepts more Ledger syntax, improving Ledger
  file compatibility (#1962).  We now test our ability to at least
  read the sample journals from Ledger's baseline functional tests,
  and our success rate has improved from 80% to 90% since 1.28.
  
  - `since` is accepted as synonym of `from` in period expressions
  - `apply year` and `year` are accepted as synonyms of `Y`
  - `(lot notes)` in amounts and `((valuation expressions))` after amounts are now ignored
  - directives 
    `A`, `assert`, `bucket`, `capture`, `check`, `define`,
    `expr`, `eval`, `python`, `value`,
    `apply fixed`, `apply tag`,
    `end apply fixed`, `end apply tag`, `end apply year`
	are now ignored
  - subdirectives of `payee`, `tag`, and `commodity` (other than `format`) are now ignored
  - `pop` directive is no longer supported

- When reading CSV, we now check that assigned account names are valid (parseable).
  (#1978)

Fixes

- aregister now handles an extra account query correctly. (#2007)

- balance's `--help` now mentions `--layout=tidy`

- Balance commands with `--layout=bare` now generate proper table
  layout in HTML output.

- register's `-w`/`--width` option no longer gives ugly parse error messages.

- stats's `--help` no longer wrongly claims to support -O/--output-format.

- Balance assignments with a cost now generate a correct balance assertion. (#1965)

- The CSV reader now properly skips header lines before attempting to parse records. (#1967)

Scripts/addons

- Scripts can now use Hledger.Cli.Script, a convenient new prelude which
  helps reduce import boilerplate. It currently re-exports:

	  Control.Monad
	  Data.Either
	  Data.List
	  Data.Maybe
	  Data.Ord
	  Data.Time
	  Text.Printf hiding (formatString)
	  Data.Text (Text, pack, unpack)
	  Safe hiding (at)
	  System.Directory
	  System.Environment
	  System.Exit
	  System.FilePath
	  System.Process
	  Hledger
	  Hledger.Cli
	  Hledger.Cli.Main (argsToCliOpts)

  (Not much of Data.Text/Data.Text.IO because those need to be qualified.)

Docs

- chunk the hledger manual into parts, rename and rearrange sections for better structure/flow
- add a cheatsheet demonstrating all the main journal features that I recommend
- move a number of my not-so-recommended journal features into a less visible "Other syntax" section
- add: payees/descriptions completion
- areg: more advice on account-matching
- bal: --budget: clarify use of print --forecast
- bal: budget: compare with forecasting; add some tips
- balance cleanups/reorder
- check: adjacentconversionpostings was dropped
- cli: balance: fix link to Budgeting page
- cli: fix all links to Journal > Tags / Commands > tags
- codes: improve example suggested by Rob Nielsen
- csv, timeclock, timedot: clarify comment lines (#1953)
- csv: add new coinbase example
- csv: clarify amount-in/amount-out docs (#1970)
- csv: clarify skip/valid csv semantics (#1967)
- csv: clarify valid CSV requirements and issues (fix #1966)
- csv: cleanup, reorder, CSV rules tips -> Working with CSV
- csv: fix wrong if tables doc; rewrite several sections (#1977)
- csv: flatten, clean up CSV sections
- csv: improve Amount field / Setting amounts
- csv: note -in and -out are used together for one posting (#1970)
- csv: rules factoring tips
- csv: try to clarify how CSV fields and hledger fields work
- document --infer-market-prices with signed costs (#1870)
- fix duplicate market prices heading breaking info navigation
- import: note a pitfall with multifile import
- improve Directives summaries
- introduction/input/output improvements
- journal: cheatsheet: clarify date tag
- journal: rewrite Account names, mention brackets/parentheses (#1915)
- mention pivoting on a tag with multiple values (#1950)
- more cost notation docs; describe Ledger and Beancount cost notation
- more mention of posting order effect on inferring cost (#1959)
- period expressions doc updates
- Removed redundant paragraph in documentation. (J. B. Rainsberger)
- rename directive sections, fix many links
- reorganise commands list, like the CLI
- reorganise bin/README & the Scripts page, add entries for recent scripts
- replace "transaction prices" terminology with "costs"
- tags: discuss multi-values/overriding (#1950)
- update market price inference docs per sol
- Updated section on pivoting. Used synonyms for "member" in cases where there could be confusion with the tag named "member." (Robert Nielsen)
- use more standard and consistent boilerplate in hledger, ui, web man pages
- virtual postings: improve wording per Robert Nielsen


# 1.28 2022-12-01

Features

- The `accounts` command has new flags: `--undeclared` (show accounts used but not declared), `--unused` (show accounts declared but not used),  and `--find` (find the first account matched by the first command argument, a convenience for scripts). Also `-u` and `-d` short flags have been added for `--used` and `--declared`.

- A new CSV rule `intra-day-reversed` helps generate transactions in correct order with CSVs where records are reversed within each day.

- CSV rules can now correctly convert CSV date-times with a implicit or explicit timezone to dates in your local timezone. Previously, CSV date-times with a different time zone from yours could convert to off-by-one
  dates, because the CSV's timezone was ignored.
  Now,

  1. When a CSV has date-times with an implicit timezone different from yours, you can use the `timezone` rule to declare it.

  2. CSV date-times with a known timezone (either declared by `timezone` or parsed with `%Z`) will be localised to the system timezone
     (or to the timezone set with the `TZ` environment variable).

  (#1936)
  
Improvements

- print --match now respects -o and -O.

- print --match now returns a non-zero exit code when there is no acceptable match.

- Support megaparsec 9.3. (Felix Yan)

- Support GHC 9.4.

Fixes

- In CSV rules, when assigning a parenthesised account name to   `accountN`, extra whitespace is now ignored, allowing unbalanced postings to be detected correctly.

Scripts/addons

- bin/hledger-move helps record transfers involving subaccounts and costs,
  eg when withdrawing some or all of an investment balance containing many lots and costs.

- bin/hledger-git no longer uses the non-existent git record command.
  (#1942) (Patrick Fiaux)

- bin/watchaccounts is a small shell script for watching the account tree as you make changes.

# 1.27.1 2022-09-18

Fixes

- Balance commands using `-T -O html` no longer fail with an error
  when there is no data to report.
  (#1933)

# 1.27 2022-09-01

Features

- `hledger check recentassertions` (and flycheck-hledger in Emacs if
  you enable this check) requires that all balance-asserted accounts
  have a balance assertion within 7 days before their latest posting.

  This helps remind you to not only record transactions, but also to
  regularly check account balances against the real world, to catch
  errors sooner and avoid a time-consuming hunt.

- The --infer-costs general flag has been added, as the inverse
  operation to --infer-equity.  --infer-costs detects commodity
  conversion transactions which have been written with equity
  conversion postings (the traditional accounting notation) and adds
  PTA cost notation (@@) to them (allowing cost reporting).
  See https://hledger.org/hledger.html#equity-conversion-postings .
  (Stephen Morgan)

Improvements

- Many error messages have been improved. Most error messages now use
  a consistent, more informative format. 
  (#1436)

- The accounts command has a new --directives flag which makes it
  show valid account directives which you can paste into a journal.

- The accounts command has a new --positions flag which shows where
  accounts were declared, useful for troubleshooting.
  (#1909)

- Bump lower bounds for Diff and githash. (Andrew Lelechenko)

- GHC 8.6 and 8.8 are no longer supported. Building hledger now
  requires GHC 8.10 or greater.

Fixes

- Account display order is now calculated correctly even when accounts
  are declared in multiple files.
  (#1909)

- At --debug 5 and up, account declarations info is logged.
  (#1909)

- hledger aregister and hledger-ui now show transactions correctly
  when there is a type: query.
  (#1905)

- bal: Allow cumulative gain and valuechange reports.
  Previously, --cumulative with --gain or --valuechange would produce an
  empty report. This fixes this issue to produce a reasonable report.
  (Stephen Morgan)

- bal: budget goal amounts now respect -c styles (fixes #1907)

- bal: budget goals now respect -H (#1879)

- bal: budget goals were ignoring rule-specified start date

- cf/bs/is: Fixed non-display of child accounts when there is an
  intervening account of another type.
  (#1921) (Stephen Morgan)

- roi: make sure empty cashflows are skipped when determining first cashflow (Charlotte Van Petegem)
  Empty cashflows are added when the begin date of the report is before the first
  transaction.

Scripts/addons

- https://hledger.org/scripts.html - an overview of scripts and addons in bin/.

- paypaljson, paypaljson2csv - download txns from paypal API

- hledger-check-postable.hs - check that no postings are made to accounts with a postable:(n|no) tag

- hledger-addon-example.hs - script template


# 1.26.1 2022-07-11

- require safe 0.3.19+ to avoid deprecation warning

# 1.26 2022-06-04

Improvements

- `register` and `aregister` have been made faster, by 

  - considering only the first 1000 items for choosing column
    widths. You can restore the old behaviour (guaranteed alignment
    across all items) with the new `--align-all` flag.
    ([#1839](https://github.com/simonmichael/hledger/issues/1839), Stephen Morgan)

  - discarding cost data more aggressively, giving big speedups for
    large journals with many costs.
  	([#1828](https://github.com/simonmichael/hledger/issues/1828), Stephen Morgan)

- Most error messages from the journal reader and the `check` command now use
  a consistent layout, with an "Error:" prefix, line and column numbers,
  and an excerpt highlighting the problem. Work in progress.
  ([#1436](https://github.com/simonmichael/hledger/issues/1436)) (Simon Michael, Stephen Morgan)

- `hledger check ordereddates` now always checks all transactions
  (previously it could be restricted by query arguments).

- The `--pivot` option now supports a `status` argument, to pivot on transaction status.

- Update bash completions (Jakob Schöttl)

Fixes

- Value reports with `--date2` and a report interval (like `hledger bal -VM --date2`)
  were failing with a "expected all spans to have an end date" error since 1.22;
  this is now fixed.
  ([#1851](https://github.com/simonmichael/hledger/issues/1851), Stephen Morgan)

- In CSV rules, interpolation of a non-existent field like `%999` or `%nosuchfield`
  is now ignored (previously it inserted that literal text).
  Note this means such an error will not be reported; 
  Simon chose this as the more convenient behaviour when converting CSV.
  Experimental.
  ([#1803](https://github.com/simonmichael/hledger/issues/1803), [#1814](https://github.com/simonmichael/hledger/issues/1814)) (Stephen Morgan)

- `--infer-market-price` was inferring a negative price when selling.
  ([#1813](https://github.com/simonmichael/hledger/issues/1813), Stephen Morgan)

- Allow an escaped forward slash in regular expression account aliases.
  ([#982](https://github.com/simonmichael/hledger/issues/982), Stephen Morgan)

- The `tags` command now also lists tags from unused account declarations.
  It also has improved command-line help layout.
  ([#1857](https://github.com/simonmichael/hledger/issues/1857))

- `hledger accounts` now shows its debug output at a more appropriate level (4).

# 1.25 2022-03-04

Breaking changes

- Journal format's `account NAME  TYPECODE` syntax, deprecated in 1.13, has been dropped.
  Please use `account NAME  ; type:TYPECODE` instead.
  (Stephen Morgan)

- The rule for auto-detecting "cash" (liquid asset) accounts in the `cashflow` report  
  has changed: it's now "all accounts under a top-level `asset` account, with 
  `cash`, `bank`, `checking` or `saving` in their name" (case insensitive, variations allowed).  
  So if you see a change in your `cashflow` reports, you might need to add
  `account` directives with `type:C` tags, declaring your top-most cash accounts.

Features

- The new `type:TYPECODES` query matches accounts by their accounting type.
  Account types are declared with a `type:` tag in account directives,
  or inferred from common english account names, or inherited from parent accounts,
  as described at [Declaring accounts > Account types].
  This generalises the account type detection of `balancesheet`, `incomestatement` etc.,
  so you can now select accounts by type without needing fragile account name regexps.
  Also, the `accounts` command has a new `--types` flag to show account types.
  Eg:

      hledger bal type:AL  # balance report showing assets and liabilities
      hledger reg type:x   # register of all expenses
      hledger acc --types  # list accounts and their types

  ([#1820](https://github.com/simonmichael/hledger/issues/1820), 
  [#1822](https://github.com/simonmichael/hledger/issues/1822)) 
  (Simon Michael, Stephen Morgan)

- The `tag:` query can now also match account tags, as defined in account directives.
  Subaccounts inherit tags from their parents.
  Accounts, postings and transactions can be filtered by account tag.
  ([#1817](https://github.com/simonmichael/hledger/issues/1817))

- The new `--infer-equity` flag replaces the `@`/`@@` price notation in commodity
  conversion transactions with more correct equity postings (when not using `-B/--cost`).
  This makes these transactions fully balanced, and preserves the accounting equation.
  For example:

      2000-01-01
        a             1 AAA @@ 2 BBB
        b            -2 BBB

      $ hledger print --infer-equity
      2000-01-01
        a                               1 AAA
        equity:conversion:AAA-BBB:AAA  -1 AAA
        equity:conversion:AAA-BBB:BBB   2 BBB
        b                              -2 BBB

  
  `equity:conversion` is the account used by default. To use a different account,
  declare it with an account directive and the new `V` (`Conversion`) account type.
  Eg:
  
      account Equity:Trading    ; type:V

  ([#1554](https://github.com/simonmichael/hledger/issues/1554)) (Stephen Morgan, Simon Michael)

- Balance commands (`bal`, `bs` etc.) can now generate easy-to-process "tidy" CSV data 
  with `-O csv --layout tidy`.
  In tidy data, every variable is a column and each row represents a single data point 
  (cf <https://vita.had.co.nz/papers/tidy-data.html>).
  ([#1768](https://github.com/simonmichael/hledger/issues/1768), 
  [#1773](https://github.com/simonmichael/hledger/issues/1773), 
  [#1775](https://github.com/simonmichael/hledger/issues/1775)) 
  (Stephen Morgan)

Improvements

- Strict mode (`-s/--strict`) now also checks periodic transactions (`--forecast`) 
  and auto postings (`--auto`). 
  ([#1810](https://github.com/simonmichael/hledger/issues/1810)) (Stephen Morgan)

- `hledger check commodities` now always accepts zero amounts which have no commodity symbol. 
  ([#1767](https://github.com/simonmichael/hledger/issues/1767)) (Stephen Morgan)

- Relative [smart dates](hledger.html#smart-dates) may now specify an arbitrary number of some period into the future or past).
  Some examples:
  - `in 5 days`
  - `in -6 months`
  - `5 weeks ahead`
  - `2 quarters ago`
  
  (Stephen Morgan)

- CSV output now always disables digit group marks (eg, thousands separators),
  making it more machine readable by default. 
  ([#1771](https://github.com/simonmichael/hledger/issues/1771)) (Stephen Morgan)

- Unicode may now be used in field names/references in CSV rules files.
  ([#1809](https://github.com/simonmichael/hledger/issues/1809)) (Stephen Morgan)

- Error messages improved:
  - Balance assignments
  - aregister
  - Command line parsing (less "user error")

Fixes

- `--layout=bare` no longer shows a commodity symbol for zero amounts. 
  ([#1789](https://github.com/simonmichael/hledger/issues/1789)) (Stephen Morgan)

- `balance --budget` no longer elides boring parents of unbudgeted accounts 
  if they have a budget. 
  ([#1800](https://github.com/simonmichael/hledger/issues/1800)) (Stephen Morgan)

- `roi` now reports TWR correctly

  - when there are several PnL changes occurring on a single day
  - and also when investment is fully sold/withdrawn/discounted at the end of a particular reporting period.

  ([#1791](https://github.com/simonmichael/hledger/issues/1791)) (Dmitry Astapov)

Documentation

- There is a new CONVERSION & COST section, replacing COSTING. 
  ([#1554](https://github.com/simonmichael/hledger/issues/1554))

- Some problematic interactions of account aliases with other features have been noted. 
  ([#1788](https://github.com/simonmichael/hledger/issues/1788))

- Updated: [Declaring accounts > Account types](https://hledger.org/hledger.html#account-types)

# 1.24.1 2021-12-10

Fixes

- `balance --declared` is now filtered correctly by a `not:ACCT` query.
  (#1783)

- More reliable --version output, with commit date and without patch level.

API changes:

- new type synonyms ProgramName, PackageVersion, VersionString
- versionStringForProgname -> versionString with extra argument
- versionStringFor -> versionStringWith with extra argument

# 1.24 2021-12-01

Features

- balance commands provide more control over how multicommodity amounts
  are displayed. (And they no longer elide too-wide amounts by default.)
  The --commodity-column flag has been deprecated and replaced by a new
  --layout option, with three values:
  
  - wide (the default, shows amounts on one line unelided, like older hledger versions)
  - tall (a new display mode, shows one amount per line)
  - bare (like the old --commodity-columm, shows one commodity per line with symbols in their own column)
  
  (Stephen Morgan)

- The balance commands have a new `--declared` flag, causing them to
  include leaf (ie, non-parent) accounts declared by account directives,
  even if they contain no transactions yet. Together with `-E`, this shows
  a balance for both used and declared accounts.
  The idea is to be able to see a useful "complete" balance report, even
  when you don't have transactions in all of your declared accounts yet.
  (#1765)

- journal files now support a `decimal-mark` directive as a more
  principled way (than `commodity` directives) to specify the decimal character
  in use in that file, to ensure accurate number parsing.
  (#1670, Lawrence Wu)

Improvements

- The stats command now shows rough but useful performance stats: run
  time and processing speed in transactions per second.

- balance: support the --related flag, like register, showing the
  other postings from the transactions. (#1469, Stephen Morgan)

- roi now uses posting dates when available, and honors the --date2
  flag. This will not change the results computed for the typical
  use-case, it just makes "roi" more thorough/consistent.
  (Dmitry Astapov)

- aregister now shows transactions' secondary date if the --date2 flag is used.
  (#1731)

- timedot: a D default commodity (and style) declared in a parent
  journal file will now be applied to timedot amounts. This means they
  can be priced and valued/converted.

- cli: The --pretty and --forecast options can now be written after the
  command name, like other general options.
  (Stephen Morgan)

- register -V -H with no interval now values at report end date, like balance.
  (#1718, Stephen Morgan)

- Allow megaparsec 9.2.

- Drop the base-compat-batteries dependency. (Stephen Morgan)

Fixes

- prices: Do not include zero amounts when calculating amounts for balance assignments. (#1736) (Stephen Morgan)
  This is not usually a problem, but can get in the way of auto-inferring prices.

- csv: Successfully parse an empty csv file. (#1183) (Stephen Morgan)

- balance: Balance reports with --depth=0 properly report aggregated
  values, not zero everywhere. (#1761) (Stephen Morgan)

- prices: Do not try to generate prices when there would be a zero
  denominator. Also correctly generate reverse prices for zero
  amounts. (Stephen Morgan)

- csv: Allow both amount-in and amount-out fields to contain a zero.
  (#1733, Stephen Morgan)

- balance: Balance reports should consider date: queries when
  calculating report span with --date2. (#1745, Stephen Morgan)

- print: auto: The print command should always display inferred
  amounts for --auto generated postings. (#1276, Stephen Morgan)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


# 1.23 2021-09-21

Features

- The balance command has a new `--gain` report type, showing
  unrealised capital gains/losses. Essentially, this is the difference
  between the amounts' costs and their total present value. More
  precisely, between the value of the amounts' costs and the value of
  the amounts on the valuation date(s). (Ie, you can report gain in a
  different currency.)
  ([#1623](https://github.com/simonmichael/hledger/issues/1623),
  [#1432](https://github.com/simonmichael/hledger/issues/1432),
  Stephen Morgan, Charlotte Van Petegem)

- The new `-c/--commodity-style` option makes it easy to override
  commodity display styles at runtime, eg to adjust the number of
  decimal places or change the position of the symbol.
  ([#1593](https://github.com/simonmichael/hledger/issues/1593), Arjen Langebaerd)

- The balance commands have a new `--commodity-column` flag that
  displays commodity symbols in a dedicated column, showing one line
  per commodity and all amounts as bare numbers.
  ([#1559](https://github.com/simonmichael/hledger/issues/1559),
  [#1626](https://github.com/simonmichael/hledger/issues/1626),
  [#1654](https://github.com/simonmichael/hledger/issues/1654),
  Lawrence Wu, Simon Michael, Stephen Morgan)

- The `balance --budget` option can now take an argument,
  a case insensitive description substring which selects a subset of
  the journal's periodic transactions for setting budget goals. 
  This makes it possible to keep multiple named budgets in one journal, 
  and select the one you want with --budget's argument. 
  ([#1612](https://github.com/simonmichael/hledger/issues/1612))

- Period expressions now support `every weekday`, `every weekendday` and
  `every mon,wed,...` (multiple days of the week).
  This is intended for periodic transaction rules used with
  `--forecast` (or `bal --budget`).
  ([#1632](https://github.com/simonmichael/hledger/issues/1632), Lawrence Wu)

- The new `--today=DATE` option allows overriding today's date. This
  can be useful in tests and examples using relative dates, to make
  them reproducible.
  ([#1674](https://github.com/simonmichael/hledger/issues/1674), Stephen Morgan)

- In CSV rules, multi-line comments are now supported. Newlines in CSV
  data are preserved, or newlines can be added by writing `\n` when
  assigning to `comment`, `comment1` etc. 
  (Malte Brandy)

Improvements

- Incremental performance improvements; hledger 1.23 is the fastest
  hledger yet, about 10% faster than 1.22. 
  (Stephen Morgan)

- `register` no longer slows down when there are many report intervals.
  ([#1683](https://github.com/simonmichael/hledger/issues/1683), Stephen Morgan)

- Numbers in SQL output now always use decimal period (`.`),
  independent of commodity display styles. 
  (Stephen Morgan)

- `--sort` now gives a more intuitive sort oder when there are
   multiple commodities. Negative numbers in one commodity are always
   less than positive numbers in another commodity.
   ([#1563](https://github.com/simonmichael/hledger/issues/1563), Stephen Morgan)

- `--infer-market-price` has been renamed to `--infer-market-prices`.
  (The old spelling still works, since we accept flag prefixes.)

- Our pretty-printed JSON now orders object attributes alphabetically,
  across all GHC and haskell lib versions.

- register with a report interval starting on custom dates
  (eg: `hledger reg -p "every 15th day of month") now makes the 
  date column wide enough to show the start and end dates.
  It also wastes less whitespace after the column.
  ([#1655](https://github.com/simonmichael/hledger/issues/1655), Stephen Morgan)

- The --forecast option will now reject a report interval in its
  argument, instead of silently ignoring it.

- In JSON output, object attributes are now ordered alphabetically,
  consistently for all GHC and haskell lib versions. 
  ([#1618](https://github.com/simonmichael/hledger/issues/1618), Stephen Morgan)

- JSON output now indents with 2 spaces rather than 4. 
  (Stephen Morgan)

- The balance commands' `-S/--sort-amount` flag now behaves more
  predictably and intuitively with multiple commodities.
  Multi-commodity amounts are sorted by comparing their amounts in
  each commodity, with alphabetically-first commodity symbols being
  most significant, and assuming zero with alphabetically-first commodity symbols being
  most significant, and assuming zero when a commodity is missing.
  ([#1563](https://github.com/simonmichael/hledger/issues/1563), 
  [#1564](https://github.com/simonmichael/hledger/issues/1564), Stephen Morgan)
  
- The close command now uses the later of today or journal's last day
  as default closing date, providing more intuitive behaviour when
  closing a journal with future transactions. Docs have been improved.
  ([#1604](https://github.com/simonmichael/hledger/issues/1604))

- Rules for selecting the forecast period (within with --forecast
  generates transactions) have been tweaked slightly, and
  some disagreement between docs and implementation has been fixed.
  Now, the forecast period begins on:
  - the start date supplied to the `--forecast` argument, if any
  - otherwise, the later of
    - the report start date if specified with -b/-p/date:
    - the day after the latest normal (non-periodic) transaction in the journal, if any
  - otherwise today.

  It ends on:
  - the end date supplied to the `--forecast` argument, if any
  - otherwise the report end date if specified with -e/-p/date:
  - otherwise 180 days (6 months) from today.

  This is more intuitive in some cases. (Eg:
  `hledger reg --forecast -b 2020-01-01` on a journal containing 
  only periodic transaction rules now shows forecast transactions 
  starting from 2020-01-01, rather than from today.)
  ([#1648](https://github.com/simonmichael/hledger/issues/1648), 
  [#1665](https://github.com/simonmichael/hledger/issues/1665),
  [#1667](https://github.com/simonmichael/hledger/issues/1667), 
  Stephen Morgan, Simon Michael)

- Require base >=4.11, prevent red squares on Hackage's build matrix.
  (We officially support GHC 8.6+, which means base 4.12, 
  but Hackage shows all packages building successfully with 
  base 4.11/GHC 8.4+ somehow, so it's still allowed..)

Fixes

- A rare bug causing incorrect balances to be reported by the
  cf/bs/bse/is commands, since hledger 1.19, has been fixed.
  (cf/bs/bse/is with --tree --no-elide --begin DATE and certain
  account directives could show wrong balances).
  ([#1698](https://github.com/simonmichael/hledger/issues/1698), Stephen Morgan)

- aregister now aligns multicommodity amounts properly (broken since 1.21).
  ([#1656](https://github.com/simonmichael/hledger/issues/1656), Stephen Morgan)

- `balance -E` (and hledger-ui Z) now correctly show zero parent accounts,
  fixing a bug introduced in hledger 1.19.
  ([#1688](https://github.com/simonmichael/hledger/issues/1688), Stephen Morgan)

- The `roi` command no longer gives an ugly error in a certain case
  with PnL applied on the first day of investment. (Dmitry Astapov)

- `--forecast` now generates transactions up to the day before the
  specified report end date (instead of two days before).
  ([#1633](https://github.com/simonmichael/hledger/issues/1633), Stephen Morgan)

- Certain errors in CSV conversion, such as a failing balance assertion,
  were always being reported as line 2.

# 1.22.2 2021-08-07

Breaking changes

- aregister no longer hides future transactions by default.
  This is a consequence of the fix for 
  [#1638](https://github.com/simonmichael/hledger/issues/1638). 
  It makes aregister consistent, so we think it's a reasonable change.
  So if you have future-dated transactions in your journal which you
  don't want reported, you now must exclude them with `-e tomorrow` or
  `date:-tomorrow` in the command, as with other reports.
  (Stephen Morgan)

Improvements

- Timedot format's doc has been rewritten.

Fixes

- Make balance assignments in forecasted transactions work again
  (broken in 1.22.1).
  Forecast transactions are now generated early and processed
  in the same way as other transactions.
  ([#1638](https://github.com/simonmichael/hledger/issues/1638), Stephen Morgan)

- aregister preserves the order of same-day transactions again
  (broken in 1.22.1).
  ([#1642](https://github.com/simonmichael/hledger/issues/1642), Stephen Morgan)

# 1.22.1 2021-08-02

Improvements

- Bash shell completions (for hledger, hledger-ui, hledger-web) are
  now included in the hledger package's release tarballs, making them
  more likely to be installed by system packages. (Jakob Schöttl)

- roi docs now discuss how to quote multi-word queries. (#1609, Dmitry
  Astapov)

- Allow megaparsec 9.1

Fixes

- `cur:` and `amt:` queries now match the original amounts before
  valuation and cost conversion, as they did before hledger 1.22. We
  believe this is the more useful behaviour in practice. (#1625)
  (Stephen Morgan)

- Queries now work better with `register --related`, no longer showing
  duplicate postings when more than one posting in a transaction is
  matched. (#1629) (Stephen Morgan)

- Valuation now works with `register --related`. (#1630) (Stephen
  Morgan)

- Auto posting rules now also see inferred amounts,
  not just explicit amounts. (#1412) (Stephen Morgan)

- Our info manuals now have more robust directory metadata (no
  subdirectory path), making them more likely to be linked in your
  top-level Info directory by system packages. (#1594) (Simon Michael,
  Damien Cassou)

- The error message for a non-existent input file no longer shows
  excess double quotes. (#1601, Stephen Morgan)

- Journal format docs: The commodity directive's scope is now
  correctly described (lasts until end of current file).

- The aregister command now properly ignores a `depth:` argument. 
  It might now also behave more correctly with valuation or `--txn-dates`.
  (#1634, Stephen Morgan)

# 1.22 2021-07-03

Features

- check: A new `balancednoautoconversion` check requires transactions
  to balance without the use of inferred transaction prices. (Explicit
  transaction prices are allowed.) This check is included in `--strict`
  mode. The old `autobalanced` check has been renamed to
  `balancedwithautoconversion`. (Stephen Morgan)

Improvements

- Many internal optimisations have been applied (cf hledger-lib
  changelog). Overall, you can expect most reports to be about 20%
  faster. The register report is more than 2x faster and uses 4x less
  memory. (Stephen Morgan)

      ~/src/hledger$ quickbench -w hledger-1.21,hledger
      Running 5 tests 1 times with 2 executables at 2021-06-29 13:13:26 HST:
    
      Best times:
      +----------------------------------------------------++--------------+---------+
      |                                                    || hledger-1.21 | hledger |
      +====================================================++==============+=========+
      | -f examples/10000x1000x10.journal print            ||         1.18 |    0.90 |
      | -f examples/10000x1000x10.journal register         ||        12.82 |    5.95 |
      | -f examples/10000x1000x10.journal balance          ||         1.38 |    0.86 |
      | -f examples/1000x1000x10.journal balance --weekly  ||         0.96 |    0.78 |
      | -f examples/10000x1000x10.journal balance --weekly ||        13.07 |   10.79 |
      +----------------------------------------------------++--------------+---------+

- ANSI color is now disabled automatically (on stdout) when the
  `-o/--output-file` option is used (with a value other than `-`).
  (#1533)

- ANSI color is now also available in debug output, determined in the
  usual way by `--color`, `NO_COLOR`, and whether the output (stderr)
  is interactive.
  
- The --version flag shows more details of the build, when known: git
  tag, number of commits since the tag, commit hash, platform and
  architecture. (Stephen Morgan)

- balance: Capitalisation of "account" and "total" (and lack of a
  colon in the latter) in CSV output is now consistent for single- and
  multi-period reports.

- balance reports' CSV output now includes full account names. (#1566)
  (Stephen Morgan)

- csv: We now accept spaces when parsing amounts from CSV. (Eric
  Mertens)

- json: Avoid adding unnecessary decimal places in JSON output. (Don't
  increase them all to 10 decimal places.) (Stephen Morgan)
  
- json: Simplify amount precision (asprecision) in JSON output.
  It is now just the number of decimal places, rather than an object.
  (Stephen Morgan)

- GHC 9.0 is now officially supported, and GHC 8.0, 8.2, 8.4 are not;
  building hledger now requires GHC 8.6 or greater.

- Added a now-required lower bound on containers. (#1514)

Fixes

- Auto posting rules now match postings more precisely, respecting
  `cur:` and `amt:` queries. (#1582) (Stephen Morgan)

- balance reports: Fix empty cells when amounts are too wide to fit
  (broken since 1.20) (#1526). (Stephen Morgan)

- csv: Fix the escaping of double quotes in CSV output (broken in
  1.21). (Stephen Morgan)

- register: Fix the running total when there is a report interval
  (broken since 1.19) (#1568). (Stephen Morgan)

- stats: No longer gets confused by posting dates. (#772) (Stephen Morgan)

- timeclock: `hledger print` shows timeclock amounts with just 2
  decimal places again (broken in 1.21). (#1527)

- When all transaction amounts have the same sign, the error message
  no longer adds an inferred price. (#1551) (Stephen Morgan)

- Cleaned up some references to old man pages. (Felix Yan)

# 1.21 2021-03-10

## general

- hledger is now generally about 10% more memory- and time-efficient,
  and significantly more so in certain cases, eg journals with many
  total transaction prices. (Stephen Morgan)

- The `--help/-h` and `--version` flags are no longer position-sensitive;
  if there is a command argument, they now always refer to the command
  (where applicable).

- The new `--info` flag opens the hledger info manual, if "info" is in $PATH.
  `hledger COMMAND --info` will open COMMAND's info node.

- The `--man` flag opens the hledger man page, if "man" is in $PATH.
  `hledger COMMAND --man` will scroll the page to CMD's section, if "less"
  is in $PATH. (We force the use of "less" in this case, overriding any
  $PAGER or $MAN_PAGER setting.)

- Some command aliases, considered deprecated, have been removed:
  `txns`, `equity`, and the single-letter command aliases `a`, `b`,
  `p`, and `r`. This was discussed at
  https://github.com/simonmichael/hledger/pull/1423 and on the hledger
  mail list. It might annoy some folks; please read the issue and do
  follow up there if needed.
  
- Notable documentation updates:
  the separate file format manuals have been merged into the hledger manual,
  the topic hierarchy has been simplified,
  the `balance` command docs and "commands" section have been rewritten.

## valuation

- Costing and valuation are now independent, and can be combined.
  `--value=cost` and `--value=cost,COMM` are still supported
  (equivalent to `--cost` and `--cost --value=then,COMM` respectively), 
  but deprecated. (Stephen Morgan)

- `-V` is now always equivalent to `--value=end`. (Stephen Morgan)

- `--value=end` now includes market price directives as well as
  transactions when choosing a valuation date for single-period
  reports. (#1405, Stephen Morgan)

- `--value=end` now picks a consistent valuation date for single- and
  and multi-period reports. (#1424, Stephen Morgan)

- `--value=then` is now supported with all reports, not just register. (Stephen Morgan)

- The too-vague `--infer-value` flag has been renamed to `--infer-market-price`.
  Tip: typing `--infer-market` or even `--infer` is sufficient.
  The old spelling still works, but is now deprecated.

## commands

- add: Infix matches are now scored higher. If the search pattern
  occurs in full within the other description, that match gets a +0.5
  score boost.

- add: `--debug` now shows transaction matching results, useful when
  troubleshooting.

- balance: To accomodate new report types, the
  `--change|--cumulative|--historical|--budget` flags have been split
  into two groups: report type (`--sum|--budget|...`) and accumulation
  type (`--change|--cumulative|--historical`). `--sum` and `--change`
  are the defaults, and your balance commands should still work as
  before. (Stephen Morgan et al, #1353)

- balance: The `--valuechange` report type has been added, showing the
  changes in period-end values. (Stephen Morgan, #1353)

- balance: With `--budget`, the first and last subperiods are enlarged
  to whole intervals for calculating the budget goals also. (Stephen
  Morgan)

- balance: In multi-period balance reports, specifying a report period
  now also forces leading/trailing empty columns to be displayed,
  without having to add `-E`. This is consistent with `balancesheet`
  etc. (#1396, Stephen Morgan)

- balancesheet, cashflow: declaring just a Cash account no longer
  hides other Asset accounts.

- check: Various improvements:

  - check name arguments may be given as case-insensitive prefixes
  - `accounts` and `commodities` may also be specified as arguments
  - `ordereddates` now checks each file separately (#1493)
  - `ordereddates` no longer supports the `--unique` flag or query arguments
  - `payees` is a new check requiring payee declarations
  - `uniqueleafnames` now gives a fancy error message like the others
  - the old `checkdates`/`checkdupes` commands have been dropped

- help: The `help` command now shows only the hledger (CLI) manual,
  its `--info/--man/--pager` flags have been renamed to `-i/-m/-p`,
  and `--cat` has been dropped.

- help: With a TOPIC argument (any heading or heading prefix, case
  insensitive), it will open the manual positioned at this topic if
  possible. (Similar to the new `--man` and `--info` flags described above.)
  <!-- `hledger help print` will show `print`'s doc with the best available viewer (usually info). -->
  <!-- `hledger help print -m` is equivalent to `hledger print --man`.) -->

- payees: Add `--used`/`--declared` flags, like the `accounts` command.

- print: Now always shows amounts with all decimal places,
  unconstrained by commodity display style. This ensures more
  parseable and sensible-looking output in more cases, and behaves
  more like Ledger's print. (There may be a cosmetic issue with
  trailing zeroes.) (#931, #1465)

- print: With `--match`, infix matches are now scored higher, as with
  the add command.

- print: `--match` now provides debug output useful for troubleshooting.

  If you forget to give `--match` an argument, it can confusingly
  consume a following flag. Eg if you write:

      hledger print --match -x somebank   # should be: hledger print --match=somebank -x

  it gets quietly parsed as:

      hledger print --match="-x"

  Now you can at least use --debug to figure it out:

      hledger print --match -x somebank --debug
      finding best match for description: "-x"
      similar transactions:
      ...

- roi: Now supports the valuation options (#1417), and uses commodity display styles.
  Also the manual has been simplified, with some content moved to the Cookbook.
  (Dmitry Astapov):

## journal format

- The `commodity` directive now properly sets the display style of the
  no-symbol commodity. (#1461)

## csv format

- More kinds of malformed signed numbers are now ignored, in
  particular just a sign without a number, which simplifies sign
  flipping with amount-in/amount-out.

## API

- API changes include:
  ```
  Hledger.Cli.Utils:
  +journalSimilarTransaction
  
  Hledger.Cli.Commands.Add:
   transactionsSimilarTo -> Hledger.Data.Journal.journalTransactionsSimilarTo
    and now takes a number-of-results argument
  ```


# 1.20.4 2021-01-29

- aregister: ignore a depth limit, as in 1.19 (#1468).
  In 1.20-1.20.3, aregister had stopped showing transactions in subaccounts 
  below a depth limit. Now it properly shows all subaccount transactions, 
  ensuring that the register's final total matches a balance report with 
  similar arguments.

# 1.20.3 2021-01-14

- When searching for price chains during valuation/currency conversion:

  - It no longer hangs when there are price loops. (And in case of
    future bugs, it will give up rather than search forever.) (#1439)
  - It now really finds the shortest path. (#1443)
  - Useful progress info is displayed with `--debug=1` or `--debug=2`.

- balance, incomestatement: End-valued multi-period balance change
  reports (eg: `bal -MV`) have been reverted to show value-of-change,
  as in previous hledger versions, rather than change-of-value, for
  now. (#1353, #1428) (Stephen Morgan)

- balance: End-valued balance change reports now choose the same final
  valuation date and show consistent results whether single-period or
  multi-period. (#1424) (Stephen Morgan)

- balance: the `--drop` option now works with `csv` and `html` output.
  (#1456) (Ilya Konovalov)

- check: the `commodities` check, and `-s`/`--strict` mode, now ignore
  the "AUTO" internal pseudo-commodity. (#1419) (Ilya Konovalov)

- register: Then-valued multi-period register reports
  (eg: `register -M --value=then`) now calculate the correct values.
  (#1449) (Stephen Morgan)

- roi: now shows a better error message when required prices are
  missing. (#1446) (Dmitry Astapov)

- The no-symbol commodity's input number format can now be set by a
  `commodity` directive, like other commodities. (#1461)

# 1.20.2 2020-12-28

- help: Fix loss of capitalisation in part of the hledger-ui manual. 

- Fix the info manuals' node structure.

- Drop unused parsec dependency.

# 1.20.1 2020-12-15

- bal, bs, cf, is: In amount-sorted balance reports, equal-balance accounts 
  are now reliably sorted by name. (Simon Michael, Stephen Morgan)

- help: Fix the topic hierarchy in Info manuals.

# 1.20 2020-12-05

## general

- strict mode: with -s/--strict, hledger requires that
  all accounts and commodities are declared with directives.

- Reverted a stripAnsi change in 1.19.1 that caused a 3x slowdown of amount rendering
  in terminal reports. (#1350)

- Amount and table rendering has been improved, so that stripAnsi is no longer needed.
  This speeds up amount rendering in the terminal, speeding up some reports by 10% or more since 1.19.
  (Stephen Morgan)

- Amount eliding no longer displays corrupted ANSI codes (#1352, Stephen Morgan)

- Eliding of multicommodity amounts now makes better use of available space,
  avoiding unnecessary eliding (showing as many amounts as possible within
  32 characters). (Stephen Morgan)

- Command line help for --no-elide now mentions that it also disables eliding of
  multicommodity amounts.

- Query terms containing quotes (eg to match account names containing quotes)
  now work properly. (#1368, Stephen Morgan)

- cli, journal: Date range parsing is more robust, fixing failing/incorrect cases such as: (Stephen Morgan)

  - a hyphenated range with just years (`2017-2018`)
  - a hyphenated date with no day in a hyphenated range (`2017-07-2018`)
  - a dotted date with no day in a dotted range (`2017.07..2018.02`)
 
- Debug output is prettier (eg, in colour), using pretty-simple instead of pretty-show.

- csv, timedot, timeclock files now respect command line --alias options,
  like journal files.  (#859)

- Market price lookup for value reports is now more robust, fixing several bugs
  (and debug output is more informative).
  There has been a slight change in functionality: when chaining prices,
  we now prefer chains of all "forward" prices, even if longer, with chains
  involving reverse prices being the last resort.
  (#1402)

## commands

- add: number style (eg thousands separators) no longer disturbs the value
  that is offered as default. (#1378)

- bal: --invert now affects -S/--sort-amount, reversing the order. (#1283, #1379) (Stephen Morgan)

- bal: --budget reports no longer insert an extra space inside the brackets. (Stephen Morgan)

- bal: --budget reports now support CSV output (#1155)

- bal, is, bs --change: 
  Valued multiperiod balance change reports now show changes of value, 
  rather than the value of changes. (#1353, Stephen Morgan)

- bal: clearer debug output, following debug levels policy

- check: A new command which consolidating the various check-* commands.
  It runs the default, strict, or specified checks and produces
  no output and a zero exit code if all is well.

- check-dates: this command is deprecated and will be removed
  in next release; use "hledger check ordereddates" instead.

- check-dupes: this command is deprecated and will be removed
  in next release; use "hledger check uniqueleafnames" instead.

- import: The journal's commodity styles (declared or inferred) are now applied
  to imported amounts, overriding their original number format.

- roi: TWR now handles same-day pnl changes and cashflows,
  calculation failure messages have been improved, and
  the documentation includes more detail and examples.
  (#1398) (Dmitry Astapov)

## journal format

- The journal's commodity styles are now applied to forecasted transactions. (#1371)

- journal, csv: commodity style is now inferred from the first amount, as documented,
  not the last. This was "working wrongly" since hledger 1.12..

- A zero market price no longer causes "Ratio has zero denominator" error
  in valued reports. (#1373)

## csv format

- The new `decimal-mark` rule allows reliable number parsing
  when CSV numbers contain digit group marks (eg thousands separators).

- The CSV reader's verbose "assignment" debug output is now at level 9.


# 1.19.1 2020-09-07

- Fix alignment of coloured numbers (#1345, #1349, Stephen Morgan)

- Fix a regression in account type autodetection for accounts with
  capitalised names. (#1341)

- Allow megaparsec 9

# 1.19 2020-09-01

## general

- When parsing dates, the year is now required to have at least four
  digits. So eg we no longer accept `200/1/1` as a valid date, it
  would need to be written `0200/1/1`. This was done for.. reasons,
  and is experimental; let us know if it causes you trouble.

- The --color/--colour=WHEN command line option, support for the
  NO_COLOR environment variable, and smarter autodetection of colour
  terminals have been added (#1296)

- Command line options taking a numeric argument are now validated
  more carefully, preventing issues with unexpected negatives or Int
  overflow. (Stephen Morgan)

- In queries, you can now specify a quarter like `2020q1` or `q4`
  (the q is case-insensitive). (#1247, Henning Thieleman, Stephen Morgan)

- In report intervals, `fortnightly` has been added as a synonym for
  `biweekly`. (Stephen Morgan)

- -t and -l command line flags have been added as short forms of
  --tree and --flat (#1286)

- All reports displaying accounts now choose flat mode by default
  (Stephen Morgan)

- Reports now show at most 2 commodities of multicommodity amounts,
  unless the --no-elide flag is used. This helps keep them readable by
  default, since multicolumn, multicommodity balance reports otherwise
  tend to become very wide, especially in tree mode.

- Numbers with more than 255 decimal places, which we do not support,
  now give an error instead of silently misparsing. (#1326)

- Digit groups are now limited to at most 255 digits each. (#1326)

- Account aliases (on command line or in journal) containing a bad
  regular expression now give a more detailed error message.

- A tab character could get parsed as part of a commodity symbol, with
  confusing results. This no longer happens. (#1301, Dmitry Astapov)

- Debug output is now organised better by debug level.
  The levels are:

  0. normal command output only (no warnings)
  1. useful warnings & most common troubleshooting info (valuation, eg)
  2. common troubleshooting info, more detail
  3. report options selection
  4. report generation
  5. report generation, more detail
  6. input file reading
  7. input file reading, more detail
  8. command line parsing
  9. any other rarely needed or more in-depth info

- Added a missing lower bound for aeson, making cabal installs more
  reliable. (#1268)

- lib: parseAmountQueryTerm: allow whitespace around arg parts (#1312)
  Whitespace around the operator, sign, or number is now tolerated.

## commands

- account,bal,bs,cf,is: --drop now also works in tree mode (Stephen Morgan)

- add: fix an error in the command line help (arguments are inputs,
  not a query)

- aregister: a new command showing a transaction-oriented account
  register, like hledger-ui, hledger-web, or your bank statement. 
  Each line represents a whole transaction in one account, unlike
  the register command which shows individual postings possibly from
  multiple accounts. You might prefer aregister when reconciling
  real-world asset/liability accounts, and register when reviewing
  detailed revenues/expenses. (#1294)

- bal,bs,cf,is: boring parents are now elided by default in tabular
  balance reports too, like single-column reports. (Stephen Morgan)

- bal,bs,cf,is: monthly column headings are no longer elided to just
  the short month name, if multiple years are being displayed.

- bal --budget's column headings are now end dates rather than
  periods when appropriate (ie with --cumulative or --historical).

- bs,cf,is: -%/--no-total no longer forces --no-total (Stephen Morgan)

- bs,cf,is: --no-total now hides subtotals as well as the grand total
  (Stephen Morgan)

- codes: a new command for listing transaction codes

- print: a new `sql` output format has been added (Dmitry Astapov)

- roi: errors are now shown without a call stack

- tags: add --parsed flag, hide empties without --empty. With the
  --parsed flag, all tags or values are shown in the order they are
  parsed from the input data, including duplicates. With -E/--empty,
  any blank/empty values will also be shown, otherwise they are
  omitted.

## journal format

- account directives can specify a new `Cash` account type. This is a
  subtype of `Asset`, denoting accounts which should be displayed
  in `cashflow` reports. 
  
- The built-in regular expressions for choosing default account types
  have been tweaked, and documentation for account types has been
  improved.

## csv format

- Inferring the appropriate default field separator based on file
  extension (, for .csv, ; for .ssv, \t for .tsv) now works as
  documented.

- Conditional rule patterns can now be grouped with the `&` (AND) operator,
  allowing more powerful matching. (Michael Sanders)

- Invalid csv rules files now give clearer parse error messages.
  (Dmitry Astapov)

- "If tables", a compact bulk format for conditional rules, have been
  added. (Dmitry Astapov)

- csv conversion with a lot of conditional rules is now faster (Dmitry Astapov)


# 1.18.1 2020-06-21

- journal: document recursive wildcards

- by default, value reports work as in 1.17; to infer market prices from
  transactions, add the new --infer-value flag. (#1239, #1253)

- organise debug output better

- print: amounts in csv output now have commodity symbol, digit group
  separators and prices removed (Dmitry Astapov)


# 1.18 2020-06-07

## General

- The --forecast flag now takes an optional argument
  (--forecast=PERIODICEXPR), allowing periodic transactions to
  start/end on any date and to overlap recorded transactions.
  (#835, #1236) (Dmitry Astapov)

- An upper case file extension no longer confuses file format
  detection. (#1225)

- In the commands list, redundant source scripts are now hidden
  properly when a corresponding .com/.exe file exists. (#1225)

- We now show `..` instead of `-` to indicate date ranges, eg in
  report titles, to stand out more from hyphenated dates. 
  (Stephen Morgan)
  
- Period expressions (eg in -p, date:, and periodic rules) now accept
  `to`, `until`, `-`, or `..` as synonyms. (Stephen Morgan)

- When parsing amounts, whitespace between sign and number is now allowed.

- A clearer error message is shown on encountering a malformed regular
  expression.

## commands

- commands allowing different output formats now list their supported
  formats accurately in --help (#689)

- commands allowing JSON output now actually produce JSON (#689)

- bal, bs: show .. (not ,,) in report titles, like other reports

## journal format

- We now also infer market prices from transactions, like Ledger.
  See https://hledger.org/hledger.html#market-prices (#1239). 
  
  Upgrade note: this means value reports (-V, -X etc.) can give
  different output compared to hledger 1.17. If needed, you can
  prevent this by adding a P directive declaring the old price, on or
  after the date of the transaction causing the issue.

- The include directive now accepts a file format prefix, like the
  -f/--file option. This works with glob patterns too, applying the
  prefix to each path. This can be useful when included files don't
  have the standard file extension, eg:

      include timedot:2020*.md

- We now accept (and ignore) Ledger-style lot dates
  (`[DATE]`) and four lot price forms (`{PRICE}`, `{{PRICE}}`,
  `{=PRICE}`, `{{=PRICE}}`), anywhere after the posting amount but
  before any balance assertion.

- We now accept Ledger-style parenthesised "virtual posting
  costs" (`(@)`, `(@@)`). In hledger these are equivalent to the
  unparenthesised form.

- The unbalanced transaction error message is clearer, especially when
  postings all have the same sign, and is split into multiple lines
  for readability.

## csv format

- You can now generate up to 99 postings in a transaction. (Vladimir Sorokin)

- You can now generate postings with an explicit 0 amount. (#1112)

- For each posting, when both numbered and unnumbered amount
  assignments are active (eg: both `amount` and `amount1`), we ignore
  the unnumbered ones. This makes it easier to override old `amount`
  rules.
  
- Fix a 1.17.1 regression involving amount-in/amount-out. (#1226)

- Assigning too many non-zero or zero values to a posting amount now
  gives a clearer error. (#1226)


# 1.17.1.1 2020-03-19

- update bounds after some belated hledger-* version bumps

# 1.17.1 2020-03-19

- csv: amount1 no longer forces a second posting or second posting amount.
  The "special handling for pre 1.17 rules" should now be less
  noticeable. amount1/amount2 no longer force a second posting or
  explicit amounts on both postings. (Only amount/amount-in/amount-out
  do that.) Error messages and handling of corner cases may be more
  robust, also.

- journal: a commodity directive without decimal mark now gives a more
  verbose error message with examples

- journal: inclusive balance assignments now work (#1207)

- require newer Decimal, math-functions libs to ensure consistent
  rounding behaviour, even when built with old GHCs/snapshots. 
  hledger uses banker's rounding (rounds to nearest even number, eg
  0.5 displayed with zero decimal places is "0").

# 1.17 2020-03-01

## General

- hledger's default date format is now YYYY-MM-DD (ISO-8601 dates).
  (Brian Wignall, Jakob Schöttl, Simon Michael)

- Drop the file format auto-detection feature.

  For a long time hledger has auto-detected the file format when it's
  not known, eg when reading from a file with unusual extension (like
  .dat or .txt), or from standard input (-f-), or when using the
  include directive (which currently ignores file extensions).  This
  was done by trying all readers until one succeeded.  This worked
  well in practice. But recent changes to timedot format have made
  this kind of auto-detection unreliable. (timedot and journal formats
  overlap).

  For predictability and to minimise confusion, hledger will no longer
  guess; when there's no file extension or reader prefix available, it
  always assumes journal format.

  To specify one of the other formats, you must use its standard file
  extension (`.timeclock`, `.timedot`, `.csv`, `.ssv`, `.tsv`), or a
  reader prefix (`-f csv:foo.txt`, `-f timedot:-`).

  Experimental, feedback welcome.

- Fix extra $ symbol (Mateus Furquim)

- --output-format now rejects invalid formats

- Numbers in JSON output now provide a floating point Number
  representation as well as our native Decimal object representation,
  since the later can sometimes contain 255-digit integers. The
  floating point numbers can have up to 10 decimal digits (and an
  unbounded number of integer digits.)
  Experimental, suggestions needed. (#1195)

- Fix finding latest date in queryEndDate Or queries and simplify
  date comparison code. (Stephen Morgan)

- Fix issue 457. (Jacek Generowicz)
  Issue #457 pointed out that commands such as

      hledger ui 'amt:>200'

  failed. This was because the process of dispatching from `hledger ui`
  to `hledger-ui` (note addition of `-`) lost the quotes around
  `amt:>20` and the `>` character was interpreted as a shell redirection
  operator, rather than as part of the argument.

  The machinery for quoting or escaping arguments which contain
  characters which require quoting or escaping (thus far whitespace and
  quotes) already existed. This solution simply adds shell stdio
  redirection characters to this set.

## commands

- add: you can use `<` to undo and redo previous inputs (Gaith Hallak)

- bs, cf, is, bal, print, reg: support json output

- bs, cf, is: fix excess subreport columns in csv output

- bs, cf, is, bal: fix an issue with border intersections in
  --pretty-tables output. (Eric Mertens)

- close: fix a rounding bug that could generate unbalanced transactions. (#1164)

- close: hide cost prices by default, show them with --show-costs.
  close no longer preserves costs (transaction prices) unless you ask
  it to, since that can generate huge entries when there are many
  foreign currency/investment transactions. (#1165)

- close: equity amounts are omitted by default, for simpler entries;
  -x/--explicit shows them (usually causing more postings). (#1165)

- close: --interleaved generates equity postings alongside each closed
  account, making troubleshooting easier.

- close: "equity:opening/closing balances" is now the default
  closing and opening account.

- close: --close-desc/--open-desc customise the closing/opening
  transaction descriptions. (#1165)

- close: some --open*/--close* flags have been simplified for memorability:

  --closing -> --close
  --opening -> --open
  --close-to -> --close-acct
  --open-from -> --open-acct

  The old flags are accepted as hidden aliases, and deprecated. (#1165)

- print, register: a new valuation type, --value=then, shows the
  market value at each posting's date.

- print: -V/-X/--value now imply -x/--explicit, as -B/--cost does.
  This avoids a bug where print -V of a transaction with an implicit
  commodity conversion would convert only some of its postings to value.

## journal format

- The include directive no longer tries all readers.  It now picks
  just one, based on the included file's extension, defaulting to
  journal.  (It doesn't yet handle a reader prefix.)

- The default commodity (D) directive now limits display precision too. (#1187)
  D directives are now fully equivalent to commodity directives for
  setting a commodity's display style. (Previously it couldn't limit
  the number of decimal places.)  When both kinds of directive exist,
  commodity directives take precedence.  When there are multiple D
  directives in the journal, only the last one affects display style.

## csv format

- Conditional blocks can now match single fields. \o/

- The experimental --separator command line option has been dropped,
  replaced a new `separator` directive in CSV rule files. (Aleksandar Dimitrov)
  Also the `.tsv` and `.ssv` file extensions are now recognised,
  and set the default `separator` to TAB and semicolon respectively.
  (#1179)

- Allow manual assignment of the "expenses:unknown" account name. (#1192)

- CSV rule keywords are now case insensitive. (Aleksandar Dimitrov)

## timeclock format

- Misc. fixes making parsing more robust. (Jakob Schöttl)

## timedot format

- More support for org mode: org headlines can now be used for date
  lines and timelog items (the stars are ignored). Also, any org
  headlines before the first date line are ignored.

- You can now write a description after a date, which will be used in
  all of that day's transactions.


# 1.16.2 2020-01-14

- add support for megaparsec 8 (#1175)
 
- close: mention --close-to/--open-from in docs
 
# 1.16.1 2019-12-03

- Drop unnecessary mtl-compat dependency

- Fix building with GHC 8.0, 8.2

# 1.16 2019-12-01

## General

- add support for GHC 8.8, base-compat 0.11 (#1090)

- drop support for GHC 7.10

- The benchmark suite has been disabled.

- The --anon flag now also anonymises transaction codes and account
  names declared with account directives. (Mykola Orliuk) (#901)

## commands

- balance/bs/cf/is: balance commands now support the -%/--percent flag
  to show amounts as percentages of the column's total. (Michael Kainer)

  If there are multiple commodities involved in a report hledger bails
  with an error message. This can be avoided by using -B/--cost. Also note
  that if one uses -% with the balance command the chances are high that
  all numbers are 0. This is due to the fact that by default balance sums
  up to zero. If one wants to use -% in a meaningful way with balance one
  has to add a query.

  In order to keep the implementation as simple as possible --tree has no
  influence over how the percentages are calculated, i.e., the percentages
  always represent the fraction of the columns total. If one wants to know
  the percentages relative to a parent account, one has to use a query to
  narrow down the accounts.

- balance: --budget no longer errors when there is neither budget nor
  transactions in the report period (Dmitry Astapov)

- balance: --budget has improved debug output (shows budget txns)
  (Dmitry Astapov)

- check-dates: now sets the exit status code (Amitai Burstein)

- close: no longer strips zeroes after the decimal mark, and preserves
  parseable output (#1137)

- close: the --close-to, --open-from options allow closing/opening
  account names to be chosen

- import: create the journal if missing, like the add command
  Streamlines import/migration instructions.

- import: --catchup marks all transactions imported, without importing

- import: more informative output: mention the input files, also show
  a message when nothing was imported

- prices: show price amounts with proper display style; always show
  full precision

- roi: don't give an error with empty input data (Dmitry Astapov)

- tests: unit tests are now run by tasty, and show coloured output by default (#1090).
  Test running options have changed, see the command help. 
  Some unit tests have been collapsed, so the reported test count has
  dropped a little.

## journal format

- Fixed: wrong dates generated by certain periodic transaction rules,
  eg "~ every 12 months from 2019/04". (Dmitry Astapov) (#1085)

## csv format

CSV conversion is now more powerful (#1095, Dmitry Astapov, Simon Michael):

- A variable number of postings can be generated, from zero to nine. (#627, #1095)

- In conditional blocks, `skip` can be used to skip one or more
  records after a pattern match, or the new `end` rule can be used to
  skip all remaining records. (#1076)

- The new `balance-type` CSV rule controls which kind of balance
  assertions are generated (=, ==, =*, ==*)

- Postings with balance assignments can be generated. (#1000)

- Both the amount-in/amount-out fields having a non-empty value is now
  accepted, as long as one of them is zero. (#570)

- Line feeds/carriage returns in (quoted) CSV values are now converted
  to spaces during conversion. (#416, #841)

- Field assignments can now unset a field (eg a posting can be
  suppressed by assigning no value to its account).

- CSV records with varying lengths are now allowed; short records will
  be padded with empty fields as needed. This allows us to handle eg
  exported Google spreadsheets, where trailing empty fields are omitted.

- Journals generated from CSV are now finalised and checked like
  ordinary journals (#1000). So invalid transactions generated from
  CSV will be rejected, amount styles will be standardised etc.

- Fixed: we no longer add an extra (third) space between description and comment.

- Fixed: whitespace on the line after an if block no longer causes misparsing. (#1120)

- Fixed: an empty field assignment no longer consumes the next line. (#1001)

- Fixed: interpolation of field names containing punctuation now works.

- Docs have been rewritten and clarified.

Migration notes:

- When `print`ing from CSV, there is now one less space between
  transaction descriptions and comments, which may generate noisy
  diffs if you are comparing old and new reports. diff -w
  (--ignore-all-space) will filter these out.

- CSV rules now give you more freedom to generate any journal
  entries you want, including malformed or unbalanced ones. 
  The csv reader now checks the journal after conversion,
  so it will report any problems with the generated entries.

- Balance assertions generated from CSV are not checked, currently.
  This is appropriate when you are downloading partial CSV data to
  be merged into your main journal. If you do need to check balance
  assertions right away, you can pipe through hledger again:

      $ hledger -f a.csv print | hledger -f- print



# 1.15.2 2019-09-05

- -V and -X now respect a report end date (set with -e or -p or date:)
  when choosing the valuation date (which determines the market prices
  used). This is how -V works in hledger 1.14 and Ledger, and it means
  that -V isn't exactly equivalent to either --value=end or
  --value=now. Possibly some other corner cases in valuation have been
  fixed as well. "Effect of --value on reports" in the hledger manual
  has been updated and is more accurate.

# 1.15.1 2019-09-02

- add commodities, descriptions, diff, notes, payees commands to manual

# 1.15 2019-09-01

## General

- There is a new valuation option `--value=TYPE[,COMM]`, with
  backwards-compatible `-B/--cost`, `-V/--market`, `-X/--exchange=COMM`
  variants. These provide control over valuation date (#329), and
  inference of indirect market prices (similar to Ledger's -X) (#131).
  Experimental.
  
- Market valuation (-V/-X/--value) is now much faster (#999):

      +-------------------------------------------++--------------+--------------+
      |                                           || hledger-1.14 | hledger-1.15 |
      +===========================================++==============+==============+
      | -f examples/10000x1000x10.journal bal -Y  ||         2.43 |         2.44 |
      | -f examples/10000x1000x10.journal bal -YV ||        44.91 |         6.48 |
      | -f examples/10000x1000x10.journal reg -Y  ||         4.60 |         4.15 |
      | -f examples/10000x1000x10.journal reg -YV ||        61.09 |         7.21 |
      +-------------------------------------------++--------------+--------------+

- How date options like `-M` and `-p` interact has been updated and clarified.
  (Jakob Schöttl) (#1008, #1009, #1011)

- Restore `--aux-date` and `--effective` as `--date2` aliases (#1034).
  These Ledger-ish spellings were dropped over the years, to improve
  `--help`'s layout. Now we support them again, as semi-hidden flags
  (`--help` doesn't list them, but they are mentioned in `--date2`'s help).

## commands

- add, web: on Windows, trying to add transactions to a file path
  containing trailing periods (eg `hledger add -f  Documents.\.hledger.journal`) 
  now gives an error, since this could cause data loss otherwise (#1056).
  This affects the add command and hledger-web's add form.

- bal: --budget: don't always convert to cost.

- bal: --budget: don't show a percentage when budgeted and actual
  amounts are in different commodities.

- bal/bs/bse: `-H/--historical` or `--cumulative` now disables `-T/--row-total` (#329).
  Multiperiod balance reports which show end balances (eg, `bal -MH` or `bs -M`)
  no longer show a Totals column, since summing end balances generally
  doesn't make sense.

- bs: show end date(s) in title, not transactions date span (#1078)
  Compound balance reports showing ending balances (eg balancesheet),
  now show the ending date (single column) or range of ending
  dates (multi column) in their title. ,, (double comma) is used
  rather than - (hyphen) to suggest a sequence of discrete dates
  rather than a continuous span.

- close: preserve transaction prices (costs) accurately (#1035).
  The generated closing/opening transactions were collapsing/misreporting
  the costs in balances involving multiple costs.
  Now, each separately-priced amount gets its own posting.
  (And only the last of these (for each commodity) gets a balance assertion.)
  Also the equity posting's amount is now always shown explicitly, 
  which in multicommodity situations means that multiple equity postings are shown. 
  The upshot is that a balance -B report will be unchanged after
  the closing & opening transactions generated by the close command.

- descriptions, payees, notes commands added (Caleb Maclennan)

- diff: Gabriel Ebner's hledger-diff is now a built in command,
  and https://github.com/gebner/hledger-diff is deprecated.

- help: don't require a journal file

- print: now also canonicalises the display style of balance assertion amounts (#1042)

- reg: show negative amounts in red, like balance and Ledger

- reg: fix `--average`, broken since 1.12 (#1003)

- stats: show count of market prices (P directives), and the commodities covered

- tags: add --values flag to list tag values.

- tags: now runs much faster when there many tags

## journal format

- Transactions and postings generated/modified by periodic transaction
  rules and/or transaction modifier rules are now marked with tags
  (`generated-transaction`, `generated-posting`, `modified`) for
  easier troubleshooting and filtering.

## csv format

- When interpolating CSV values, outer whitespace is now stripped.
  This removes a potential snag in amount field assignments (#1051),
  and hopefully is harmless and acceptable otherwise.

- We no longer add inter-field spaces in CSV error messages.
  Some CSV errors would show the problem record, eg:

      2000-01-01,a,"1"
  
  with extra spaces added, eg:

      the CSV record is: "2000-01-01", "a", "1"

  which was inaccurate and not valid RFC-4180 CSV format.

- CSV parse errors are human-readable again (broken since 1.11) (#1038)

- CSV rules now allow the amount to be unassigned, if there is an
  assignment to "balance" (generating a balance assignment in this
  case). (#1000)


# 1.14.2 2019-03-20

- require easytest <0.3 to fix build issue

- fix some CSV parse errors which weren't in human readable format

# 1.14.1 2019-03-01

- fix missing Commodities.txt build error

# 1.14 2019-03-01

- journal: subaccount-including balance assertions have been
  added, with syntax =* and ==* (experimental) (#290)

- new commodities command lists commodity symbols

- new --invert option flips sign of amounts in reports

# 1.13.2 (2019/02/04)

- print, register: restore the accidentally dropped -o, -O flags (#967)

# 1.13.1 (2019/02/02)

- stop depending on here to avoid haskell-src-meta/stackage blockage.

# 1.13 (2019/02/01)

- cli: reorganised commands list. Addons now have a + prefix.

- cli: the command line help and manual section for all hledger's
  commands are now consistent, and generated from the same source.

- cli: comprehensive bash completion support is now provided (in
  shell-completion/). See how-to in the Cookbook. (Jakob Schöttl)

- balance --budget: budget amounts now aggregate hierarchically, like
  account balances. Unbudgeted accounts can be shown with -E/--empty
  (along with zero-balance accounts), and the --show-budgeted flag has
  been dropped.  (Dmitry Astapov)

- balance: new --transpose flag switches the rows and columns of
  tabular balance reports (in txt and csv output formats). (Dmitry
  Astapov)

- close: generated balance assertions now have exact amounts with all
  decimal digits, ignoring display precision. Also, balance assertion
  amounts will no longer contain prices. (#941, #824, #958)

- files: now shows up in the commands list

- import: be silent when there's nothing to import

- roi: percentages smaller than 0.01% are displayed as zero (Dmitry
  Astapov)

- stats, ui: correct file order is preserved when using --auto (#949)

- journal: account directive: the account name can now be followed by
  a comment on the same line

- journal: account directive: account types for the bs/bse/cf/is
  commands can now be set with a `type:` tag, whose value is `Asset`,
  `Liability`, `Equity`, `Revenue`, `Expense`, `A`, `L`, `E`, `R` or
  `X` (case-insensitive).  The previous syntax (`account assets A`) is
  now deprecated.

- journal: account directive: account sort codes like `account 1000`
  (introduced in 1.9, deprecated in 1.11) are no longer supported.

- journal: transaction modifiers (auto postings) can affect periodic
  transactions (--auto can add postings to transactions generated with
  --forecast). (Dmitry Astapov)

- journal: balance assertion errors now show exact amounts with all
  decimal digits.  Previously it was possible, in case of a commodity
  directive limiting the display precision, to have a balance
  assertion error with asserted and actual amounts looking the
  same. (#941)

- journal: fixed a periodic transaction parsing failure (#942) (Dmitry
  Astapov)

# 1.12.1 (2018/12/03)

-   roi: use math-functions lib instead of statistics,
    be more stackage nightly compatible

# 1.12 (2018/12/02)

-   install script: ensure a new-enough version of stack; more informative output

-   build with GHC 8.6/base-4.12 (Peter Simons)

-   add required upper bound for statistics (Samuel May)

-   --anon anonymises more thoroughly (including linked original postings) (Moritz Kiefer)

-   unbalanced transaction errors now include location info (Mykola Orliuk)

-   accounts command: --drop also affects the default flat output, without needing an explicit --flat flag

-   accounts command: the --codes flag has been dropped

-   accounts command: filtering by non-account-name queries now works

-   add command: fix transaction rendering regression during data entry and in journal file

-   balance command: fix wrongful eliding of zero-balance parent accounts in tree mode (Dmitry Astapov)

-   journal format, bs/bse/cf/is commands: account directives can declare account types (#877)
    Previously you had to use one of the standard english account names
    (assets, liabilities..) for top-level accounts, if you wanted them to
    appear in the right place in the balancesheet, balancesheetequity,
    cashflow or incomestatement reports.

    Now you can use your preferred account names, and use account directives
    to declare which accounting class (Asset, Liability, Equity, Revenue or
    eXpense) an account (and its subaccounts) belongs to, by writing one of
    the letters A, L, E, R, X after the account name, after two or more
    spaces. This syntax may change (see issue). Experimental.

    Currently we allow unlimited account type declarations anywhere in the
    account tree. So you could declare a liability account somewhere under
    assets, and maybe a revenue account under that, and another asset account
    even further down. In such cases you start to see oddities like accounts
    appearing in multiple places in a tree-mode report. I have left it this
    way for now in case it helps with, eg, modelling contra accounts, or
    combining multiple files each with their own account type
    declarations. (In that scenario, if we only allowed type declarations on
    top-level accounts, or only allowed a single account of each type,
    complications seem likely.)

-   journal format: periodic transaction rules now require a double space separator.
    In periodic transaction rules which specify a transaction description or
    same-line transaction comment, this must be separated from the period
    expression by two or more spaces, to prevent ambiguous parsing. Eg
    this will parse correctly as "monthly" thanks to the double space:

        ~ monthly  In 2020 we'll end this monthly transaction.

-   journal format: exact/complete balance assertions (Samuel May).
    A stronger kind of balance assertion, written with a double equals sign,
    asserts an account's complete account balance, not just the balance in
    one commodity. (But only if it is a single-commodity balance, for now.)
    Eg:

        1/1
          (a)  A 1
          (a)  B 1
          (a)  0   =  A 1   ; commodity A balance assertion, succeeds
          (a)  0   == A 1   ; complete balance assertion, fails

-   journal format: account directives now allow whitespace or a comment after the account name

-   journal format: using \~ for home directory in include directives now works (#896) (Mykola Orliuk)

-   journal format: prevent misleading parse error messages with cyclic include directives (#853) (Alex Chen)

-   journal format: transaction modifier multipliers handle total-priced amounts correctly (#928).
    Multipliers (*N) in transaction modifier rules did not multiply
    total-priced amounts properly. Now the total prices are also multiplied,
    keeping the transaction balanced.

-   journal format: do amount inference/balance assignments/assertions before transaction modifiers (#893, #908) (Jesse Rosenthal)
    Previously, transaction modifier (auto postings) rules were applied
    before missing amounts were inferred. This meant amount multipliers could
    generate too many missing-amount postings, making the transaction
    unbalanceable (#893).

    Now, missing amount inference (and balance assignments, and balance
    assertions, which are interdependent) are done earlier, before
    transaction modifier rules are applied (#900, #903).

    Also, we now disallow the combination of balance assignments and
    transaction modifier rules which both affect the same account, which
    could otherwise cause confusing balance assertion failures (#912).
    (Because assignments now generate amounts to satisfy balance assertions
    before transaction modifier rules are applied (#908).)

-   journal format: periodic transaction rules are now aware of Y default year directives. (#892)
    Ie when a default year Y is in effect, they resolve partial or relative
    dates using Y/1/1 as the reference date, rather than today's date.

# 1.11.1 (2018/10/06)

-   fix wrong transaction rendering in balance assertion errors and when
    using the add command

# 1.11 (2018/9/30)

-   The default display order of accounts is now influenced by
    the order of account directives. Accounts declared by account
    directives are displayed first (top-most), in declaration order,
    followed by undeclared accounts in alphabetical order. Numeric
    account codes are no longer used, and are ignored and considered
    deprecated.

    So if your accounts are displaying in a weird order after upgrading,
    and you want them alphabetical like before, just sort your account
    directives alphabetically.

-   Account sorting (by name, by declaration, by amount) is now more
    robust and supported consistently by all commands (accounts,
    balance, bs..) in all modes (tree & flat, tabular & non-tabular).

-   close: new --opening/--closing flags to print only the opening or
    closing transaction

-   files: a new command to list included files

-   prices: query arguments are now supported. Prices can be filtered by
    date, and postings providing transaction prices can also be filtered.

-   rewrite: help clarifies relation to print --auto (#745)

-   roi: a new command to compute return on investment, based on hledger-irr

-   test: has more verbose output, more informative failure messages,
    and no longer tries to read the journal

-   csv: We use a more robust CSV lib (cassava) and now support
    non-comma separators, eg --separator ';' (experimental, this flag
    will probably become a CSV rule) (#829)

-   csv: interpolated field names in values are now properly case insensitive, so
    this works:

        fields  ...,Transaction_Date,...
        date %Transaction_Date

-   journal: D (default commodity) directives no longer break multiplier
    amounts in transaction modifiers (AKA automated postings) (#860)

-   journal: "Automated Postings" have been renamed to "Transaction Modifiers".

-   journal: transaction comments in transaction modifier rules are now parsed correctly. (#745)

-   journal: when include files form a cycle, we give an error instead
    of hanging.

-   upper-case day/month names in period expressions no longer give an error (#847, #852)

# 1.10 (2018/6/30)

-   journal: many parse error messages have become more informative, and
    some now show the source line and error location.

-   journal: ;tag: is no longer parsed as a tag named ";tag" (#655)

-   journal: transaction price amounts having their own price amounts is
    now a parse error

-   journal: amounts with space as digit group separator and trailing whitespace
    now parse correctly (#780)

-   journal: in amounts containing digits and a single space, the space
    is now interpreted as a digit group separator, not a decimal separator (#749)

-   journal: in commodity/format/D directives, the amount must now include a decimal separator.

    When more precise control is needed over number parsing, our
    recommended solution is commodity directives. Commodity directives
    that don't specify the decimal separator leave things ambiguous,
    increasing the chance of misparsing numbers. In some cases it could
    cause amounts with a decimal point to be parsed as if with a digit
    group separator, so 1.234 became 1234.

    It seems the simple and really only way to do this reliably is to require
    an explicit decimal point character. Most folks probably do this already.
    Unfortunately, it makes another potential incompatibility with ledger and
    beancount journals. But the error message will be clear and easy to
    work around.

-   journal: directives currently have diverse and somewhat tricky
    semantics, especially with multiple files. The manual now describes
    their behaviour precisely.

-   journal: `alias` and `apply account` directives now affect `account` directives (#825)

-   journal: periodic transactions can now have all the usual transaction fields
    (status mark, code, description, comment), for generating more expressive
    forecast transactions.

-   journal: forecast transactions now have the generating period
    expression attached as a tag named "recur".

-   journal: periodic transactions now start on the first instance of the
    recurring date, rather than the day after the last regular transaction (#750)

-   journal: periodic transaction rules now allow period expressions relative to today's date

-   csv: amount-in/amount-out errors are more detailed

-   balance: --drop is now ignored when not in flat mode,
    rather than producing a corrupted report (#754)

-   budget: --drop now preserves the <unbudgeted> top-level account in --budget reports

-   register: in CSV output, the code field is now included (#746)

-   smart dates now allow the YYYYMM format, and are better documented

-   use hledger-lib 1.10

# 1.9.1 (2018/4/30)

-   use hledger-lib 1.9.1

-   budget (balance --budget): monthly columns are displayed in the
    proper order. This fixes a regression in 1.9.

-   budget: budgets can be built from periodic transactions with
    different intervals again. In 1.9, budgets were restricted to a
    single interval, but this was a mistake. This restores the 1.5
    behaviour.

-   budget: budget reports are more intuitive and much less likely to
    produce no output.

-   budget: when no report interval is specified, a budget report for
    the whole journal period is shown.

-   budget: periodic transactions and the requested report period can
    each have their own start/end dates, and the resulting report will
    span the union of those periods, showing zeroes where data is
    missing.

-   budget: total row and total/average columns are now calculated correctly

-   budget: actual, percentage, and goal amounts are now aligned in
    columns for better readability (usually, unless numbers get huge).

-   budget: combining --budget and --sort-amount is not yet supported
    and now gives an error.

-   csv: handle "-%amount" in a rule when the CSV amount is parenthesised (#736)

-   journal: automated postings are now generated early, before journal finalisation,
    so they are present for amount inference, transaction balancing, and balance assertions
    (#729)

-   journal: automated postings are now inserted right after the posting that triggered them
    (#729)

-   cli: command-line account aliases are now applied early, before journal finalisation,
    so they are equivalent to alias directives in the journal (#730)

-   journal: inferred amounts now have the appropriate standard amount style applied
    (setting the precision correctly, eg). (#737)

-   journal: when checking for balanced transactions, amount styles declared with
    commodity directives are also used (previously only inferred amount styles were).

# 1.9 (2018/3/31)

-   support ghc 8.4, latest deps

-   journal: account directives can define a numeric account code to
    customize sorting. bal/bs/cf/is will sort accounts by account code,
    if any, then account name.

-   journal: support scientific number notation (#704, #706)

-   csv: reading a CSV file containing no records is no longer an error

-   cli: when the system text encoding is UTF-8, ignore any UTF-8 BOM
    prefix found when reading files. (Paypal's new CSV has this BOM
    prefix, causing a confusing parse error.)

-   cli: tabular reports no longer have a trailing blank line added.
    (This allows omitting the ">=0" delimiters in our functional tests,
    making them easier to read and maintain.)

-   acc: the accounts command now has --declared and --used flags

-   bal: the --invert flag flips all signs

-   bal: --drop now works with CSV output

-   bal/bs/bse/cf/is: show overall report span in title

-   bal/bs/bse/cf/is: show short month names as headings in monthly reports

-   bal/bs/bse/cf/is: these commands can now generate HTML output

-   bal/bs/is/cf: drop short name and indent fields from multicolumn CSV

-   bs/bse/cf/is: these, the "financial statement" commands, now show
    normal income, liability and equity balances as positive numbers.
    Negative numbers now indicate a contra-balance (eg an overdrawn
    checking account), a net loss, or a negative net worth. This makes
    these reports more like conventional financial statements, and easier
    to read and share with others. (Other commands, like balance, have not
    changed.) (experimental)

-   bs/cf/is: always show a tabular report, even with no report
    interval. Previously you would get a simple borderless report like
    the original balance command. Less code, fewer bugs.

-   bs/bse/cf/is: in CSV output, don't repeat the headings row for each subreport

-   budget: warn that CSV output with bal --budget is unimplemented

-   budget: bal --budget shows budget goals even with no or zero actual amounts.
    Makes budget reports more intuitive, at the cost of a temporary hack
    which may misorder columns in some cases (if actual and budget
    activity occur in a different range of columns).

-   budget: --budget uses only periodic txns with the selected interval.\
    Budgets with different interval, eg a daily and weekly budget, are independent.

-   budget: show mostly fixed-width columns for readability

-   budget: fix bug where a budget report could include budget goals
    ending on the day before the report start date (splitSpan issue)

-   close: the equity command has been renamed to close. It now ignores
    any begin date (it always closes historical end balances). It also
    ignores --date2.

# 1.5 (2017/12/31)

-   --auto adds Ledger-style automated postings to transactions (Dmitry Astapov, Mykola Orliuk)

-   --forecast generates Ledger-style periodic transactions in the future (Dmitry Astapov, Mykola Orliuk)

-   -V/--value uses today's market prices by default, not those of last transaction date. #683, #648

-   add: suggest implied (parent) and declared (by account directives) account names also

-   bal: --budget shows performance compared to budget goals defined
    with periodic transactions. Accounts with budget goals are
    displayed folded (depth-clipped) at a depth matching the budget
    specification. Unbudgeted accounts are hidden, or with
    --show-unbudgeted, shown at their usual depth. (Dmitry Astapov)

-   import: the output of --dry-run is now valid journal format

-   print: -B shows converted amounts again, as in 1.1, even without
    -x. #551 (Mykola Orliuk, Simon Michael)

-   tag: the first argument now filters tag names, additional arguments
    filter transactions (#261)

-   remove upper bounds on all but hledger* and base (experimental)

# 1.4 (2017/9/30)

-   cli: a @FILE argument reads flags & args from FILE, one per line

-   cli: reorganized commands list, added some new command aliases:

    -   accounts: a
    -   balance: b
    -   print: p, txns
    -   register: r

-   cli: accept -NUM as a shortcut for --depth=NUM (eg: -2)

-   cli: improve command-line help for --date2 (#604)

-   cli: make --help and -h the same, drop --man and --info for now (#579)

-   help: offers multiple formats, accepts topic substrings.
    The separate info/man commands have been dropped. help now
    chooses an appropriate documentation format as follows:

    -   it uses info if available,
    -   otherwise man if available,
    -   otherwise $PAGER if defined,
    -   otherwise less if available,
    -   otherwise it prints on stdout
    -   (and it always prints on stdout when piped).

    You can override this with the `--info`/`--man`/`--pager`/`--cat` flags.
    (#579)

-   bal/bs/cf/is: --sort-amount/-S sorts by largest amount instead of
    account name

-   bs/cf/is: support --output-file and --output-format=txt\|csv
    The CSV output should be reasonably ok for dragging into a
    spreadsheet and reformatting.

-   bal/bs/cf/is: consistent double space between columns, consistent
    single final blank line. Previously, amounts wider than the column
    headings would be separated by only a single space.

-   bs/is: don't let an empty subreport disable the grand totals (fixes #588)

-   cf: exclude asset accounts with ":fixed" in their name (Christian G. Warden, Simon Michael, #584)

-   new balancesheetequity command: like balancesheet but also shows
    equity accounts (Nicholas Niro)

-   new import command: adds new transactions seen in one or more input
    files to the main journal file

-   print: --new shows only transactions added since last time
    (saves state in .latest.JOURNALFILE file)

-   new tags command: lists tags in matched transactions

-   most addons formerly shipped in bin/ are now builtin commands. These
    include: check-dates, check-dupes, equity, prices, print-unique,
    register-match, rewrite.

-   refactor: new Commands module and subdirectory.
    Builtin commands are now gathered more tightly in a single module,
    Hledger.Cli.Commands, facilitating change. The legacy "convert"
    command has been dropped.

-   refactor: BalanceView -> CompoundBalanceCommand

-   deps: drop support for directory < 1.2

-   deps: allow ansi-terminal 0.7

-   deps: drop oldtime flag, require time 1.5+

-   deps: simplify shakespeare bounds

-   deps: remove ghc < 7.6 support

# 1.3.1 (2017/8/25)

-   bs/is: don't let an empty subreport disable the grand totals (#588)

-   allow megaparsec 6 (#594)

-   allow megaparsec-6.1 (Hans-Peter Deifel)

-   restore upper bounds on hledger packages

# 1.3 (2017/6/30)

The "uncleared" transaction/posting status, and associated UI flags
and keys, have been renamed to "unmarked" to remove ambiguity and
confusion. This means that we have dropped the `--uncleared` flag,
and our `-U` flag now matches only unmarked things and not pending
ones. See the issue and linked mail list discussion for more
background. (#564)

Also the -P short flag has been added for --pending, and the -U/-P/-C
flags can be combined.

bs/is: fix "Ratio has zero denominator" error (#535)

bs/is/cf: fix --flat (#552) (Justin Le, Simon Michael)

bal/bs/is/cf: show negative amounts in red (Simon Michael, Justin Le).
These commands now shows negative amounts in red, when hledger detects
that ANSI codes are supported, (ie when TERM is not "dumb" and stdout
is not being redirected or piped).

print: show pending mark on postings (fixes #563).
A pending mark on postings is now displayed, just like a cleared mark.
Also there will now be a space between the mark and account name.

print: amounts are now better aligned, eg when there are posting
status marks or virtual postings

# 1.2 (2017/3/31)

## CLI

"hledger" and "hledger -h" now print a better organised commands list
and general usage message respectively (#297).

The common reporting flags can now be used anywhere on the command line.

Fixed deduplication of addons in commands list.

Fixed ugly stack traces in command line parse error messages.

The -V/--value flag is now a global report flag, so it works with
balance, print, register, balancesheet, incomestatement, cashflow,
etc. (Justin Le)

The `--pivot` global reporting option replaces all account names with
the value of some other field or tag. It has been improved, eg:

-   we don't add the field/tag name name as a prefix
-   when pivoting on a tag, if the tag is missing we show a blank
    (rather than showing mixed tag values and account names)
-   a pipe character delimiter may be used in descriptions to get a more accurate
    and useful payee report (`hledger balance --pivot payee`)

options cleanups

## Addons

Easier installation:
move add-ons and example scripts to bin/,
convert to stack scripts,
add a build script to install all deps,
add some functional tests,
test add-ons with Travis CI,
add installation docs to download page.

Improved docs:
all addons now contain their own documentation. Most of them (all but
hledger-budget) use a new reduced-boilerplate declaration format
and can show short (-h) and long (--help) command line help.
(Long help is declared with pre and postambles to the generated
options help, short help is that truncated at the start of the hledger
common flags.)

`hledger` now shows a cleaner list of addon commands, showing only the
compiled version of an addon when both source and compiled versions
are in $PATH. (Addons with .exe extension or no extension are
considered compiled. Modification time is not checked, ie, an old
compiled addon will override a newer source version. If there are
three or more versions of an addon, all are shown. )

New addons added/included:

-   autosync - example symlink to ledger-autosync
-   budget - experimental budget reporting command supporting Ledger-like periodic transactions and automated transactions (Mykola Orliuk)
-   chart - pie-chart-generating prototype, a repackaging of the old hledger-chart tool
-   check - more powerful balance assertions (Michael Walker)
-   check-dupes - find accounts sharing the same leaf name (Stefano Rodighiero)
-   prices - show all market price records (Mykola Orliuk)
-   register-match - a helper for ledger-autosync's deduplication, finds best match for a transaction description

The equity command now always generates a valid journal transaction,
handles prices better, and adds balance assertions (Mykola Orliuk).

The rewrite command is more robust and powerful (Mykola Orliuk):

-   in addition to command-line rewrite options, it understands rewrite rules
    defined in the journal, similar to Ledger's automated transactions (#99).
    Eg:

        = ^income
            (liabilities:tax)  *.33

        = expenses:gifts
            budget:gifts  *-1
            assets:budget  *1

-   it can generate diff output, allowing easier review of the proposed
    changes, and safe modification of original journal files (preserving
    file-level comments and directives). Eg:

        hledger-rewrite --diff Agency --add-posting 'Expenses:Taxes  *0.17' | patch

-   rewrites can affect multiple postings in a transaction, not just one.

-   posting-specific dates are handled better

## balance

A new --pretty-tables option uses unicode characters for rendering
table borders in multicolumn reports (#522) (Moritz Kiefer)

## balancesheet/cashflow/incomestatement

These commands are now more powerful, able to show multicolumn reports
and generally having the same features as the balance command. (Justin Le)

balancesheet has always ignored a begin date specified with a `-b` or
`-p` option; now it also ignores a begin date specified with a `date:`
query. (Related discussion at #531)

## print

The output of print is now always a valid journal (fixes #465) (Mykola Orliuk).

print now tries to preserves the format of implicit/explicit balancing
amounts and prices, by default. To print with all amounts explicit,
use the new `--explicit/-x` flag (fixes #442). (Mykola Orliuk)

Don't lose the commodity of zero amounts/zero balance assertions (fixes #475) (Mykola Orliuk)

## Misc

Fix a regression in the readability of option parsing errors (#478) (Hans-Peter Deifel)

Fix an example in Cli/Main.hs (Steven R. Baker)

Allow megaparsec 5.2 (#503)

# 1.1 (2016/12/31)

## balance

-   with -V, don't ignore market prices in the future (#453, #403)

-   with -V and multiple same-date market prices, use the last parsed not the highest price (#403)

## misc

-   fix non-existent "oldtime" dependency (#431)

-   extra/hledger-equity.hs now generates valid journal format when there are multiple commodities

# 1.0.1 (2016/10/27)

-   allow megaparsec 5.0 or 5.1

-   fix benchmark build failure (#423)

# 1.0 (2016/10/26)

## add

-   suggest only one commodity at a time as default amount (#383)

    (since we currently can't input more than one at a time)

## balance

-   added --change flag for consistency

-   -H/--historical now also affects single-column balance reports with a start date (#392).

    This has the same effect as just omitting the start date, but adds consistency.

-   in CSV output, render amounts in one-line format (#336)

## balancesheet

-   fix an infinite loop (#393)

## print

-   in CSV output, fix and rename the transaction id field

## register

-   fix a sorting regression with --date2 (#326)

-   --average/-A is now affected by --historical/-H

-   added --cumulative flag for consistency

-   in CSV output, include the transaction id and rename the total field (#391)

## stats

-   fixed an issue with ordering of include files

## misc

-   --pivot option added, groups postings by tag instead of account (#323) (Malte Brandy)

-   --anon option added, obfuscates account names and descriptions (#265) (Brian Scott)

    (Only affects the hledger tool, for now.)

-   try to clarify balance/register's various report modes,

    kinds of "balance" displayed, and related options and language.

-   with multiple --change/--cumulative/--historical flags, use the last one instead of complaining

-   don't add the "d" suffix when displaying day periods

-   stack-ify extra/hledger-rewrite.hs

## misc

-   added GHC 8 support, dropped GHC 7.6 and 7.8 support.

    GHC 7.8 support could be restored with small code changes and a maintainer.

-   a cabal.project file has been added (Moritz Kiefer)

-   use hpack for maintaining cabal files (#371).

    Instead of editing cabal files directly, we now edit the less
    verbose and less redundant package.yaml files and let stack (or
    hpack) update the cabal files. We commit both the .yaml and
    .cabal files.

-   clean up some old cabal flags

-   tools/simplebench has been spun off as the quickbench package.

-   add Appveyor CI builds, provide up-to-date binaries for Windows

-   extra: add a bunch of CSV rules examples

## docs

-   the website is simpler, clearer, and more mobile-friendly.

    Docs are now collected on a single page and organised by type: getting started, reference, more.

-   reference docs have been split into one manual for each executable and file format.

    This helps with maintenance and packaging and also should make it
    easier to see what's available and to read just what you need.

-   manuals are now provided in html, plain text, man and info formats

    generated from the same source by a new Shake-based docs build system. (#292)

-   versioned manuals are provided on the website, covering recent releases and the latest dev version (#385, #387)

-   manuals are built in to the hledger executables, allowing easy offline reading on all platforms.

        PROG -h              shows PROG's command-line usage
        PROG --help          shows PROG's manual (fixed width)
        PROG --man           shows PROG's manual with man (formatted/paged)
        PROG --info          shows PROG's manual with info (hypertext)
        hledger help [TOPIC] shows any manual
        hledger man  [TOPIC] shows any manual with man
        hledger info [TOPIC] shows any manual with info

-   the general and reporting options are now listed in all executable manuals.

    We assume any of them which are unsupported are harmlessly ignored.

-   demo.hledger.org is using beancount's example journal.

    This is the somewhat realistic example journal from the beancount
    project, tweaked for hledger.

-   minor copyedits (jungle-boogie)

## cli

-   parsing multiple input files is now robust.

    When multiple -f options are provided, we now parse each file
    individually rather than just concatenating them, so they can
    have different formats (#320). Note this also means that
    directives (like \`Y\` or \`alias\`) no longer carry over from one
    file to the next.

-   -I has been added as the short flag for --ignore-assertions

    (this is different from Ledger's CLI, but useful for hledger-ui).

-   parsing an argument-less --debug option is more robust

0.27 (2015/10/30)

Account aliases:

-   Regular expression account aliases are now fast enough that you can
    use lots of them without slowing things down. They now take
    O(aliases x accounts) time, instead of O(aliases x transactions);
    also, regular expressions are no longer recompiled unnecessarily.

Documentation:

-   Each hledger package now includes one or more man pages, generated
    from markdown by the mighty pandoc. Currently there are six: one
    for each main executable and each input file format. Currently these
    somewhat duplicate the manual on the website; this will be resolved
    somehow. (#282).

-   The site is now built with hakyll-std, a generic hakyll script.

-   hledger once again has a HCAR entry.

Tools:

-   The hledger cabal files are now generated from package.yaml files by
    hpack, in principle, removing a lot of error-prone duplication and
    boilerplate. (In practice, both files are being updated manually
    for the moment, until hpack supports flags and conditional blocks.)

-   Time/allocation and heap profiling is working again, and easier:

    -   `make quickprof-CMD` generates a profile for CMD, which runs
        against one of the sample journals. (CMD must be one word,
        enclosing in double quotes isn't working here for some reason).

    -   `make quickheap-CMD` generates a heap profile for CMD, in
        hledgerprof.ps, and tries to open it in a viewer (currently the
        mac-friendly "open" executable, so you may need to adjust this in
        the makefile). As with quickprof, CMD must be one word and runs
        against one of the sample journals.

    -   `make hledgerprof` builds the hledgerprof executable used for
        time/allocation profiling. `make hledgercov` builds the hledgercov
        executable used for coverage reports.

-   Travis CI now tests the build on each github push and announces
    status changes by email and on #hledger.

Journal format:

-   Dates must now begin with a digit (not /, eg).

-   The comment directive longer requires an end comment, and will
    extend to the end of the file(s) without it.

Command-line interface:

-   Output (balance reports, register reports, print output etc.)
    containing wide characters, eg chinese/japanese/korean characters,
    should now align correctly, when viewed in apps and fonts that show
    wide characters as double width (#242).

-   The argument for --depth or depth: must now be positive.

add:

-   Journal entries are now written with all amounts explicit, to avoid
    losing price info (#283).

-   Fixed a bug which sometimes (when the same letter pair was repeated)
    caused it not to pick the most similar past transaction for defaults.

balance:

-   There is now a -V/--value flag to report current market value (as in Ledger).
    It converts all reported amounts using their "default market price".
    "Market price" is the new name for "historical prices", defined with the P directive.
    The default market price for a commodity is the most recent one found in the journal on or before the report end date.

    Unlike Ledger, hledger's -V uses only the market prices recorded
    with P directives; it does not use the "transaction prices"
    recorded as part of posting amounts (which are used by -B/--cost).
    Also, using both -B and -V at the same time is supported.

-   Fixed a bug in amount normalization which caused amount styles
    (commodity symbol placement, decimal point character, etc.) to be
    lost in certain cases (#230, #276).

-   The balance command's --format option can now adjust the rendering
    style of multi-commodity amounts, if you begin the format string
    with one of:

        %_  - renders amounts on multiple lines, bottom-aligned (the default)
        %^  - renders amounts on multiple lines, top-aligned
        %,  - renders amounts on one line, comma-separated

-   The balance report's final total (and the line above it) now adapt
    themselves to a custom --format.

print:

-   The --match option prints the journal entry that best matches a
    description (ie whose description field is most similar to the value
    given, and if there are several equally similar, the most recent).
    This was originally an add-on I used to guess account names for
    ledger-autosync. It's nice for quickly looking up a recent
    transaction from a guessed or partial description.

-   print now always right-aligns the amounts in an entry, even when
    they are wider than 12 characters. (If there is a price, it's
    considered part of the amount for right-alignment.)

register:

-   Amount columns now resize automatically, using more space if it's
    needed and available.

0.26 (2015/7/12)

Account aliases:

-   Account aliases are once again non-regular-expression-based, by default. (#252)

    The regex account aliases added in 0.24 trip up people switching between
    hledger and Ledger. (Also they are currently slow).

    This change makes the old non-regex aliases the default; they are
    unsurprising, useful, and pretty close in functionality to Ledger's.

    The new regex aliases are still available; they must be enclosed
    in forward slashes. (Ledger effectively ignores these.)

Journal format:

-   We now parse, and also print, journal entries with no postings, as
    proposed on the mail lists. These are not well-formed General
    Journal entries/transactions, but here is my rationale:

    -   Ledger and beancount parse them
    -   if they are parsed, they should be printed
    -   they provide a convenient way to record (and report) non-transaction events
    -   they permit more gradual introduction and learning of the concepts.
        So eg a beginner can keep a simple journal before learning about accounts and postings.

-   Trailing whitespace after a `comment` directive is now ignored.

Command-line interface:

-   The -f/file option may now be used multiple times.
    This is equivalent to concatenating the input files before running hledger.
    The add command adds entries to the first file specified.

Queries:

-   real: (no argument) is now a synonym for real:1

-   tag: now matches tag names with a regular expression, like most other queries

-   empty: is no longer supported, as it overlaps a bit confusingly with
    amt:0. The --empty flag is still available.

-   You can now match on pending status (#250)

    A transaction/posting status of ! (pending) was effectively equivalent
    to * (cleared). Now it's a separate state, not matched by --cleared.
    The new Ledger-compatible --pending flag matches it, and so does
    --uncleared.

    The relevant search query terms are now status:*, status:! and
    status: (the old status:1 and status:0 spellings are deprecated).

    Since we interpret --uncleared and status: as "any state except cleared",
    it's not currently possible to match things which are neither cleared
    nor pending.

activity:

-   activity no longer excludes 0-amount postings by default.

add:

-   Don't show quotes around the journal file path in the "Creating..."
    message, for consistency with the subsequent "Adding..." message.

balancesheet:

-   Accounts beginning with "debt" or now also recognised as liabilities.

print:

-   We now limit the display precision of inferred prices. (#262)

    When a transaction posts to two commodities without specifying the
    conversion price, we generate a price which makes it balance (cf
    https://hledger.org/hledger.html#prices). The print command showed
    this with full precision (so that manual calculations with the
    displayed numbers would look right), but this sometimes meant we
    showed 255 digits (when there are multiple postings in the
    commodity being priced, and the averaged unit price is an
    irrational number). In this case we now set the price's display
    precision to the sum of the (max) display precisions of the
    commodities involved. An example:

          hledgerdev -f- print
          <<<
          1/1
              c    C 10.00
              c    C 11.00
              d  D -320.00
          >>>
          2015/01/01
              c  C 10.00 @ D 15.2381
              c  C 11.00 @ D 15.2381
              d     D -320.00

          >>>=0

    There might still be cases where this will show more price decimal
    places than necessary.

-   We now show inferred unit prices with at least 2 decimal places.

    When inferring prices, if the commodities involved have low
    display precisions, we don't do a good job of rendering
    accurate-looking unit prices. Eg if the journal doesn't use any
    decimal places, any inferred unit prices are also displayed with
    no decimal places, which makes them look wrong to the user. Now,
    we always give inferred unit prices a minimum display precision of
    2, which helps a bit.

register:

-   Postings with no amounts could give a runtime error in some obscure case, now fixed.

stats:

-   stats now supports -o/--outputfile, like register/balance/print.
-   An O(n\^2) performance slowdown has been fixed, it's now much faster on large journals.

        +--------------------------------------++--------+--------+
        |                                      ||   0.25 |   0.26 |
        +======================================++========+========+
        | -f data/100x100x10.journal     stats ||   0.10 |   0.16 |
        | -f data/1000x1000x10.journal   stats ||   0.45 |   0.21 |
        | -f data/10000x1000x10.journal  stats ||  58.92 |   2.16 |
        +--------------------------------------++--------+--------+

Miscellaneous:

-   The June 30 day span was not being rendered correctly; fixed. (#272)

-   The bench script invoked by "cabal bench" or "stack bench" now runs
    some simple benchmarks.

    You can get more accurate benchmark times by running with --criterion.
    This will usually give much the same numbers and takes much longer.

    Or with --simplebench, it benchmarks whatever commands are
    configured in bench/default.bench. This mode uses the first
    "hledger" executable in $PATH.

-   The deprecated shakespeare-text dependency has been removed more thoroughly.

0.25.1 (2015/4/29)

-   timelog: support the description field (#247)

0.25 (2015/4/7)

-   GHC 7.10 compatibility (#239)

-   build with terminfo support on POSIX systems by default

    On non-windows systems, we now build with terminfo support by
    default, useful for detecting terminal width and other things.

    This requires the C curses dev libraries, which makes POSIX
    installation slightly harder; if it causes problems you can
    disable terminfo support with the new `curses` cabal flag, eg:
    cabal install -f-curses ... (or cabal might try this
    automatically, I'm not sure).

-   register: use the full terminal width, respect COLUMNS, allow column width adjustment

    On POSIX systems, register now uses the full terminal width by
    default. Specifically, the output width is set from:

    1.  a --width option
    2.  or a COLUMNS environment variable (NB: not the same as a bash shell var)
    3.  or on POSIX (non-windows) systems, the current terminal width
    4.  or the default, 80 characters.

    Also, register's --width option now accepts an optional
    description column width following the overall width (--width
    WIDTH\[,DESCWIDTH\]). This also sets the account column width, since
    the available space (WIDTH-41) is divided up between these two
    columns. Here's a diagram:

          <--------------------------------- width (W) ---------------------------------->
          date (10)  description (D)       account (W-41-D)     amount (12)   balance (12)
          DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA

    Examples:

          $ hledger reg                 # use terminal width on posix
          $ hledger reg -w 100          # width 100, equal description/account widths
          $ hledger reg -w 100,40       # width 100, wider description
          $ hledger reg -w $COLUMNS,100 # terminal width and set description width

-   balance: new -T/--row-total and -A/--average options

    In multicolumn balance reports, -T/--row-total now shows a row totals
    column and -A/--average shows a row averages column.
    This helps eg to see monthly average expenses (hledger bal \^expenses -MA).

    NB our use of -T deviates from Ledger's UI, where -T sets a custom
    final total expression.

-   balance: -N is now short for --no-total
-   balance: fix partially-visible totals row with --no-total

    A periodic (not using --cumulative or --historical) balance report
    with --no-total now hides the totals row properly.

-   journal, csv: comment lines can also start with *

    As in Ledger. This means you can embed emacs org/outline-mode nodes in
    your journal file and manipulate it like an outline.

0.24.1 (2015/3/15)

-   journal: fix balance accumulation across assertions (#195)

    A sequence of balance assertions asserting first one commodity, then
    another, then the first again, was not working.

-   timelog: show hours with two decimal places instead of one (#237)
-   in weekly reports, simplify week 52's heading like the others
-   disallow trailing garbage in a number of parsers

    Trailing garbage is no longer ignored when parsing the following:
    balance --format option, register --width option, hledger-rewrite
    options, hledger add's inputs, CSV amounts, posting amounts,
    posting dates in tags.

-   allow utf8-string-1 (fpco/stackage/#426)

0.24 (2014/12/25)

General:

-   fix redundant compilation when cabal installing the hledger packages
-   switch to Decimal for representing amounts (#118)
-   report interval headings (eg in balance, register reports) are shown
    compactly when possible
-   general speedups

Journal format:

-   detect decimal point and digit groups more robustly (#196)
-   check that transaction dates are followed by whitespace or newline
-   check that dates use a consistent separator character
-   balance assertions now are specific to a single commodity, like
    Ledger (#195)
-   support multi-line comments using "comment", "end comment"
    directives, like Ledger

CSV format:

-   reading CSV data from stdin now works better
-   the rules file include directive is now relative to the current
    file's directory (#198)
-   the original order of same-day transactions is now usually preserved
    (if the records appear to be in reverse date order, we reverse them
    before finally sorting by transaction date)
-   CSV output is now built in to the balance, print, and register
    commands, controlled by -O/--output-format (and -o/--output-file,
    see below)

CLI:

-   the --width and --debug options now require their argument (#149)
-   when an option is repeated, the last value takes precedence (#219).
    This is helpful eg for customising your reporting command aliases on
    the fly.
-   smart dates (used in -p/-b/-e/date:/date2:) now must use a
    consistent separator character, and must be parseable to the end
-   output destination and format selection is now built in to the
    balance, print and register commands, controlled by -o/--output-file
    and -O/--output-format options. Notes:
    -   -o - means stdout
    -   an output file name suffix matching a supported format will also
        set the output format, unless overridden by --output-format
    -   commands' supported output formats are listed in their
        command-line help. Two formats are currently available:
        txt (the default) and csv.
-   balance assertions can be disabled with --ignore-assertions

Account aliases:

-   all matching account aliases are now applied, not just one directive
    and one option
-   account aliases now match by case insensitive regular expressions
    matching anywhere in the account name
-   account aliases can replace multiple occurrences of the pattern
    within an account name
-   an account alias replacement pattern can reference matched groups
    with \N

Queries:

-   date:/date2: with a malformed date now reports an error instead of
    being ignored
-   amt: now supports >= or <=
-   clarify status: docs and behaviour; \"*\" is no longer a synonym for
    "1" (fixes #227)

balance:

-   fix: in tree mode, --drop is ignored instead of showing empty account names
-   a depth limit of 0 now shows summary items with account name "...",
    instead of an empty report (#206)
-   in multicolumn balance reports, -E now also shows posting-less
    accounts with a non-zero balance during the period (in addition to
    showing leading & trailing empty columns)
-   in multicolumn reports, multi-commodity amounts are rendered on one
    line for better layout (#186)
-   multicolumn reports' title now includes the report span

register:

-   runs faster with large output
-   supports date2:, and date:/date2: combined with --date2, better (fixes
    #201, #221, #222)
-   a depth limit of 0 now shows summary items (see balance)
-   -A/--average now implies -E/--empty
-   postings with multi-commodity amounts are now top-aligned, like
    Ledger

Extra commands:

-   hledger-equity: fix end date in title; print closing entry too
-   hledger-check-dates: added

0.23.3 (2014/9/12)

-   allow text 1.2+ (#207)

0.23.2 (2014/5/8)

-   register: also fix date sorting of postings (#184)

0.23.1 (2014/5/7)

-   register: fix a refactoring-related regression that the tests
    missed: if transactions were not ordered by date in the journal,
    register could include postings before the report start date in the
    output. (#184)
-   add: don't apply a default commodity to amounts on entry (#138)
-   cli: options before the add-on command name are now also passed to it (#182)
-   csv: allow the first name in a fields list to be empty (#178)
-   csv: don't validate fields count in skipped lines (#177)

0.23 (2014/5/1)

Journal format:

-   A # (hash) in column 0 is now also supported for starting a top-level journal comment, like Ledger.
-   The "too many missing amounts" error now reminds about the 2-space rule.
-   Fix: . (period) is no longer parsed as a valid amount.
-   Fix: default commodity directives no longer limit the maximum display precision (#169).
-   Fix: + before an amount is no longer parsed as part of the commodity (#181).

CLI:

-   Command-line help cleanups, layout improvements.
-   Descriptions are shown for known add-ons in the command list.
-   Command aliases have been simplified.
-   Add-ons can now have any of these file extensions:
    none, hs, lhs, pl, py, rb, rkt, sh, bat, com, exe.
-   Add-ons are displayed without their file extensions when possible.
-   Add-ons with the same name as a built-in command or alias are ignored.
-   Fix: add-on detection and invocation now works on windows.
-   Fix: add-ons with digits in the name are now found.
-   Fix: add-on arguments containing a single quote now work.
-   Fix: when -- is used to hide add-on options from the main program,
    it is no longer passed through as an add-on argument.

Queries:

-   The currency/commodity query prefix (sym:) has been renamed to cur:.
-   Currency/commodity queries are applied more strongly in register and
    balance reports, filtering out unwanted currencies entirely. Eg
    hledger balance cur:'$' now reports only the dollar amounts even if
    there are multi-currency transactions or postings.
-   Amount queries like amt:N, amt:<N and amt:>N, where N is not 0, now do an unsigned
    comparison of the amount and N. That is, they compare the absolute magnitude.
    To do a signed comparison instead, write N with its sign (eg amt:+N, amt:<+N, amt:>-N).
-   Fix: amount queries no longer give false positives on multi-commodity amounts.

accounts:

-   An accounts command has been added, similar to Ledger's, for listing account names
    in flat or hierarchical mode.

add:

-   Tab completion now works at all prompts, and will insert the default if the input area is empty.
-   Account and amount defaults are more robust and useful.
-   Transactions may also be completed by the enter key, when there are no more default postings.
-   Input prompts are displayed in a different colour when supported.

balance:

-   Balance reports in flat mode now always show exclusive (subaccount-excluding) balances.
-   Balance reports in flat mode with --depth now aggregate deeper accounts at the depth limit instead of excluding them.
-   Multicolumn reports in flat mode now support --drop.
-   Multicolumn balance reports can now show the account hierarchy with --tree.
-   Multicolumn report start/end dates are adjusted to encompass the displayed
    report periods, so the first and last periods are "full" and comparable to the others.
-   Fix: zero-balance leaf accounts below a non-zero-balance parent are no longer always shown (#170).
-   Fix: multicolumn reports now support --date2 (cf #174).

balancesheet, cashflow, incomestatement:

-   These commands now support --flat and --drop.

print:

-   Tag queries (tag:) will now match a transaction if any of its postings match.

register:

-   The --display option has been dropped. To see an accurate running total which
    includes the prior starting balance, use --historical/-H (like balance).
-   With a report interval, report start/end dates are adjusted to encompass the displayed
    periods, so the first and last periods are "full" and comparable to the others.
-   Fix: --date2 now works with report intervals (fixes #174).

Miscellaneous:

-   Default report dates now derive from the secondary dates when --date2 is in effect.
-   Default report dates now notice any posting dates outside the transaction dates' span.
-   Debug output improvements.
-   New add-on example: extra/hledger-rewrite.hs, adds postings to matched entries.
-   Compatible with GHC 7.2 (#155) - GHC 7.8, shakespeare 2

0.22.2 (2014/4/16)

-   display years before 1000 with four digits, not three
-   avoid pretty-show to build with GHC < 7.4
-   allow text 1.1, drop data-pprint to build with GHC 7.8.x

0.22.1 (2014/1/6) and older: see https://hledger.org/release-notes or doc/release-notes.md.
