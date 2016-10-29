<!-- Web release notes. Content since 1.0 comes from doc/release-notes.org. -->
<!-- Putting the dates last is preferred for readability, but they are first in the headings below since that nicely keeps them out of the anchor urls. -->
<style>
#toc > ol > li { padding-top:0; }
h4 { margin-top:2em; }
</style>
<nav id="toc">
<p>Major releases:</p>
<ol>
<li><a href="#hledger-1.0">hledger 1.0 (2016/10/26)</a>
<li><a href="#hledger-0.27">hledger 0.27 (2015/10/30)</a>
<li><a href="#hledger-0.26">hledger 0.26 (2015/7/12)</a>
<li><a href="#hledger-0.25">hledger 0.25 (2015/4/7)</a>
<li><a href="#hledger-0.24">hledger 0.24 (2014/12/25)</a>
<li><a href="#hledger-0.23">hledger 0.23 (2014/5/1)</a>
<li><a href="#hledger-0.22">hledger 0.22 (2013/12/13)</a>
<li><a href="#hledger-0.21">hledger 0.21 (2013/6/1)</a>
<li><a href="#hledger-0.20">hledger 0.20 (2013/5/1)</a>
<li><a href="#hledger-0.19">hledger 0.19 (2012/10/21)</a>
<li><a href="#hledger-0.18">hledger 0.18 (2012/5/29)</a>
<li><a href="#hledger-0.17">hledger 0.17 (2012/2/1)</a>
<li><a href="#hledger-0.16">hledger 0.16 (2011/10/1)</a>
<li><a href="#hledger-0.15">hledger 0.15 (2011/9/1)</a>
<li><a href="#hledger-0.14">hledger 0.14 (2011/4/22)</a>
<li><a href="#hledger-0.13">hledger 0.13 (2010/12/6)</a>
<li><a href="#hledger-0.12">hledger 0.12 (2010/9/5)</a>
<li><a href="#hledger-0.11">hledger 0.11 (2010/07/17)</a>
<li><a href="#hledger-0.10">hledger 0.10 (2010/05/23)</a>
<li><a href="#hledger-0.9">hledger 0.9 (2010/04/10)</a>
<li><a href="#hledger-0.8">hledger 0.8 (2010/02/11)</a>
<li><a href="#hledger-0.7">hledger 0.7 (2009/12/11)</a>
<li><a href="#hledger-0.6">hledger 0.6 (2009/06/13)</a>
<li><a href="#hledger-0.5">hledger 0.5 (2009/05/23)</a>
<li><a href="#hledger-0.4">hledger 0.4 (2009/04/03)</a>
<li><a href="#hledger-0.3">hledger 0.3 (2009/01/17)</a>
<li><a href="#hledger-0.2">hledger 0.2 (2008/11/23)</a>
<li><a href="#hledger-0.1">hledger 0.1 (2008/10/15)</a>
</ol>
</nav>

# Release notes

<!--
Based on the 
[hledger](http://hackage.haskell.org/package/hledger/changelog),
[hledger-ui](http://hackage.haskell.org/package/hledger-ui/changelog),
[hledger-web](http://hackage.haskell.org/package/hledger-web/changelog),
[hledger-api](http://hackage.haskell.org/package/hledger-web/changelog),
[hledger-lib](http://hackage.haskell.org/package/hledger-lib/changelog) &
[hledger project](https://github.com/simonmichael/hledger/blob/master/doc/CHANGES)
change logs.
-->


## 2016/10/26 hledger 1.0

***More hledger-ui features, 
better hledger-web layout,
new web API server, 
new timedot format, 
--pivot & --anon, 
performance improvements,
reorganized multi-format docs,
built-in help. 
***
<!-- ([announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1267)) -->
<!-- [announcement](https://groups.google.com/forum/#!topic/hledger/3w7G0H9e7aE) -->

Release contributors:
Simon Michael, Dominik Süß, Thomas R. Koll, Moritz Kiefer,
jungle-boogie, Sergei Trofimovich, Malte Brandy, Sam Doshi, 
Mitchell Rosen, Hans-Peter Deifel, Brian Scott, and Andrew Jones.

  [project-wide](#project-wide-changes)
| [hledger-lib](#hledger-lib-1.0)
| [hledger](#hledger-1.0-1)
| [hledger-ui](#hledger-ui-1.0)
| [hledger-web](#hledger-web-1.0)
| [hledger-api](#hledger-api-1.0)

### project-wide changes

#### misc

-   added GHC 8 support, dropped GHC 7.6 and 7.8 support.

    GHC 7.8 support could be restored with small code changes and a maintainer.

-   a cabal.project file has been added (Moritz Kiefer)

-   use hpack for maintaining cabal files ([#371](http://bugs.hledger.org/371)).

    Instead of editing cabal files directly, we now edit the less
    verbose and less redundant package.yaml files and let stack (or
    hpack) update the cabal files. We commit both the .yaml and
    .cabal files.

-   clean up some old cabal flags

-   tools/simplebench has been spun off as the [quickbench](http://hackage.haskell.org/package/quickbench) package.

-   add Appveyor CI builds, provide [more up-to-date Windows binaries](http://hledger.org/developer-guide.html#quick-links)

-   extra: add a bunch of CSV rules examples

#### docs

-   the website is simpler, clearer, and more mobile-friendly.

    Docs are now collected on a single page and organised by type: getting started, reference, more.

-   reference docs have been split into one manual for each executable and file format.

    This helps with maintenance and packaging and also should make it
    easier to see what's available and to read just what you need.

-   manuals are now provided in html, plain text, man and info formats

    generated from the same source by a new Shake-based docs build system. ([#292](http://bugs.hledger.org/292))

-   versioned manuals are provided on the website, covering recent releases and the latest dev version ([#385](http://bugs.hledger.org/385), [#387](http://bugs.hledger.org/387))

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

#### cli

-   parsing multiple input files is now robust.

    When multiple -f options are provided, we now parse each file
    individually rather than just concatenating them, so they can
    have different formats ([#320](http://bugs.hledger.org/320)).  Note this also means that
    directives (like \`Y\` or \`alias\`) no longer carry over from one
    file to the next.

-   I has been added as the short flag for --ignore-assertions

    (this is different from Ledger's CLI, but useful for hledger-ui).

-   parsing an argument-less --debug option is more robust

### hledger-lib 1.0

#### timedot format

-   new "timedot" format for retroactive/approximate time logging.

    Timedot is a plain text format for logging dated, categorised
    quantities (eg time), supported by hledger.  It is convenient
    for approximate and retroactive time logging, eg when the
    real-time clock-in/out required with a timeclock file is too
    precise or too interruptive.  It can be formatted like a bar
    chart, making clear at a glance where time was spent.

#### timeclock format

-   renamed "timelog" format to "timeclock", matching the emacs package

-   sessions can no longer span file boundaries (unclocked-out

    sessions will be auto-closed at the end of the file).

-   transaction ids now count up rather than down ([#394](http://bugs.hledger.org/394))

-   timeclock files no longer support default year directives

-   removed old code for appending timeclock transactions to journal transactions.

    A holdover from the days when both were allowed in one file.

#### csv format

-   fix empty field assignment parsing, rule parse errors after megaparsec port ([#407](http://bugs.hledger.org/407)) (Hans-Peter Deifel)

#### journal format

-   journal files can now include timeclock or timedot files ([#320](http://bugs.hledger.org/320))

    (but not yet CSV files).

-   fixed an issue with ordering of same-date transactions included from other files

-   the "commodity" directive and "format" subdirective are now supported, allowing

    full control of commodity style ([#295](http://bugs.hledger.org/295)) The commodity directive's
    format subdirective can now be used to override the inferred
    style for a commodity, eg to increase or decrease the
    precision. This is at least a good workaround for [#295](http://bugs.hledger.org/295).

-   Ledger-style "apply account"/"end apply account" directives are now used to set a default parent account.

-   the Ledger-style "account" directive is now accepted (and ignored).

-   bracketed posting dates are more robust ([#304](http://bugs.hledger.org/304))

    Bracketed posting dates were fragile; they worked only if you
    wrote full 10-character dates. Also some semantics were a bit
    unclear. Now they should be robust, and have been documented
    more clearly. This is a legacy undocumented Ledger syntax, but
    it improves compatibility and might be preferable to the more
    verbose "date:" tags if you write posting dates often (as I do).
    Internally, bracketed posting dates are no longer considered to
    be tags.  Journal comment, tag, and posting date parsers have
    been reworked, all with doctests.

-   balance assertion failure messages are clearer

-   with --debug=2, more detail about balance assertions is shown.

#### misc

-   file parsers have been ported from Parsec to Megaparsec \o/ ([#289](http://bugs.hledger.org/289), [#366](http://bugs.hledger.org/366)) (Alexey Shmalko, Moritz Kiefer)

-   most hledger types have been converted from String to Text, reducing memory usage by 30%+ on large files and giving a slight speed increase

-   file parsers have been simplified for easier troubleshooting ([#275](http://bugs.hledger.org/275)).

    The journal/timeclock/timedot parsers, instead of constructing
    opaque journal update functions which are later applied to build
    the journal, now construct the journal directly by modifying the
    parser state. This is easier to understand and debug. It also
    rules out the possibility of journal updates being a space
    leak. (They weren't, in fact this change increased memory usage
    slightly, but that has been addressed in other ways).  The
    ParsedJournal type alias has been added to distinguish
    "being-parsed" journals and "finalised" journals.

-   file format detection is more robust.

    The Journal, Timelog and Timedot readers' detectors now check
    each line in the sample data, not just the first one. I think the
    sample data is only about 30 chars right now, but even so this
    fixed a format detection issue I was seeing. 
    Also, we now always try parsing stdin as journal format (not just sometimes).

-   all file formats now produce transaction ids, not just journal ([#394](http://bugs.hledger.org/394))

-   git clone of the hledger repo on windows now works ([#345](http://bugs.hledger.org/345))

-   added missing benchmark file ([#342](http://bugs.hledger.org/342))

-   our stack.yaml files are more compatible across stack versions ([#300](http://bugs.hledger.org/300))

-   use [newer file-embed](https://github.com/snoyberg/file-embed/issues/18) to fix ghci working directory dependence

-   report more accurate dates in account transaction report when postings have their own dates

    (affects hledger-ui and hledger-web registers).
    The newly-named "transaction register date" is the date to be
    displayed for that transaction in a transaction register, for
    some current account and filter query.  It is either the
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

-   the Hledger.Read.\* modules have been reorganised for better reuse.

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

### hledger 1.0

#### add

-   suggest only one commodity at a time as default amount ([#383](http://bugs.hledger.org/383))

    (since we currently can't input more than one at a time)

#### balance

-   added --change flag for consistency

-   H/--historical now also affects single-column balance reports with a start date ([#392](http://bugs.hledger.org/392)).

    This has the same effect as just omitting the start date, but adds consistency.

-   in CSV output, render amounts in one-line format ([#336](http://bugs.hledger.org/336))

#### balancesheet

-   fix an infinite loop ([#393](http://bugs.hledger.org/393))

#### print

-   in CSV output, fix and rename the transaction id field

#### register

-   fix a sorting regression with --date2 ([#326](http://bugs.hledger.org/326))

-   --average/-A is now affected by --historical/-H

-   added --cumulative flag for consistency

-   in CSV output, include the transaction id and rename the total field ([#391](http://bugs.hledger.org/391))

#### stats

-   fixed an issue with ordering of include files

#### misc

-   --pivot option added, groups postings by tag instead of account ([#323](http://bugs.hledger.org/323)) (Malte Brandy)

-   --anon option added, obfuscates account names and descriptions ([#265](http://bugs.hledger.org/265)) (Brian Scott)

    (Only affects the hledger tool, for now.)

-   try to clarify balance/register's various report modes,

    kinds of "balance" displayed, and related options and language.

-   with multiple --change/--cumulative/--historical flags, use the last one instead of complaining

-   don't add the "d" suffix when displaying day periods

-   stack-ify extra/hledger-rewrite.hs

### hledger-ui 1.0

#### accounts screen

-   at depth 0, show accounts on one "All" line and show all transactions in the register

-   0 now sets depth limit to 0 instead of clearing it

-   always use --no-elide for a more regular accounts tree

#### register screen

-   registers can now include/exclude subaccount transactions.

    The register screen now includes subaccounts' transactions if the
    accounts screen was in tree mode, or when showing an account
    which was at the depth limit. Ie, it always shows the
    transactions contributing to the balance displayed on the
    accounts screen. As on the accounts screen, F toggles between
    tree mode/subaccount txns included by default and flat
    mode/subaccount txns excluded by default. (At least, it does when
    it would make a difference.)

-   register transactions are filtered by realness and status ([#354](http://bugs.hledger.org/354)).

    Two fixes for the account transactions report when --real/--cleared/real:/status: 
    are in effect, affecting hledger-ui and hledger-web:
    
    1.  exclude transactions which affect the current account via an excluded posting type.
        Eg when --real is in effect, a transaction posting to the current account with only
        virtual postings will not appear in the report.
    
    2.  when showing historical balances, don't count excluded posting types in the
        starting balance. Eg with --real, the starting balance will be the sum of only the
        non-virtual prior postings.
        
        This is complicated and there might be some ways to confuse it still, causing
        wrongly included/excluded transactions or wrong historical balances/running totals
        (transactions with both real and virtual postings to the current account, perhaps ?)

-   show more accurate dates when postings have their own dates.

    If postings to the register account matched by the register's
    filter query have their own dates, we show the earliest of these
    as the transaction date.

#### misc

-   H toggles between showing "historical" or "period" balances ([#392](http://bugs.hledger.org/392)).

    By default hledger-ui now shows historical balances, which
    include transactions before the report start date (like hledger
    balance --historical). Use the H key to toggle to "period" mode,
    where balances start from 0 on the report start date.

-   shift arrow keys allow quick period browsing

    -   shift-down narrows to the next smaller standard period
        (year/quarter/month/week/day), shift-up does the reverse
    -   when narrowed to a standard period, shift-right/left moves to
        the next/previous period
    -   \`t\` sets the period to today.

-   a runs the add command

-   E runs $HLEDGER<sub>UI</sub><sub>EDITOR</sub> or $EDITOR or a default editor (vi) on the journal file.

    When using emacs or vi, if a transaction is selected the cursor will be positioned at its journal entry.

-   / key sets the filter query; BACKSPACE/DELETE clears it

-   Z toggles display of zero items (like --empty), and they are shown by default.

    -E/--empty is now the default for hledger-ui, so accounts with 0 balance
    and transactions posting 0 change are shown by default.  The Z key
    toggles this, entering "nonzero" mode which hides zero items.

-   R toggles inclusion of only real (non-virtual) postings

-   U toggles inclusion of only uncleared transactions/postings

-   I toggles balance assertions checking, useful for troubleshooting

-   vi-style movement keys are now supported (for help, you must now use ? not h) ([#357](http://bugs.hledger.org/357))

-   ESC cancels minibuffer/help or clears the filter query and jumps to top screen

-   ENTER has been reserved for later use

-   reloading now preserves any options and modes in effect

-   reloading on the error screen now updates the message rather than entering a new error screen

-   the help dialog is more detailed, includes the hledger-ui manual, and uses the full terminal width if needed

-   the header/footer content is more efficient; historical/period and tree/flat modes are now indicated in the footer

-   date: query args on the command line now affect the report period.

    A date2: arg or --date2 flag might also affect it (untested).

-   hledger-ui now uses the quicker-building microlens

### hledger-web 1.0

#### ui

-   use full width on large screens, hide sidebar on small screens, more standard bootstrap styling ([#418](http://bugs.hledger.org/418), [#422](http://bugs.hledger.org/422)) (Dominik Süß)

-   show the sidebar by default ([#310](http://bugs.hledger.org/310))

-   fix the add link's tooltip

-   when the add form opens, focus the first field ([#338](http://bugs.hledger.org/338))

-   leave the add form's date field blank, avoiding a problem with tab clearing it ([#322](http://bugs.hledger.org/322))

-   use transaction id instead of date in transaction urls ([#308](http://bugs.hledger.org/308)) (Thomas R. Koll)

-   after following a link to a transaction, highlight it (Thomas R. Koll)

-   misc. HTML/CSS/file cleanups/fixes (Thomas R. Koll)

#### misc

-   startup is more robust ([#226](http://bugs.hledger.org/226)).

    Now we exit if something is already using the specified port,
    and we don't open a browser page before the app is ready.

-   termination is more robust, avoiding stray background threads.

    We terminate the server thread more carefully on exit, eg on control-C in GHCI.

-   more robust register dates and filtering in some situations (see hledger-ui notes)

-   reloading the journal preserves options, arguments in effect ([#314](http://bugs.hledger.org/314)).

    The initial query specified by command line arguments is now preserved
    when the journal is reloaded. This does not appear in the web UI, it's
    like an invisible extra filter.

-   show a proper not found page on 404

-   document the special \`inacct:\` query ([#390](http://bugs.hledger.org/390))

### hledger-api 1.0

#### misc

-   new hledger-api tool: a simple web API server with example clients ([#316](http://bugs.hledger.org/316))

-   start an Angular-based API example client ([#316](http://bugs.hledger.org/316)) (Thomas R. Koll)



## 2015/10/30 hledger 0.27

***New curses-style interface, market value reporting, wide characters, fast regex aliases, man pages***
([announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1267))
<!-- [announcement](https://groups.google.com/forum/#!topic/hledger/3w7G0H9e7aE) -->

Release contributors:
Simon Michael,
Carlos Lopez-Camey.

**hledger 0.27:**

Account aliases:

- Regular expression account aliases are now fast enough that you can
  use lots of them without slowing things down. They now take
  O(aliases x accounts) time, instead of O(aliases x transactions);
  also, regular expressions are no longer recompiled unnecessarily.

Documentation:

- The hledger packages now have man pages, based on the current user
  manual, thanks to the mighty pandoc ([#282](http://bugs.hledger.org/282)).
 
Journal format:

- Dates must now begin with a digit (not /, eg).

- The comment directive longer requires an end comment, and will
  extend to the end of the file(s) without it.

Command-line interface:

- Output (balance reports, register reports, print output etc.)
  containing wide characters, eg chinese/japanese/korean characters,
  should now align correctly, when viewed in apps and fonts that show
  wide characters as double width ([#242](http://bugs.hledger.org/242)).
 
- The argument for --depth or depth: must now be positive.

add:

- Journal entries are now saved with all amounts explicit, to avoid
  losing price info ([#283](http://bugs.hledger.org/283)).

- Fixed a bug which sometimes (when the same letter pair was repeated)
  caused it not to pick the most similar past transaction for defaults.
    
balance:

- There is now a -V/--value flag to report current market value (as in Ledger).
  It converts all reported amounts using their "default market price".
  "Market price" is the new name for "historical prices", defined with the P directive.
  The default market price for a commodity is the most recent one found in the journal on or before the report end date.
    
    Unlike Ledger, hledger's -V uses only the market prices recorded
  with P directives; it does not use the transaction prices
  recorded as part of posting amounts.
  Using both -B and -V at the same time is possible.

- Fixed a bug in amount normalization which caused amount styles
  (commodity symbol placement, decimal point character, etc.) to be
  lost in certain cases ([#230](http://bugs.hledger.org/230), [#276](http://bugs.hledger.org/276)).

- The balance command's --format option can now adjust the rendering
  style of multi-commodity amounts, if you begin the format string
  with one of:
    
     %_  - renders amounts on multiple lines, bottom-aligned (the default)
     %^  - renders amounts on multiple lines, top-aligned
     %,  - renders amounts on one line, comma-separated
    
- The balance report's final total (and the line above it) now adapt
  themselves to a custom --format.

print:

- The --match option prints the journal entry that best matches a
  description (ie whose description field is most similar to the value
  given, and if there are several equally similar, the most recent).
  This was originally an add-on I used to guess account names for
  ledger-autosync. It's nice for quickly looking up a recent
  transaction from a guessed or partial description.

- print now always right-aligns the amounts in an entry, even when
  they are wider than 12 characters.  (If there is a price, it's
  considered part of the amount for right-alignment.)

register:

- Amount columns now resize automatically, using more space if it's
  needed and available.

**hledger-ui 0.27:**

- [hledger-ui](manual#ui) is a new curses-style UI, intended to be a standard part
  of the hledger toolset for all users (except on native MS Windows,
  where the vty lib is not [yet](https://github.com/coreyoconnor/vty/pull/1) supported).

    The UI is quite simple, allowing just browsing of accounts and
  transactions, but it has a number of improvements over the old
  hledger-vty, which it replaces:

    - adapts to screen size
    - handles wide characters
    - shows multi-commodity amounts on one line
    - manages cursor and scroll position better
    - allows depth adjustment
    - allows --flat toggle
    - allows --cleared toggle
    - allows journal reloading
    - shows a more useful transaction register, like hledger-web
    - offers multiple color themes
    - includes some built-in help

    hledger-ui is built with brick, a new higher-level UI library based
  on vty, making it relatively easy to grow and maintain.

**hledger-web 0.27:**

- Fix keyboard shortcut for adding a transaction (Carlos Lopez-Camey)

- Clear the form when clicking 'Add a transaction' (just like the shortcut) (Carlos Lopez-Camey)

- Disallow -f- (reading from standard input) which currently doesn't work ([#202](http://bugs.hledger.org/202))

- Fix broken links when using --base-url ([#235](http://bugs.hledger.org/235))

- Fix the --file-url option ([#285](http://bugs.hledger.org/285))

- Show fewer "other accounts" in the account register: to reduce
  clutter in the "other accounts" field, if there are both real and
  virtual postings to other accounts, show only the accounts posted to
  by real postings.


## 2015/7/12 hledger 0.26

<!-- [announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/N) -->
<!-- [announcement](https://groups.google.com/forum/#!topic/hledger/k2Y_NYZGGJw) -->
***Website & doc updates, account aliases, misc. bugfixes & cleanups, performance.***

Release contributors:
Simon Michael,
Imuli,
Carlos Lopez-Camey,
Kyle Marek-Spartz,
Rick Lupton,
Simon Hengel.

**Changes to hledger.org & docs:**

- examples everywhere, screenshots, content & style updates
- manual: reorganise topics, add some undocumented things, clarify some things
- dev guide: more links, put how-tos first, copy diagram from old wiki, update the setup docs


**User-visible changes in hledger since 0.25.1:**

Account aliases:

- Account aliases are once again non-regular-expression-based, by default. (#252)
    
    The regex account aliases added in 0.24 tend to trip up people
    switching between hledger and Ledger. (Also they are currently
    slow).  We now use the old non-regular-expression aliases again,
    by default; these are unsurprising, useful, and pretty close in
    functionality to Ledger's aliases.

    The new regex aliases are still available, but they must now be
    enclosed in forward slashes. (Ledger effectively ignores these.)
    
Journal format:

- We now parse, and also print, journal entries with no postings, as
  proposed on the mail lists.  These are not well-formed General
  Journal entries/transactions, but on the other hand:
    Ledger and beancount parse them;
    if they are parsed, they should be printed;
    they provide a convenient way to record (and report) non-transaction events;
    and they permit more gradual introduction and learning of the concepts
    (so eg a beginner can keep a simple journal before learning about accounts and postings).

- Trailing whitespace after a `comment` directive is now ignored.

Command-line interface:

- The -f/file option may now be used multiple times. 
  This is equivalent to concatenating the input files before running hledger.
  The add command adds entries to the first file specified.

Queries:

- real: (no argument) is now a synonym for real:1

- tag: now matches tag names with a regular expression, like most other queries

- empty: is no longer supported, as it overlaps a bit confusingly with
  amt:0. The --empty flag is still available.

- You can now match on pending status (#250)
    
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

- activity no longer excludes 0-amount postings by default.

add:

- Don't show quotes around the journal file path in the "Creating..."
  message, for consistency with the subsequent "Adding..." message.

balancesheet:

- Accounts beginning with "debt" or now also recognised as liabilities.

print:

- We now limit the display precision of inferred prices. (#262)
    
    When a transaction posts to two commodities without specifying the
    conversion price, we generate a price which makes it balance (cf
    http://hledger.org/manual.html#prices). The print command showed
    this with full precision (so that manual calculations with the
    displayed numbers would look right), but this sometimes meant we
    showed 255 digits (when there are multiple postings in the
    commodity being priced, and the averaged unit price is an
    irrational number). In this case we now set the price's display
    precision to the sum of the (max) display precisions of the
    commodities involved. An example:
    ```
    hledger -f- print
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
    ```
    There might still be cases where this will show more price decimal
    places than necessary. 

- We now show inferred unit prices with at least 2 decimal places.
    
    When inferring prices, if the commodities involved have low
    display precisions, we don't do a good job of rendering
    accurate-looking unit prices. Eg if the journal doesn't use any
    decimal places, any inferred unit prices are also displayed with
    no decimal places, which makes them look wrong to the user.  Now,
    we always give inferred unit prices a minimum display precision of
    2, which helps a bit.

register:

- Postings with no amounts could give a runtime error in some obscure case, now fixed.

stats: 

- stats now supports -o/--outputfile, like register/balance/print.
- An O(n^2) performance slowdown has been fixed, it's now much faster on large journals.
    ```
    +--------------------------------------++--------+--------+
    |                                      ||   0.25 |   0.26 |
    +======================================++========+========+
    | -f data/100x100x10.journal     stats ||   0.10 |   0.16 |
    | -f data/1000x1000x10.journal   stats ||   0.45 |   0.21 |
    | -f data/10000x1000x10.journal  stats ||  58.92 |   2.16 |
    +--------------------------------------++--------+--------+
    ```
    
Miscellaneous:

- The June 30 day span was not being rendered correctly; fixed. (#272)
- The deprecated shakespeare-text dependency has been removed more thoroughly.
- The bench script invoked by "cabal bench" or "stack bench" now runs
  some simple benchmarks.
  You can get more accurate benchmark times by running with --criterion.
  This will usually give much the same numbers and takes much longer.
  Or with --simplebench, it benchmarks whatever commands are
  configured in bench/default.bench. This mode uses the first
  "hledger" executable in $PATH.

**User-visible changes in hledger-web since 0.25.1:**

- make the j keybinding respect --base-url (fixes #271)
- respect command line options (fixes #225)
- include the unminified jquery source again (#161)
- fix build breakage from #165 (fixes #268)
- fix a js error breaking add form in browsers other than firefox (fixes #251)
- drop deprecated network-conduit dependency

#### 2015/4/29 hledger-web 0.25.1

- support/require base-compat >0.8 (#245)

#### 2015/4/29 hledger 0.25.1

- timelog: support the description field (#247)

#### 2015/4/29 hledger-lib 0.25.1

- support/require base-compat >0.8 (#245)

## 2015/4/7 hledger 0.25

<!-- [announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/N) -->
[announcement](https://groups.google.com/forum/#!topic/hledger/k2Y_NYZGGJw)
***GHC 7.10 compatibility, terminal width awareness, useful averages and totals columns, and a more robust hledger-web add form.***

Release contributors:
Simon Michael,
Julien Moutinho.

**User-visible changes in hledger since 0.24.1:**

- GHC 7.10 compatibility ([#239](http://bugs.hledger.org/239))

- On POSIX systems, the register command now uses the full terminal width by
    default. Specifically, the output width is set from:
    
    1. a --width option
    2. or a COLUMNS environment variable (NB: not the same as a bash shell var)
    3. or on POSIX (non-windows) systems, the current terminal width
    4. or the default, 80 characters.
    
    This feature requires the C curses dev libraries, making installation slightly harder.
    If that's a problem you can disable curses support with a cabal flag:
    `cabal install -f-curses ...`.

- register's --width option now accepts an optional
    description column width following the overall width (`--width
    WIDTH[,DESCWIDTH]`). This also sets the account column width, since
    the available space (WIDTH-41) is divided up between these two
    columns. Here's a diagram:
<br clear="all">
```    
    <--------------------------------- width (W) ---------------------------------->
    date (10)  description (D)       account (W-41-D)     amount (12)   balance (12)
    DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
```    
    Examples:
```
    $ hledger reg                 # use terminal width on posix
    $ hledger reg -w 100          # width 100, equal description/account widths
    $ hledger reg -w 100,40       # width 100, wider description
    $ hledger reg -w $COLUMNS,100 # terminal width and set description width
```

- balance: new -T/--row-total and -A/--average options

    In multicolumn balance reports, -T/--row-total now shows a totals
    column and -A/--average shows an averages column.
    This helps eg to see monthly average expenses (hledger bal ^expenses -MA).

    NB our use of -T deviates from Ledger's UI, where -T sets a custom
    final total expression.

- balance: -N is now short for --no-total
- balance: fix partially-visible totals row with --no-total
    
    A periodic (not using --cumulative or --historical) balance report
    with --no-total now hides the totals row properly.

- journal, csv: comment lines can also start with *
    
    As in Ledger. This means you can embed emacs org/outline-mode nodes in
    your journal file and manipulate it like an outline.

**User-visible changes in hledger-web since 0.24.1:**

- GHC 7.10 compatibility ([#239](http://bugs.hledger.org/239))

- fix the add form when there are included files ([#234](http://bugs.hledger.org/234))

    NB to make this work, the add form now shows the full file path of
    the main and included journal files.

- improve add form validation ([#223](http://bugs.hledger.org/223), [#234](http://bugs.hledger.org/234))
    
    All add form errors are displayed as form errors, not internal
    server errors, and when there are errors the add form is redisplayed
    (form inputs are not preserved, currently).

- keep the add button right-aligned when pressing ctrl - on the add form

#### 2015/3/15 hledger 0.24.1

- timelog: show hours with 2 decimal places, not 1 ([#237](http://bugs.hledger.org/237))
- fix balance accumulation through assertions in several commodities ([#195](http://bugs.hledger.org/195))
- fix rendering of week 52 heading in weekly reports
- allow utf8-string-1 ([fpco/stackage/#426](https://github.com/fpco/stackage/issues/426))

#### 2015/3/15 hledger-lib 0.24.1

- fix JournalReader "ctx" compilation warning
- add some type signatures in Utils to help make ghci-web

#### 2015/1/10 hledger-web 0.24.1

- add missing modules to fix cabal tests ([#232](http://bugs.hledger.org/232))


## 2014/12/25 hledger 0.24

Release contributors:
Simon Michael,
Julien Moutinho,
Ryan Desfosses,
Gergely Risko,
Gwern Branwen.

<!-- [announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/N) -->
***CSV export,
a non-floating point number representation,
more powerful account aliases,
speedups,
and
a streamlined web UI.***

**User-visible changes in hledger since 0.23.3:**

General:

- fix redundant compilation when cabal installing the hledger packages
- switch to Decimal for representing amounts ([#118](http://bugs.hledger.org/118))
- report interval headings (eg in balance, register reports) are shown
  compactly when possible
- general speedups.
```
+--------------------------------------------++----------------+--------------+--------+
|                                            || hledger-0.23.3 | hledger-0.24 | ledger |
+============================================++================+==============+========+
| -f data/100x100x10.journal     balance     ||           0.05 |         0.03 |   0.01 |
| -f data/1000x1000x10.journal   balance     ||           0.34 |         0.21 |   0.04 |
| -f data/10000x1000x10.journal  balance     ||           2.72 |         1.48 |   0.19 |
| -f data/10000x1000x10.journal  balance aa  ||           3.16 |         1.55 |   0.14 |
| -f data/100x100x10.journal     register    ||           0.09 |         0.05 |   0.04 |
| -f data/1000x1000x10.journal   register    ||           0.66 |         0.32 |   0.30 |
| -f data/10000x1000x10.journal  register    ||           6.27 |         2.77 |   2.80 |
| -f data/10000x1000x10.journal  register aa ||           3.30 |         1.62 |   0.21 |
| -f data/100x100x10.journal     print       ||           0.06 |         0.05 |   0.01 |
| -f data/1000x1000x10.journal   print       ||           0.42 |         0.25 |   0.04 |
| -f data/10000x1000x10.journal  print       ||           3.95 |         2.57 |   0.38 |
| -f data/10000x1000x10.journal  print aa    ||           3.23 |         1.56 |   0.14 |
| -f data/100x100x10.journal     stat        ||           0.04 |         0.03 |   0.01 |
| -f data/1000x1000x10.journal   stat        ||           0.35 |         0.24 |   0.03 |
| -f data/10000x1000x10.journal  stat        ||          14.84 |        13.29 |   0.20 |
| -f data/10000x1000x10.journal  stat aa     ||          12.08 |        10.16 |   0.17 |
+--------------------------------------------++----------------+--------------+--------+
```

Journal format:

- detect decimal point and digit groups more robustly ([#196](http://bugs.hledger.org/196))
- check that transaction dates are followed by whitespace or newline
- check that dates use a consistent separator character
- balance assertions now are specific to a single commodity, like
  Ledger ([#195](http://bugs.hledger.org/195))
- support multi-line comments using "comment", "end comment"
  directives, like Ledger

CSV format:

- fix: reading CSV data from stdin now works better
- the original order of same-day transactions is now usually preserved
  (if the records appear to be in reverse date order, we reverse them
  before finally sorting by transaction date)
- the rules file include directive is now relative to the current
  file's directory ([#198](http://bugs.hledger.org/198))
- CSV output is now built in to the balance, print, and register
  commands, controlled by -O/--output-format (and -o/--output-file,
  see below). This means that hledger data can be easily exported,
  eg for spreadsheet reporting or to migrate to a different tool.

CLI:

- the --width and --debug options now require their argument ([#149](http://bugs.hledger.org/149))
- when an option is repeated, the last value takes precedence ([#219](http://bugs.hledger.org/219)).
  This is helpful eg for customising your reporting command aliases on
  the fly.
- smart dates (used in -p/-b/-e/date:/date2:) now must use a
  consistent separator character, and must be parseable to the end
- output destination and format selection is now built in to the
  balance, print and register commands, controlled by -o/--output-file
  and -O/--output-format options. Notes:
  -o - means stdout.
  An output file name suffix matching a supported format will also
  set the output format, unless overridden by --output-format.
  Commands' supported output formats are listed in their
  command-line help. Two formats are currently available:
  txt (the default) and csv.
- balance assertions can be disabled with --ignore-assertions

Account aliases:

- all matching account aliases are now applied, not just one directive
  and one option
- account aliases now match by case insensitive regular expressions
  matching anywhere in the account name
- account aliases can replace multiple occurrences of the pattern
  within an account name
- an account alias replacement pattern can reference matched groups
  with \\N

Queries:

- date:/date2: with a malformed date now reports an error instead of
  being ignored
- amt: now supports >= or <=
- clarify status: docs and behaviour; "*" is no longer a synonym for
  "1" (fixes [#227](http://bugs.hledger.org/227))

balance:

- fix: in tree mode, --drop is ignored instead of showing empty account names
- a depth limit of 0 now shows summary items with account name "...",
  instead of an empty report ([#206](http://bugs.hledger.org/206))
- in multicolumn balance reports, -E now also shows posting-less
  accounts with a non-zero balance during the period (in addition to
  showing leading & trailing empty columns)
- in multicolumn reports, multi-commodity amounts are rendered on one
  line for better layout ([#186](http://bugs.hledger.org/186))
- multicolumn reports' title now includes the report span

register:

- runs faster with large output
- supports date2:, and date:/date2: combined with --date2, better (fixes
  [#201](http://bugs.hledger.org/201), [#221](http://bugs.hledger.org/221), [#222](http://bugs.hledger.org/222))
- a depth limit of 0 now shows summary items (see balance)
- -A/--average now implies -E/--empty
- postings with multi-commodity amounts are now top-aligned, like
  Ledger


**User-visible changes in hledger-web since 0.23.3:**

General:

- fix: add missing hs/js files to package
- the web UI has been streamlined, dropping the raw and entries views and
  the edit form
- the help dialog has been improved
- keyboard shortcuts are now available
- the sidebar can be toggled open or closed (press s)

Journal view:

- layout tweaks for less truncation of descriptions and account names

Register view:

- fix: don't show all zero amounts when searching by account within an
  account register view
- chart improvements: show zero balances with correct commodity; show
  accurate balance at all dates; show transaction events & tooltips;
  show zero/today lines & background colors

Add form:

- parses data more strictly and gives better errors (eg [#194](http://bugs.hledger.org/194))
- allows any number of postings, not just two
- after adding a transaction, goes back to the journal
- keyboard shortcut (a) allows quick access

Dependencies:

- allow warp 3\*, wai-handler-launch 3\*
- require yesod 1.4* (fixes [#212](http://bugs.hledger.org/212))
- js updated (jquery, bootstrap, flot), added (typeahead, cookie, hotkeys),
  removed (select2)


**API-ish changes in hledger-lib since 0.23.3:**

- fix combineJournalUpdates folding order
- fix a regexReplaceCI bug
- fix a splitAtElement bug with adjacent separators
- mostly replace slow regexpr with regex-tdfa (fixes [#189](http://bugs.hledger.org/189))
- use the modern Text.Parsec API
- allow transformers 0.4*
- regexReplace now supports backreferences
- Transactions now remember their parse location in the journal file
- export Regexp types, disambiguate CsvReader's similarly-named type
- export failIfInvalidMonth/Day (closes [#216](http://bugs.hledger.org/216))
- track the commodity of zero amounts when possible
  (useful eg for hledger-web's multi-commodity charts)
- show posting dates in debug output
- more debug helpers


#### 2014/9/12 hledger-web 0.23.3

- remove warp, wai-handler-launch upper bounds (fixes [#205](http://bugs.hledger.org/205))

#### 2014/9/12 hledger 0.23.3

- allow text 1.2+ (fixes [#207](http://bugs.hledger.org/207))

#### 2014/5/8 hledger 0.23.2

- register: also fix date sorting of postings ([#184](http://bugs.hledger.org/184))

#### 2014/5/7 hledger 0.23.1

- register: fix a refactoring-related regression that the tests
  missed: if transactions were not ordered by date in the journal,
  register could include postings before the report start date in the
  output. ([#184](http://bugs.hledger.org/184))
- add: don't apply a default commodity to amounts on entry ([#138](http://bugs.hledger.org/138))
- cli: options before the add-on command name are now also passed to it ([#182](http://bugs.hledger.org/182))
- csv: allow the first name in a fields list to be empty ([#178](http://bugs.hledger.org/178))
- csv: don't validate fields count in skipped lines ([#177](http://bugs.hledger.org/177))


## 2014/5/1 hledger 0.23

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1028)
***command-line fixes and polish, a new accounts
command, and a number of changes to the balance command relating
to --depth, --flat, and multicolumn mode, which I find has made it much
more useful.***

Changes since 0.22.2:

Journal format:

- A # (hash) in column 0 is now also supported for starting a top-level journal comment, like Ledger.
- The "too many missing amounts" error now reminds about the 2-space rule.
- Fix: . (period) is no longer parsed as a valid amount.
- Fix: default commodity directives no longer limit the maximum display precision ([#169](http://bugs.hledger.org/169)).
- Fix: + before an amount is no longer parsed as part of the commodity ([#181](http://bugs.hledger.org/181)).

CLI:

- Command-line help cleanups, layout improvements.
- Descriptions are shown for known add-ons in the command list.
- Command aliases have been simplified.
- Add-ons can now have any of these file extensions:
  none, hs, lhs, pl, py, rb, rkt, sh, bat, com, exe.
- Add-ons are displayed without their file extensions when possible.
- Add-ons with the same name as a built-in command or alias are ignored.
- Fix: add-on detection and invocation now works on windows.
- Fix: add-ons with digits in the name are now found.
- Fix: add-on arguments containing a single quote now work.
- Fix: when -- is used to hide add-on options from the main program,
  it is no longer passed through as an add-on argument.

accounts:

- An accounts command has been added, similar to Ledger's, for listing account names
  in flat or hierarchical mode.

add:

- Tab completion now works at all prompts, and will insert the default if the input area is empty.
- Account and amount defaults are more robust and useful.
- Transactions may also be completed by the enter key, when there are no more default postings.
- Input prompts are displayed in a different colour when supported.

balance:

- Balance reports in flat mode now always show exclusive (subaccount-excluding) balances.
- Balance reports in flat mode with --depth now aggregate deeper accounts at the depth limit instead of excluding them.
- Multicolumn reports in flat mode now support --drop.
- Multicolumn balance reports can now show the account hierarchy with --tree.
- Multicolumn report start/end dates are adjusted to encompass the displayed
  report periods, so the first and last periods are "full" and comparable to the others.
- Fix: zero-balance leaf accounts below a non-zero-balance parent are no longer always shown ([#170](http://bugs.hledger.org/170)).
- Fix: multicolumn reports now support --date2 (cf [#174](http://bugs.hledger.org/174)).

balancesheet, cashflow, incomestatement:

- These commands now support --flat and --drop.

print:

- Tag queries (tag:) will now match a transaction if any of its postings match.

register:

- The --display option has been dropped. To see an accurate running total which
  includes the prior starting balance, use --historical/-H (like balance). 
- With a report interval, report start/end dates are adjusted to encompass the displayed
  periods, so the first and last periods are "full" and comparable to the others.
- Fix: --date2 now works with report intervals (fixes [#174](http://bugs.hledger.org/174)).

Queries:

- The currency/commodity query prefix (sym:) has been renamed to cur:.
- Currency/commodity queries are applied more strongly in register and
  balance reports, filtering out unwanted currencies entirely. Eg
  hledger balance cur:'\$' now reports only the dollar amounts even if
  there are multi-currency transactions or postings.
- Amount queries like amt:N, amt:<N and amt:>N, where N is not 0, now do an unsigned
  comparison of the amount and N. That is, they compare the absolute magnitude.
  To do a signed comparison instead, write N with its sign (eg amt:+N, amt:<+N, amt:>-N).
- Fix: amount queries no longer give false positives on multi-commodity amounts.

Miscellaneous:

- Default report dates now derive from the secondary dates when --date2 is in effect.
- Default report dates now notice any posting dates outside the transaction dates' span.
- Debug output improvements.
- New add-on example: extra/hledger-rewrite.hs, adds postings to matched entries.
- Compatible with GHC 7.2 ([#155](http://bugs.hledger.org/155)) - GHC 7.8, shakespeare 2


## 2014/5/1 hledger-web 0.23

Changes since 0.22.8:

- The --static-root flag has been renamed to --file-url.
- hledger-web now builds with Cabal's default -O, not -O2,
  so may be a little quicker/less memory-hungry to install.


#### 2014/4/29 hledger-web 0.22.8

- allow shakespeare 2.* ([#179](http://bugs.hledger.org/179))

#### 2014/4/17 hledger-web 0.22.7

- add Peter Simons' patch fixing Data.Conduit.Network HostIPv4 error ([#171](http://bugs.hledger.org/171))

#### 2014/4/16 hledger-web 0.22.6

- depend on hledger[-lib] 0.22.2

#### 2014/4/16 hledger 0.22.2

- display years before 1000 with four digits, not three
- avoid pretty-show to build with GHC < 7.4
- allow text 1.1, drop data-pprint to build with GHC 7.8.x

#### 2014/4/15 hledger-web 0.22.5

- allow http-client 0.3.*, fixing cabal install again with GHC <= 7.6 (not yet 7.8)
- use pretty-show only with GHC 7.4+, fixing GHC 7.2 (fixes [#155](http://bugs.hledger.org/155))
- allow warp 2.1, fixing cabal install

#### 2014/2/10 hledger-web 0.22.4

* web: include the right unminified version of jquery.url.js (1.1) to avoid js breakage

#### 2014/2/10 hledger-web 0.22.3

* web: fix version number reported by --version

#### 2014/2/10 hledger-web 0.22.2

New:

* web: new option `--static-root` to set the base url for static files

Improved:

* web: include unminified source of all javascript to help packagers (fixes [#161](http://bugs.hledger.org/161))
* web: work around clang-related build failures with OS X mavericks/XCode 5
* web: allow blaze-html 0.7 (closes [#159](http://bugs.hledger.org/159))


#### 2014/1/6 hledger 0.22.1

- require the latest pretty-show so hledger installation no longer
  needs an upgraded version of happy, and the docs build on hackage

- require regex-tdfa directly instead of regex-compat-tdfa,
  simplifying Debian packaging

## 2013/12/13 hledger 0.22

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/5333)

New:

- balance: with a reporting interval (monthly, yearly etc.), the
  [balance command](manual.html#balance) will now show a multi-column report, showing either
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

  - `hledger-accountnames.hs` - print account names
  - `hledger-balance-csv.hs`  - print a balance report as CSV
  - `hledger-equity.hs`       - print an entry matching all account balances (like ledger)
  - `hledger-print-unique.hs` - print only journal entries unique descriptions
  - `hledger-register-csv.hs` - print a register report as CSV

Improved:

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

Fixed:

- csv: CSV data containing non-ascii characters is now supported

- build with latest versions of dependencies (text, warp, http-conduit etc.)

Release contributors:

Marko Kocić, Max Bolingbroke, and a big welcome to first-time committer John Wiegley! :)

#### 2013/7/10 hledger-web 0.21.3

  - drop yesod-platform dependency, it is not worthwhile. The other
    yesod dependencies are currently without version ranges, so cabal
    install might require --constraint to restrict them in some cases.

#### 2013/6/23 hledger 0.21.3

  - csv: fix wrong application of multiple assignments in a conditional block

#### 2013/6/4 hledger 0.21.2

  - web: fix a build failure

#### 2013/6/3 hledger 0.21.1

  - web: show proper Y-values in register chart (fixes [#122](http://bugs.hledger.org/122))
  - web: avoid trailing commas in register chart values, in case of trouble with IE

## 2013/6/1 hledger 0.21

Bugs fixed:

  - parsing: don't fail when a csv amount has trailing whitespace (fixes [#113](http://bugs.hledger.org/113))
  - web: don't show prices in the accounts sidebar (fixes [#114](http://bugs.hledger.org/114))
  - web: show one line per commodity in charts. Needs more polish, but fixes [#109](http://bugs.hledger.org/109).
  - web: bump yesod-platform dependency to avoid a cabal install failure

Journal reading:

  - balance assertions are now checked after reading a journal

web command:

  - web: support/require yesod 1.2
  - web: show zero-balance accounts in the sidebar (fixes [#106](http://bugs.hledger.org/106))
  - web: use nicer select2 autocomplete widgets in the add form

Documentation and infrastructure:

  - add basic cabal test suites for hledger-lib and hledger

#### 2013/5/4 hledger 0.20.0.1

  * web: require at least version 1.1.7 of yesod-core to avoid a potential build error
  * Update the bug tracker and source repository links on hackage

## 2013/5/1 hledger 0.20

Bugs fixed:

  * balance: a 0.19 regression which showed wrong total balance with `--flat` has been fixed ([#94](http://bugs.hledger.org/94))
  * register: when `--date2` is used, the register is now sorted by the secondary date
  * web: some missing static & template files have been added to the package, fixing cabal-dev and hackage builds ([#97](http://bugs.hledger.org/97), [#98](http://bugs.hledger.org/98))
  * web: some hardcoded static urls have been fixed
  * Dependencies and code have been updated to support the latest
    libraries and GHC versions.  For now, hledger requires GHC 7.2+
    and hledger-web requires GHC 7.4+.

Journal reading:

  - DOS-style line-endings are now also supported in journal and rules files.
  - `!` is now accepted in the status field as well as `*`, like ledger
  - The *actual date* and *effective date* terminology has changed to *primary date* and *secondary date*.
    Use `--date2` to select the secondary date for reports. (`--aux-date` or `--effective` are also accepted
    for ledger and backwards compatibility).
  - Per-posting dates are supported, using hledger tags or ledger's posting date syntax
  - Comment and tag handling has been improved

CSV reading:

  - CSV conversion rules have a simpler, more flexible [syntax](manual.html#csv).
    Existing rules files will need to be updated manually:
    - the filename is now `FILE.csv.rules` instead of `FILE.rules`
    - `FIELD-field N` is now `FIELD %N+1` (or set them all at once with a `fields` rule)
    - `base-currency` is now `currency`
    - `base-account` is now `account1`
    - account-assigning rules:
      add `if` before the list of regexps,
      add indented `account2 ` before the account name
  - parenthesised amounts are parsed as negative

Querying:

  - Use `code:` to match the transaction code (check number) field
  - Use `amt:` followed by `<`, `=` or `>` and a number N to match
    amounts by magnitude. Eg `amt:<0` or `amt:=100`. This works only
    with single-commodity amounts (multi-commodity amounts are
    always matched).
  - `tag:` can now match (exact, case sensitive) tag values. Eg `tag:TAG=REGEXP`.

add comand:

  - Transaction codes and comments (which may contain tags) can now be entered, following a date or amount respectively. ([#45](http://bugs.hledger.org/45))
  - The current entry may be restarted by entering `<` at any prompt. ([#47](http://bugs.hledger.org/47))
  - Entries are displayed and confirmed before they are written to the journal.
  - Default values may be specified for the first entry by providing them as command line arguments.
  - Miscellaneous UI cleanups

register command:

  - The `--related`/`-r` flag shows the other postings in each transaction, like ledger.
  - The `--width`/`-w` option increases or sets the output width.

web command:

  - The web command now also starts a browser, and auto-exits when unused, by default ("local ui mode").
    With `--server`, it keeps running and logs requests to the console ("server mode").
  - Bootstrap is now used for styling and layout
  - A favicon is served
  - The search field is wider
  - yesod devel is now supported; it uses `$LEDGER_FILE` or `~/.hledger.journal`
  - the `blaze_html_0_5` build flag has been reversed and renamed to `blaze_html_0_4`

Add-ons:

  - The hledger-interest and hledger-irr commands have been released/updated.
  - hledger-chart and hledger-vty remain unmaintained and deprecated.

Documentation and infrastructure:

  - The hledger docs and website have been reorganised and updated
  - Manuals for past releases are provided as well as the latest dev version
  - hledger has moved from darcs and darcs hub to git and github (!)
  - The bug tracker has moved from google code to github
  - Feature requests and project planning are now managed on trello
  - A build bot builds against multiple GHC versions on each commit

Release contributors:

- Sascha Welter commissioned register enhancements (--related and --width)
- David Patrick contributed a bounty for add enhancements
- Joachim Breitner added support for ! in status field
- Xinruo Sun provided hledger-web build fixes
- Peter Simons provided hledger-web build fixes, and a build bot
- Marko Kocić provided hledger-web fixes

<!-- Days since last release: 109\ -->
<!-- Commits since last release: 105 -->


#### 2012/11/24 hledger-web 0.19.3

  * web: fix "Prelude.read: no parse" errors with GHC >= 7.6
  * web & lib refactoring

## 2012/11/16 hledger-web 0.19

  * builds with yesod 1.1.3
  * obeys command-line query options at startup again
  * the autogenerated session file is now a dot file
    (.hledger-web_client_session.aes)

#### 2012/11/16 hledger 0.19.1

  * [87](http://bugs.hledger.org/87): fix an arithmetic and transaction balancing bug with multiple
    total-priced amounts ( @@ PRICE )
  * parsing: ignore ledger-style balance assertions ( = BAL ) and fixed
    lot price declarations ( {= PRICE} )


## 2012/10/21 hledger 0.19

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/4190)
***a much faster balance command, and support for the latest GHC and libs.***

  * hledger, hledger-lib: support GHC 7.6 and latest cmdargs, haskeline, split
  * balance report no longer has an O(n^2) slowdown with large numbers of accounts,
    and is generally more speedy. Benchmark on a 2010 macbook:

    ```
    +-------------------------------------------++--------------+--------------+--------+
    |                                           || hledger-0.18 | hledger-0.19 | ledger |
    +===========================================++==============+==============+========+
    | -f data/100x100x10.journal     balance    ||         0.21 |         0.07 |   0.09 |
    | -f data/1000x1000x10.journal   balance    ||        10.13 |         0.47 |   0.62 |
    | -f data/1000x10000x10.journal  balance    ||        40.67 |         0.67 |   1.01 |
    | -f data/10000x1000x10.journal  balance    ||        15.01 |         3.22 |   2.36 |
    | -f data/10000x1000x10.journal  balance aa ||         4.77 |         4.40 |   2.33 |
    +-------------------------------------------++--------------+--------------+--------+
    ```

  * build version is set with CPP instead of cabal-file-th

#### 2012/7/7 hledger 0.18.2

  * web: fix compilation error with -fblaze_html_0_5 flag
  * bump base lower bound to 4.3 to enforce GHC 7 requirement

#### 2012/6/29 hledger 0.18.1

  * register, print: fix reverse ordering of same-day transactions
  * balance: respect all query terms, not just acct
  * combine command-line flags like --depth properly with non-flag query patterns
  * web: don't auto-create a missing journal file at startup
  * stats: list included journal files
  * support tilde (~) in journal and rules file paths
  * expose more utilities from CsvReader
  * remove ensureRulesFile debug trace

## 2012/5/29 hledger 0.18

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/3736)

  * web: hledger-web is now based on yesod 1.0
  * web: fix js error breaking second use of add form ([#72](http://bugs.hledger.org/72))
  * web: make `yesod devel` work
  * the command-line now supports a more powerful [query language](manual.html#queries), consistent with the web UI
  * hledger now fully supports [tags](manual.html#tags) (aka metadata) on both transactions and postings, and querying by tag or tag value
  * new [commands](manual.html#incomestatement) `incomestatement`, `balancesheet`, and `cashflow` provide basic financial statements under certain conditions
  * format conversion is now done on demand, and the convert command has been dropped. So instead of
    `hledger convert FILE.csv` just do `hledger -f FILE.csv print` or any other command.
    You can also pipe any supported format into `hledger -f- CMD` and hledger will try to do the right thing.
  * support for GHC 6.12 has been dropped; this release has been tested with GHC 7.0.4, 7.2.2, and 7.4.1
  * unicode is now handled properly on all supported GHC versions
  * API and internal cleanups

#### 2012/3/3 hledger-web 0.17.1

  * set more upper bounds to fix cabal install issues with latest packages

## 2012/2/1 hledger 0.17

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/3149)
***fixes bugs and updates dependencies***

  * support HP 2011.4.0.0
  * support and require cmdargs 0.9
  * allow non-threaded builds, supporting more debian architectures
  * parsing: give a clearer error when journal file path contains ~
  * parsing: -B/--cost now ignores P historical prices, like ledger
  * parsing: inferred amounts now use the cost commodity if known, like ledger ([#69](http://bugs.hledger.org/69))
  * balance: report differently-priced lots in an account as a single amount, like ledger
  * web: support and require yesod >= 0.9.4
  * web: use the main aeson package again
  * web: fix a regression with dollar signs in hamlet templates
  * web: add form allowed blank account names ([#81](http://bugs.hledger.org/81))
  * chart, vty: hledger-chart and hledger-vty demoted to non-maintained extras for now

#### 2011/10/26 hledger-web 0.16.5

  * web: fix a ghc 6.12 incompatibility in Settings.hs

#### 2011/10/24 hledger-web 0.16.4

  * web: yet another cabal install fix, fix AppConfig name clash

#### 2011/10/4 hledger-web 0.16.3

  * web: another cabal install fix, disable favicon.ico since it's not easily embeddable

#### 2011/10/4 hledger-web 0.16.2

  * web: more cabal install fixes (remove bad path, add routes and models) ([#63](http://bugs.hledger.org/63))

#### 2011/10/4 hledger 0.16.1

  * parsing: show correct line number for posting parse errors ([#67](http://bugs.hledger.org/67))
  * web: declare static files as extra-source-files to fix cabal install ([#63](http://bugs.hledger.org/63))
  * web: add a threaded flag for debian ([#68](http://bugs.hledger.org/68))
  * web: fewer build warnings by default

## 2011/10/1 hledger 0.16

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/521)
***a stability/bugfix/polish release (which may become the pattern for
even-numbered releases in future.)***

  * cli: strip the -- when calling add-on commands, so their options work ([#64](http://bugs.hledger.org/64))
  * cli: hledger ADDON --version now shows add-on command's version
  * cli: only the add and web commands auto-create the journal file
  * cli: give a non-confusing error if LEDGER_FILE contains a literal tilde
  * add: clearer prompts, more validation, use . to end also
  * add: use unix line endings consistently, avoiding parse error on windows ([#51](http://bugs.hledger.org/51))
  * add: avoid excess whitespace between transactions ([#46](http://bugs.hledger.org/46))
  * balance: ledger compatibility fix: don't elide parent accounts with multiple displayed subaccounts
  * convert: always order converted transactions by date
  * convert: rename currency -> base-currency, in-field, out-field -> amount-in-field, amount-out-field
  * convert: give an error, not a zero when date or amount-in-field/amount-out-field parsing fails
  * register: show more useful range of intervals with --empty and a query pattern
  * print, web: always show both dates, ignoring --effective ([#42](http://bugs.hledger.org/42))
  * web: production builds (the default with cabal) have all web content embedded (dev builds use ./static/) ([#63](http://bugs.hledger.org/63))
  * web: update to yesod 0.9
  * web: obey at least some of the general reporting options, like --cost
  * web: adjust the default base url when a custom port is specified
  * web: prevent an infinite redirect when custom base url has a trailing slash
  * web: fix "not:'multi word'" patterns
  * web: hide old title and search form when adding/editing
  * web: adjust --help to indicate command-line arguments are not expected
  * web: don't bother running cli unit tests at startup

#### 2011/9/12 hledger 0.15.2, hledger-web 0.15.3

  * handle multiple filter patterns on the command-line again
  * don't pass an add-on command's name to it as an extra argument
  * don't give a confusing error with -f and no command
  * fix a regression balancing a transaction containing different prices
  * web: fix journal edit form
  * web: fix wrong transaction amount in account register with virtual postings
  * web: fix some invalid html

#### 2011/9/2 hledger 0.15.1, hledger-web 0.15.2

  * fix a parsec 2 incompatibility
  * web: add missing Hledger.Web.Options to cabal file
  * web: tighten up dependencies to reduce build problems

## 2011/9/1 hledger 0.15

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/2748)

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

## 2011/4/22 hledger 0.14

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/383)

  * remove the specific process dependency that caused too many cabal install problems
  * treat arguments as possibly-encoded platform strings, do not assume UTF-8
  * hledger now always reads and writes data as UTF-8, ignoring the system locale ([#34](http://bugs.hledger.org/34))
  * look at the LEDGER_FILE env var for the journal path, otherwise LEDGER, like ledger
  * handle a blank LEDGER_FILE or LEDGER value more gracefully (use the default file path)
  * the default journal file path is now ~/.hledger.journal, to avoid breaking mac filevault ([#41](http://bugs.hledger.org/41))
  * amounts with different prices are now aggregated, like ledger
  * zero amounts now have no sign or commodity, like ledger
  * parsing: assume current year when transaction dates have no year and there is no default year
  * parsing: more careful validation of eg leap years in transaction dates
  * parsing: better international number format support, allowing comma as decimal point and flexible digit groups ([#32](http://bugs.hledger.org/32))
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
  * web: make edit form more cross-browser compatible, fixing it in firefox ([#38](http://bugs.hledger.org/38))
  * web: move hidden add/edit/import forms below main content to help text-mode browsers a bit ([#33](http://bugs.hledger.org/33))

Release contributors: Simon Michael, Dmitry Astapov, Eric Kow, Max Bolingbroke, Omari Norman.
Stats:
137 days, 113 commits, 11 end-user features and 15 end-user bugfixes since last release.
189 unit & functional tests and 59% unit test coverage (hledger, hledger-lib packages).
5540 lines of code (all packages).

## 2010/12/6 hledger 0.13

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/296)
***readline editing and tab completion
from Judah Jacobson, more ledger compatibility, a more robust and
installable web interface, bugfixes, and a much-deliberated package split.***

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
  * add: add the default commodity, if any, to commodity-less amounts ([#26](http://bugs.hledger.org/26))
  * add: misc. commodity/precision/defaults-related bugfixes
  * chart: give a meaningful error message for empty journals
  * chart: update for current Chart lib (0.14)
  * web: support files now live in ./.hledger/web/ and will be auto-created at startup
  * web: page layout is more robust with wide content
  * web: allow editing of included files
  * web: handle multiple filter patterns correctly
  * web: allow single- or double-quoted filter patterns containing spaces
  * web: update for current yesod lib (0.6.*)
  * transaction balancing is now based on display precision ([#23](http://bugs.hledger.org/23))
  * briefer, more informative usage error messages

#### 2010/9/6 hledger 0.12.1

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/272)

  * web: fix account filtering breakage
  * installing: tighten up utf8-string dependency

## 2010/9/5 hledger 0.12

  * web: new, better web ui; accounts are now a permanent sidebar; add form uses auto-completing combo fields
  * installing: fix a build error with parsec 3 ([#22](http://bugs.hledger.org/22))
  * installing: require exactly matching hledger-lib version for more robust builds
  * installing: explicit data-object dependency to ensure hledger and hledger-lib use the same time version
  * installing: explicit hamlet dependency for more robust building
  * installing: build threaded and with warnings
  * installing: drop -fweb610 flag
  * installing: add gtk2hs-buildtools dependency needed to build with -fchart
  * installing: require cabal 1.6 or greater
  * add -D/--daily flag
  * register: with --depth, clip account names or aggregate postings rather than excluding them
  * fix !include with deeply nested directories ([#21](http://bugs.hledger.org/21))
  * fix obscured date parse errors with parsec 3
  * handle unicode better in errors
  * fix a ghc 6.12.3 error when running interpreted

Stats: 50 days and 90 commits since last release, now at 5741
lines of code with 136 tests and 41% unit test coverage.

#### 2010/07/17 hledger 0.11.1

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
***installation and bug fixes and api improvements***

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
***many bugfixes and small improvements, GHC 6.12 support, and a separate library package
to make building (h)ledger-compatible tools easier.***

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
  * add: fix precision handling in default amounts ([#19](http://bugs.hledger.org/19))
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
  * web: show a proper error for a bad date in add form ([#17](http://bugs.hledger.org/17))
  * web: fix for unicode search form values
  * web: fix stack overflow caused by regexpr, and handle requests
    faster ([#14](http://bugs.hledger.org/14))
  * web: look for more-generic browser executables
  * web: more robust browser starting ([#6](http://bugs.hledger.org/6))
  * error message cleanups
  * more tests, refactoring, docs

Stats: 58 days, 2 contributors, 102 commits since last release. Now
at 3983 lines of non-test code, 139 tests, 53% coverage.

## 2010/02/11 hledger 0.8

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/210)
***Bug fixes, refactoring and Hi-Res Graphical Charts.***

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
    ([#18](http://bugs.hledger.org/18))
  * web: fix help link
  * web: use today as default when adding with a blank date
  * web: re-enable account/period fields, they seem to be fixed,
    along with file re-reading ([#16](http://bugs.hledger.org/16))
  * web: get static files from the cabal data dir, or the current
    dir when using make ([#13](http://bugs.hledger.org/13))
  * web: preserve encoding during add, assuming it's utf-8 ([#15](http://bugs.hledger.org/15))
  * fix some non-utf8-aware file handling ([#15](http://bugs.hledger.org/15))
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
    (issues [#3](http://bugs.hledger.org/3), [#4](http://bugs.hledger.org/4))
  * web: allow data entry, react to data file changes, better
    layout, help links, remove histogram command and filter fields for
    now, fix bad localhost redirect, filter form did not work in eg
    firefox (issue [#7](http://bugs.hledger.org/7)), reset link did not work in all browsers
  * parsing: require whitespace between date and status code, allow
    (and ignore) a time in price records, better error messages,
    non-zero exit code on parse failure
  * display non-ascii error messages properly (issue [#5](http://bugs.hledger.org/5))
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

#### 2009/06/22 hledger 0.6.1

  * avoid use of exitSuccess which was breaking ghc 6.8/base 3
    compatibility (issue [#2](http://bugs.hledger.org/2))

## 2009/06/13 hledger 0.6

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/1215)
***Some pre-built binaries are now available. cabal install works on gnu/linux, mac and windows. Hurrah!***

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

#### 2009/05/23 hledger 0.5.1

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

## 2009/04/03 hledger 0.4

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/1097)
***There is also a new website at hledger.org, with screenshots (textual!),
a demo (will it survive!?), and docs (not too many!) ...
I wrote it because I did not want to hack on c++ and because haskell seemed a good fit ...
new happstack-based web interface.***

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

## 2008/10/15 hledger 0.1

[announcement](http://thread.gmane.org/gmane.comp.finance.ledger.general/775)
***I'm pleased to announce the first release of hledger, a command-line
accounting tool similar to John Wiegley's c++ ledger. hledger generates
simple ledger-compatible transaction & account balance reports from a
plain text ledger file. It's simple to use, at least for techies.
This has been my "learning Haskell" project, but I think it's also
useful. It is much less featureful than ledger, and not quite as fast,
but it has the virtue of being fun for haskellers to hack on. I am
documenting the code, the app is simple, and I'm not too far up the
haskell learning curve, so I think other people learning haskell might
enjoy a look. It is currently ~1100 lines of haskell excluding tests.
My thanks to John Wiegley for help with compatibility and for his very
useful ledger tool. I use it (and now, both of them) daily to track time
and money. This is of course a hot topic around our planet. I hope you
find it useful or intriguing.***

Release stats:

  * Contributors: Simon Michael
