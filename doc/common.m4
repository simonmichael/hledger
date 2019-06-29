m4_dnl m4 macro definitions used in all hledger package docs.
m4_dnl m4 commands in Shake.hs always load this file first.
m4_dnl PACKAGE/defs.m4 can add to/override these definitions.
m4_dnl
m4_dnl "m4_dnl" ignores the rest of the line (including newline).
m4_dnl
m4_dnl {{ }} will be our quoting delimiters
m4_changequote({{,}})m4_dnl
m4_dnl
m4_dnl _MACRO_ will be the naming convention for our macros
m4_define({{_include_}},     m4_defn({{m4_include}})              )m4_dnl
m4_dnl
m4_dnl Author to show in man pages.
m4_define({{_author_}}, {{}})m4_dnl
m4_dnl
m4_dnl Macros for conditionally including format-specific content
m4_dnl $1 is the manual's web slug: hledger, hledger-ui, journal, csv etc.
m4_define({{_man_}},         m4_ifdef({{MAN}},{{$1}})             )m4_dnl
m4_define({{_web_}},         m4_ifdef({{WEB}},{{$1}})             )m4_dnl
m4_define({{_webseparate_}}, m4_ifdef({{WEB && SEPARATE}},{{$1}}) )m4_dnl
m4_define({{_webcombined_}}, m4_ifdef({{WEB && COMBINED}},{{$1}}) )m4_dnl
m4_dnl
m4_dnl Links to dev and recent release versions of a manual.
m4_dnl The actual links are inserted dynamically by site.js, static generation was much too painful.
m4_dnl There should be a " (dev)" suffix here except when generating release docs.
m4_define({{_docversionlinks_}},m4_dnl
This doc is for version **_version_**
m4_dnl comment (dev) for releases, uncomment between releases:
m4_dnl (dev)
.
<span class="docversions">m4_dnl
</span>)m4_dnl
m4_dnl
m4_dnl Insert a table of contents marker, which doc build scripts will populate.
m4_define({{_toc_}},{{
\$TOC\$
}})m4_dnl
m4_dnl
m4_dnl Helpers for generating table markup.
m4_dnl _table_({{
m4_dnl | cell1 | cell2 ...
m4_dnl | cell1 | cell2 ...
m4_dnl ...
m4_dnl }})
m4_define({{_table2_}}, {{
|
|-|-$1}})m4_dnl
m4_dnl
m4_dnl Two side-by-side columns.
m4_define({{_col2_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-6">$1</div>
<div class="col-sm-6">$2</div>
</div>
</div>}})m4_dnl
m4_dnl
m4_dnl Three side-by-side columns.
m4_define({{_col3_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-4">$1</div>
<div class="col-sm-4">$2</div>
<div class="col-sm-4">$3</div>
</div>
</div>}})m4_dnl
m4_dnl
m4_dnl Various kinds of code block.
m4_define({{_shell_}},     {{```shell$1```}}          )m4_dnl
m4_define({{_shellbold_}}, {{```{.shell .bold}$1```}} )m4_dnl
m4_define({{_journal_}},   {{```journal$1```}}        )m4_dnl
m4_define({{_csv_}},       {{```csv$1```}}            )m4_dnl
m4_define({{_rules_}},     {{```rules$1```}}          )m4_dnl
m4_define({{_timeclock_}}, {{```timeclock$1```}}      )m4_dnl
m4_define({{_timedot_}},   {{```timedot$1```}}        )m4_dnl
m4_dnl
m4_dnl Various lists of common command line options.
m4_dnl Should be kept synced with CliOptions.hs etc.
m4_define({{_helpoptions_}}, {{

`-h --help`
: show general usage (or after COMMAND, command usage)

`--version`
: show version

`--debug[=N]`
: show debug output (levels 1-9, default: 1)

}} )m4_dnl
m4_dnl
m4_define({{_inputoptions_}}, {{

`-f FILE --file=FILE`
: use a different input file. For stdin, use - (default: `$LEDGER_FILE` or `$HOME/.hledger.journal`)

`--rules-file=RULESFILE`
: Conversion rules file to use when reading CSV (default: FILE.rules)

`--separator=CHAR`
: Field separator to expect when reading CSV (default: ',')

`--alias=OLD=NEW`
: rename accounts named OLD to NEW

`--anon`
: anonymize accounts and payees

`--pivot FIELDNAME`
: use some other field or tag for the account name

`-I --ignore-assertions`
: ignore any failing balance assertions

}} )m4_dnl
m4_dnl
m4_define({{_reportingoptions_}}, {{

`-b --begin=DATE`
: include postings/txns on or after this date

`-e --end=DATE`
: include postings/txns before this date

`-D --daily`
: multiperiod/multicolumn report by day

`-W --weekly`
: multiperiod/multicolumn report by week

`-M --monthly`
: multiperiod/multicolumn report by month

`-Q --quarterly`
: multiperiod/multicolumn report by quarter

`-Y --yearly`
: multiperiod/multicolumn report by year

`-p --period=PERIODEXP`
: set start date, end date, and/or reporting interval all at once using [period expressions](manual.html#period-expressions) syntax

`--date2`
: match the secondary date instead (see command help for other effects)

`-U --unmarked`
: include only unmarked postings/txns (can combine with -P or -C)

`-P --pending`
: include only pending postings/txns

`-C --cleared`
: include only cleared postings/txns

`-R --real`
: include only non-virtual postings

`-NUM --depth=NUM`
: hide/aggregate accounts or postings more than NUM levels deep

`-E --empty`
: show items with zero amount, normally hidden (and vice-versa in hledger-ui/hledger-web)

`-B --cost`
: convert amounts to their cost at transaction time
(using the [transaction price](journal.html#transaction-prices), if any)

`-V --value`
: convert amounts to their market value on the report end date
(using the most recent applicable [market price](journal.html#market-prices), if any)

`--auto`
: apply [automated posting rules](journal.html#auto-postings-transaction-modifiers) to modify transactions.

`--forecast`
: apply [periodic transaction](journal.html#periodic-transactions) rules to generate future transactions, to 6 months from now or report end date.


When a reporting option appears more than once in the command line, the last one takes precedence.

Some reporting options can also be written as [query arguments](#queries).

}} )m4_dnl
m4_dnl
m4_define({{_generaloptions_}}, {{

_inputoptions_

_reportingoptions_

_helpoptions_

_optionnotes_

}} )m4_dnl
m4_dnl
m4_dnl A standard description of hledger.
m4_define({{_hledgerdescription_}}, {{
hledger is a cross-platform program for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1). }} )m4_dnl
m4_dnl
m4_dnl A standard description of where hledger reads data from.
m4_define({{_files_}},
{{data from one or more files in hledger journal, timeclock, timedot, or CSV format
specified with `-f`, or `$LEDGER_FILE`,
or `$HOME/.hledger.journal` (on windows, perhaps `C:/Users/USER/.hledger.journal`).}})m4_dnl
m4_dnl
m4_dnl A standard description of the LEDGER_FILE environment variable.
m4_define({{_LEDGER_FILE_}}, {{
**LEDGER_FILE**
The journal file path when not specified with `-f`.
Default: `~/.hledger.journal` (on windows, perhaps `C:/Users/USER/.hledger.journal`).
}} )m4_dnl
m4_dnl
m4_dnl Dummy macro to strip _FLAGS_ marker in command docs.
m4_define({{_FLAGS_}}, {{}})m4_dnl
m4_dnl
