m4_dnl m4 macro definitions used in all hledger package docs.
m4_dnl m4 commands in Shake.hs always load this file first;
m4_dnl m4 macros in package directories could override these.
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
m4_dnl Macros for conditionally including or excluding content based on the format
m4_dnl (man, web or info).
m4_define({{_man_}},         m4_ifdef({{MAN}},{{$1}})    )m4_dnl
m4_define({{_notman_}},      m4_ifdef({{MAN}},,{{$1}})   )m4_dnl
m4_define({{_web_}},         m4_ifdef({{WEB}},{{$1}})    )m4_dnl
m4_define({{_notweb_}},      m4_ifdef({{WEB}},,{{$1}})   )m4_dnl
m4_define({{_info_}},        m4_ifdef({{INFO}},{{$1}})   )m4_dnl
m4_define({{_notinfo_}},     m4_ifdef({{INFO}},,{{$1}})  )m4_dnl
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
: show general or COMMAND help

`--man`
: show general or COMMAND user manual with man

`--info`
: show general or COMMAND user manual with info

`--version`
: show general or ADDONCMD version

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
: disable balance assertion checks (note: does not disable balance assignments)

`-s --strict`
: do extra error checking (check that all posted accounts are declared)

}} )m4_dnl
m4_dnl
m4_define({{_reportingoptions_}}, {{

`-b --begin=DATE`
: include postings/txns on or after this date
  (will be adjusted to preceding subperiod start when using a report interval)

`-e --end=DATE`
: include postings/txns before this date
  (will be adjusted to following subperiod end when using a report interval)

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
: set start date, end date, and/or reporting interval all at once using [period expressions](#period-expressions) syntax

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
: convert amounts to their cost/selling amount at transaction time

`-V --market`
: convert amounts to their market value in default valuation commodities

`-X --exchange=COMM`
: convert amounts to their market value in commodity COMM

`--value`
: convert amounts to cost or market value, more flexibly than -B/-V/-X

`--infer-market-prices`
: use transaction prices (recorded with @ or @@) as additional market prices, as if they were P directives

`--auto`
: apply [automated posting rules](hledger.html#auto-postings) to modify transactions.

`--forecast`
: generate future transactions from [periodic transaction](hledger.html#periodic-transactions) rules, for the next 6 months or till report end date.
In hledger-ui, also make ordinary future transactions visible.

`--color=WHEN (or --colour=WHEN)`
: Should color-supporting commands use ANSI color codes in text output.
: 'auto' (default): whenever stdout seems to be a color-supporting terminal.
: 'always' or 'yes': always, useful eg when piping output into 'less -R'.
: 'never' or 'no': never.
: A NO_COLOR environment variable overrides this.

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
hledger is a reliable, cross-platform set of programs for tracking money, time, or any other commodity,
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

A typical value is `~/DIR/YYYY.journal`, where DIR is a version-controlled finance directory
and YYYY is the current year. Or `~/DIR/current.journal`, where current.journal is a symbolic
link to YYYY.journal.

On Mac computers, you can set this and other environment variables in a more thorough way
that also affects applications started from the GUI (say, an Emacs dock icon).
Eg on MacOS Catalina I have a `~/.MacOSX/environment.plist` file containing
```
{
  "LEDGER_FILE" : "~/finance/current.journal"
}
```
To see the effect you may need to `killall Dock`, or reboot.


}} )m4_dnl
m4_dnl
m4_dnl The _FLAGS marker is used in generating command help (see
m4_dnl CliOptions.parseCommandDoc), but should be removed when generating manuals.
m4_dnl Just one underscore here, so pandoc doesn't strip them
m4_dnl ($FLAGS$ and =FLAGS= didn't work, not sure why).
m4_define({{_FLAGS}}, {{}})m4_dnl
m4_dnl
