m4_changequote({{,}})m4_dnl
m4_dnl
m4_define({{_include_}},     m4_defn({{m4_include}})              )m4_dnl
m4_define({{_man_}},         m4_ifdef({{MAN}},{{$1}})             )m4_dnl
m4_define({{_web_}},         m4_ifdef({{WEB}},{{$1}})             )m4_dnl
m4_define({{_webseparate_}}, m4_ifdef({{WEB && SEPARATE}},{{$1}}) )m4_dnl
m4_define({{_webcombined_}}, m4_ifdef({{WEB && COMBINED}},{{$1}}) )m4_dnl
m4_dnl
m4_define({{_author_}}, {{}})m4_dnl
m4_define({{_monthyear_}}, {{May 2016}})m4_dnl
m4_define({{_currentrelease_}}, {{0.27}})m4_dnl
m4_define({{_version_}}, {{0.28}})m4_dnl
m4_define({{_versions_}},
<div class="versions">
version:
<a href="$1.html">dev</a>
| <a href="doc/_currentrelease_()/manual.html{{#}}m4_patsubst($1,{{hledger-}})">_currentrelease_()</a>
</div>)m4_dnl
m4_dnl
m4_define({{_toc_}}, {{* toc}})m4_dnl
m4_dnl
m4_dnl _table_({{
m4_dnl | cell1 | cell2 ...
m4_dnl | cell1 | cell2 ...
m4_dnl ...
m4_dnl }})
m4_define({{_table2_}}, {{
|
|-|-$1}})m4_dnl
m4_dnl
m4_define({{_col2_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-6">$1</div>
<div class="col-sm-6">$2</div>
</div>
</div>}})m4_dnl
m4_dnl
m4_define({{_col3_}},
{{<div class="container-fluid">
<div class="row">
<div class="col-sm-4">$1</div>
<div class="col-sm-4">$2</div>
<div class="col-sm-4">$3</div>
</div>
</div>}})m4_dnl
m4_dnl
m4_define({{_shell_}},     {{```shell$1```}}          )m4_dnl
m4_define({{_shellbold_}}, {{```{.shell .bold}$1```}} )m4_dnl
m4_define({{_journal_}},   {{```journal$1```}}        )m4_dnl
m4_define({{_csv_}},       {{```csv$1```}}            )m4_dnl
m4_define({{_rules_}},     {{```rules$1```}}          )m4_dnl
m4_define({{_timeclock_}}, {{```timeclock$1```}}      )m4_dnl
m4_define({{_timedot_}},   {{```timedot$1```}}        )m4_dnl
m4_dnl
m4_define({{_generaloptions_}}, {{

`-h`
: show general usage (or after COMMAND, the command's usage)

`--help`
: show the current program's manual as plain text (or after an add-on COMMAND, the add-on's manual)

`--man`
: show the current program's manual with man

`--info`
: show the current program's manual with info

`--version`
: show version

`--debug[=N]`
: show debug output (levels 1-9, default: 1)

`-f FILE --file=FILE`
: use a different input file. For stdin, use -

`--rules-file=RULESFILE`
: Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
: display accounts named OLD as NEW

`-I --ignore-assertions`
: ignore any failing balance assertions in the journal

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
: set start date, end date, and/or reporting interval all at once (overrides the flags above)

`--date2`
: show, and match with -b/-e/-p/date:, secondary dates instead

`-C --cleared`
: include only cleared postings/txns

`--pending`
: include only pending postings/txns

`-U --uncleared`
: include only uncleared (and pending) postings/txns

`-R --real`
: include only non-virtual postings

`--depth=N`
: hide accounts/postings deeper than N

`-E --empty`
: show items with zero amount, normally hidden

`-B --cost`
: show amounts in their cost price's commodity

`--pivot TAG`
: will transform the journal before any other processing by replacing the account name of every posting having the tag TAG with content VALUE by the  account name "TAG:VALUE".
: The TAG will only match if it is a full-length match. The pivot will only happen if the TAG is on a posting, not if it is on the transaction. If the tag value is a multi:level:account:name the new account name will be "TAG:multi:level:account:name".

}} )m4_dnl
m4_dnl
m4_define({{_hledgerdescription_}}, {{
hledger is a cross-platform program for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1). }} )m4_dnl
m4_dnl
m4_define({{_files_}},
{{data from one or more files in hledger journal, timeclock, timedot, or CSV format
specified with `-f`, or `$LEDGER_FILE`,
or `$HOME/.hledger.journal` (on windows, perhaps `C:/Users/USER/.hledger.journal`).}})m4_dnl
m4_dnl
m4_define({{_LEDGER_FILE_}}, {{
**LEDGER_FILE**
The journal file path when not specified with `-f`.
Default: `~/.hledger.journal` (on windows, perhaps `C:/Users/USER/.hledger.journal`).
}} )m4_dnl
