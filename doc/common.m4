m4_dnl m4 macro definitions used in all hledger package docs.
m4_dnl m4 commands in Shake.hs always load this file first;
m4_dnl m4 macros in package directories could override these.
m4_dnl
m4_dnl Don't leave any blank lines here, outside of macros.
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
m4_define({{_man_}},         m4_ifdef({{MANFORMAT}},{{$1}})    )m4_dnl
m4_define({{_notman_}},      m4_ifdef({{MANFORMAT}},,{{$1}})   )m4_dnl
m4_define({{_web_}},         m4_ifdef({{WEBFORMAT}},{{$1}})    )m4_dnl
m4_define({{_notweb_}},      m4_ifdef({{WEBFORMAT}},,{{$1}})   )m4_dnl
m4_define({{_info_}},        m4_ifdef({{INFOFORMAT}},{{$1}})   )m4_dnl
m4_define({{_notinfo_}},     m4_ifdef({{INFOFORMAT}},,{{$1}})  )m4_dnl
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
m4_dnl
m4_define({{_generaloptions_}}, {{
```
General input/data transformation flags:
  -f --file=FILE            Read data from FILE, or from stdin if -. Can be
                            specified more than once. If not specified, reads
                            from $LEDGER_FILE or $HOME/.hledger.journal.
     --rules-file=RULEFILE  Use conversion rules from this file for
                            converting subsequent CSV/SSV/TSV files. If not
                            specified, uses FILE.rules for each such FILE.
     --alias=A=B|/RGX/=RPL  transform account names from A to B, or by
                            replacing regular expression matches
     --auto                 generate extra postings by applying auto posting
                            rules ("=") to all transactions
     --forecast[=PERIOD]    Generate extra transactions from periodic rules
                            ("~"), from after the latest ordinary transaction
                            until 6 months from now. Or, during the specified
                            PERIOD (the equals is required). Auto posting rules
                            will also be applied to these transactions. In
                            hledger-ui, also make future-dated transactions
                            visible at startup.
  -I --ignore-assertions    don't check balance assertions by default
     --infer-costs          infer conversion equity postings from costs
     --infer-equity         infer costs from conversion equity postings
     --infer-market-prices  infer market prices from costs
     --pivot=TAGNAME        use a different field or tag as account names
  -s --strict               do extra error checks (and override -I)
     --verbose-tags         add tags indicating generated/modified data

General output/reporting flags (supported by some commands):
  -b --begin=DATE           include postings/transactions on/after this date
  -e --end=DATE             include postings/transactions before this date
                            (with a report interval, will be adjusted to
                            following subperiod end)
  -D --daily                multiperiod report with 1 day interval
  -W --weekly               multiperiod report with 1 week interval
  -M --monthly              multiperiod report with 1 month interval
  -Q --quarterly            multiperiod report with 1 quarter interval
  -Y --yearly               multiperiod report with 1 year interval
  -p --period=PERIODEXP     set begin date, end date, and/or report interval,
                            with more flexibility
     --today=DATE           override today's date (affects relative dates)
     --date2                match/use secondary dates instead (deprecated)
  -U --unmarked             include only unmarked postings/transactions
  -P --pending              include only pending postings/transactions
  -C --cleared              include only cleared postings/transactions
                            (-U/-P/-C can be combined)
  -R --real                 include only non-virtual postings
     --depth=NUM            or -NUM: show only top NUM levels of accounts
  -E --empty                Show zero items, which are normally hidden.
                            In hledger-ui & hledger-web, do the opposite.
  -B --cost                 show amounts converted to their cost/sale amount
  -V --market               Show amounts converted to their value at period
                            end(s) in their default valuation commodity.
                            Equivalent to --value=end.
  -X --exchange=COMM        Show amounts converted to their value at period
                            end(s) in the specified commodity.
                            Equivalent to --value=end,COMM.
     --value=WHEN[,COMM]    show amounts converted to their value on the
                            specified date(s) in their default valuation
                            commodity or a specified commodity. WHEN can be:
                            'then':     value on transaction dates
                            'end':      value at period end(s)
                            'now':      value today
                            YYYY-MM-DD: value on given date
  -c --commodity-style=S    Override a commodity's display style.
                            Eg: -c '$1000.' or -c '1.000,00 EUR'
     --color=YN --colour    Use ANSI color codes in text output? Can be:
                            'yes' or 'always',
                            'no' or 'never' (a NO_COLOR env var forces this),
                            'auto' (the default: when using a color terminal).
     --pretty[=YN]          Use box-drawing characters in text output? Can be
                            'yes' (the default argument for --pretty) or 'no'.
                            If YN is specified, the equals is required.
     --debug=[N]            show debug output (levels 1-9, default: 1)

General help flags:
     --version              show version information
  -h --help                 show command-line help for hledger [or COMMAND]
     --info                 show the hledger manual [for COMMAND] with info
     --man                  show the hledger manual [for COMMAND] with man
```
}} )m4_dnl
m4_dnl
m4_dnl Too much hassle for now:
m4_dnl m4_define({{_helpoptions_}}, {{
m4_dnl 
m4_dnl `--version`
m4_dnl : show version information
m4_dnl 
m4_dnl `-h --help`
m4_dnl : show command-line help for hledger [or COMMAND]
m4_dnl 
m4_dnl `--info`
m4_dnl : show the hledger manual [for COMMAND] with info
m4_dnl 
m4_dnl `--man`
m4_dnl : show the hledger manual [for COMMAND] with man
m4_dnl 
m4_dnl }} )m4_dnl
m4_dnl
m4_dnl m4_define({{_inputoptions_}}, {{
m4_dnl 
m4_dnl `-f FILE --file=FILE`
m4_dnl : use a different input file. For stdin, use - (default: `$LEDGER_FILE` or `$HOME/.hledger.journal`)
m4_dnl 
m4_dnl `--rules-file=RULESFILE`
m4_dnl : Conversion rules file to use when reading CSV (default: FILE.rules)
m4_dnl 
m4_dnl `--separator=CHAR`
m4_dnl : Field separator to expect when reading CSV (default: ',')
m4_dnl 
m4_dnl `--alias=OLD=NEW`
m4_dnl : rename accounts named OLD to NEW
m4_dnl 
m4_dnl `--pivot FIELDNAME`
m4_dnl : use some other field or tag for the account name
m4_dnl 
m4_dnl `-I --ignore-assertions`
m4_dnl : disable balance assertion checks (note: does not disable balance assignments)
m4_dnl 
m4_dnl `-s --strict`
m4_dnl : do extra error checking (check that all posted accounts are declared)
m4_dnl 
m4_dnl }} )m4_dnl
m4_dnl m4_dnl
m4_dnl m4_define({{_reportingoptions_}}, {{
m4_dnl 
m4_dnl `-b --begin=DATE`
m4_dnl : include postings/txns on or after this date
m4_dnl   (will be adjusted to preceding subperiod start when using a report interval)
m4_dnl 
m4_dnl `-e --end=DATE`
m4_dnl : include postings/txns before this date
m4_dnl   (will be adjusted to following subperiod end when using a report interval)
m4_dnl 
m4_dnl `-D --daily`
m4_dnl : multiperiod/multicolumn report by day
m4_dnl 
m4_dnl `-W --weekly`
m4_dnl : multiperiod/multicolumn report by week
m4_dnl 
m4_dnl `-M --monthly`
m4_dnl : multiperiod/multicolumn report by month
m4_dnl 
m4_dnl `-Q --quarterly`
m4_dnl : multiperiod/multicolumn report by quarter
m4_dnl 
m4_dnl `-Y --yearly`
m4_dnl : multiperiod/multicolumn report by year
m4_dnl 
m4_dnl `-p --period=PERIODEXP`
m4_dnl : set start date, end date, and/or reporting interval all at once using [period expressions](hledger.html#period-expressions) syntax
m4_dnl 
m4_dnl `--date2`
m4_dnl : match the secondary date instead (see command help for other effects)
m4_dnl 
m4_dnl `--today=DATE`
m4_dnl : override today's date (affects relative smart dates, for tests/examples)
m4_dnl 
m4_dnl `-U --unmarked`
m4_dnl : include only unmarked postings/txns (can combine with -P or -C)
m4_dnl 
m4_dnl `-P --pending`
m4_dnl : include only pending postings/txns
m4_dnl 
m4_dnl `-C --cleared`
m4_dnl : include only cleared postings/txns
m4_dnl 
m4_dnl `-R --real`
m4_dnl : include only non-virtual postings
m4_dnl 
m4_dnl `-NUM --depth=NUM`
m4_dnl : hide/aggregate accounts or postings more than NUM levels deep
m4_dnl 
m4_dnl `-E --empty`
m4_dnl : show items with zero amount, normally hidden (and vice-versa in hledger-ui/hledger-web)
m4_dnl 
m4_dnl `-B --cost`
m4_dnl : convert amounts to their cost/selling amount at transaction time
m4_dnl 
m4_dnl `-V --market`
m4_dnl : convert amounts to their market value in default valuation commodities
m4_dnl 
m4_dnl `-X --exchange=COMM`
m4_dnl : convert amounts to their market value in commodity COMM
m4_dnl 
m4_dnl `--value`
m4_dnl : convert amounts to cost or market value, more flexibly than -B/-V/-X
m4_dnl 
m4_dnl `--infer-equity`
m4_dnl : infer conversion equity postings from costs
m4_dnl 
m4_dnl `--infer-costs`
m4_dnl : infer costs from conversion equity postings
m4_dnl 
m4_dnl `--infer-market-prices`
m4_dnl : use costs as additional market prices, as if they were P directives
m4_dnl 
m4_dnl `--forecast`
m4_dnl : generate transactions from [periodic rules](hledger.html#periodic-transactions),
m4_dnl : between the latest recorded txn and 6 months from today,
m4_dnl : or during the specified PERIOD (= is required).
m4_dnl : Auto posting rules will be applied to these transactions as well.
m4_dnl : Also, in hledger-ui make future-dated transactions visible.
m4_dnl 
m4_dnl `--auto`
m4_dnl : generate extra postings by applying [auto posting rules](hledger.html#auto-postings) to all txns (not just forecast txns)
m4_dnl 
m4_dnl `--verbose-tags`
m4_dnl : add visible tags indicating transactions or postings which have been generated/modified
m4_dnl 
m4_dnl `--commodity-style`
m4_dnl : Override the commodity style in the output for the specified commodity. For example 'EUR1.000,00'.
m4_dnl 
m4_dnl `--color=WHEN (or --colour=WHEN)`
m4_dnl : Should color-supporting commands use ANSI color codes in text output.
m4_dnl : 'auto' (default): whenever stdout seems to be a color-supporting terminal.
m4_dnl : 'always' or 'yes': always, useful eg when piping output into 'less -R'.
m4_dnl : 'never' or 'no': never.
m4_dnl : A NO_COLOR environment variable overrides this.
m4_dnl 
m4_dnl `--pretty[=WHEN]`
m4_dnl : Show prettier output, e.g. using unicode box-drawing characters.
m4_dnl : Accepts 'yes' (the default) or 'no' ('y', 'n', 'always', 'never' also work).
m4_dnl : If you provide an argument you must use '=', e.g. '--pretty=yes'.
m4_dnl 
m4_dnl When a reporting option appears more than once in the command line, the last one takes precedence.
m4_dnl 
m4_dnl Some reporting options can also be written as [query arguments](hledger.html#queries).
m4_dnl 
m4_dnl }} )m4_dnl
m4_dnl
m4_dnl m4_define({{_generaloptions_}}, {{
m4_dnl 
m4_dnl _inputoptions_
m4_dnl 
m4_dnl _reportingoptions_
m4_dnl 
m4_dnl _helpoptions_
m4_dnl 
m4_dnl _optionnotes_
m4_dnl 
m4_dnl }} )m4_dnl
m4_dnl
m4_dnl A standard description of hledger.
m4_define({{_hledgerdescription_}}, {{
hledger is a robust, user-friendly, cross-platform set of programs
for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1),
and largely interconvertible with beancount(1). }} )m4_dnl
m4_dnl
m4_dnl A standard description of where hledger reads data from.
m4_define({{_inputfiles_}},
{{reads from (and appends to) a journal file specified by the `LEDGER_FILE` environment variable
(defaulting to `$HOME/.hledger.journal`); or you can specify files with `-f` options.
It can also read timeclock files, timedot files, or any CSV/SSV/TSV file with a date field.}} )m4_dnl
m4_dnl
m4_dnl Like the above, with an added pointer to the hledger manual.
m4_define({{_inputfileswithptr_}},
{{_inputfiles_
(See hledger(1) -> Input for details.)}} )m4_dnl
m4_dnl
m4_dnl BUGS boilerplate
m4_define({{_reportbugs_}},
{{We welcome bug reports in the hledger issue tracker (shortcut: <http://bugs.hledger.org>),
or on the #hledger chat or hledger mail list (<https://hledger.org/support>).}} )m4_dnl
m4_dnl
m4_dnl The _FLAGS marker is used in generating command help (see
m4_dnl CliOptions.parseCommandDoc), but should be removed when generating manuals.
m4_dnl Just one underscore here, so pandoc doesn't strip them
m4_dnl ($FLAGS$ and =FLAGS= didn't work, not sure why).
m4_define({{_FLAGS}}, {{}})m4_dnl
m4_dnl
