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
  -h --help                 show command line help
     --tldr                 show command examples with tldr
     --info                 show the hledger manual with info
     --man                  show the hledger manual with man
     --version              show version information
```
}} )m4_dnl
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
