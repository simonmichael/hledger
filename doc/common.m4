m4_dnl m4 helpers and common text used in hledger docs (the manuals, mainly).
m4_dnl When Shake.hs docs scripts use m4, they always load this file first.
m4_dnl
m4_dnl `m4_dnl` ignores the rest of the line, including the newline.
m4_dnl Avoid blank lines or stray newlines in this file, unless inside a macro.
m4_dnl Add single quotes where needed to help vscode's highlighting. '
m4_dnl
m4_dnl
m4_dnl Set {{ }} as our m4 quoting delimiters:
m4_dnl
m4_changequote({{,}})m4_dnl
m4_dnl
m4_dnl
m4_dnl Easier macros for including/excluding content based on output format:
m4_dnl
m4_define({{_man_}},         m4_ifdef({{MANFORMAT}},{{$1}})    )m4_dnl
m4_define({{_notman_}},      m4_ifdef({{MANFORMAT}},,{{$1}})   )m4_dnl
m4_define({{_info_}},        m4_ifdef({{INFOFORMAT}},{{$1}})   )m4_dnl
m4_define({{_notinfo_}},     m4_ifdef({{INFOFORMAT}},,{{$1}})  )m4_dnl
m4_define({{_web_}},         m4_ifdef({{WEBFORMAT}},{{$1}})    )m4_dnl
m4_define({{_notweb_}},      m4_ifdef({{WEBFORMAT}},,{{$1}})   )m4_dnl
m4_dnl
m4_dnl
m4_dnl Author name for pandoc metadata in manuals, affecting man pages. Currently left blank.
m4_dnl
m4_define({{_author_}}, {{}})m4_dnl
m4_dnl
m4_dnl
m4_dnl A general hledger description used at the start of the manuals.
m4_dnl
m4_define({{_hledgerdescription_}}, {{
hledger is a robust, user-friendly, cross-platform set of programs
for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1),
and largely interconvertible with beancount(1). }} )m4_dnl
m4_dnl
m4_dnl
m4_dnl A short description of hledger's input files, used in the manuals. '
m4_dnl
m4_define({{_inputfiles_}},
{{reads from (and appends to) a journal file specified by the `LEDGER_FILE` environment variable
(defaulting to `$HOME/.hledger.journal`); or you can specify files with `-f` options.
It can also read timeclock files, timedot files, or any CSV/SSV/TSV file with a date field.}} )m4_dnl
m4_dnl
m4_dnl
m4_dnl The above text plus a pointer to the more detailed hledger manual section.
m4_dnl Used in the ui/web manuals.
m4_dnl
m4_define({{_inputfileswithptr_}},
{{_inputfiles_
(See hledger(1) -> Input for details.)}} )m4_dnl
m4_dnl
m4_dnl
m4_dnl Bug reporting info for the manuals' BUGS sections. '
m4_dnl
m4_define({{_reportbugs_}},
{{We welcome bug reports in the hledger issue tracker (<https://bugs.hledger.org>),
or on the hledger chat or mail list (<https://hledger.org/support>).}} )m4_dnl
m4_dnl
m4_dnl
m4_dnl A copy of the general options help shown by `hledger --help`. Used in the three manuals.
m4_dnl To make changes: update the flag definitions at top of CliOptions.hs,
m4_dnl build hledger, and manually copy most of that build's --help output to here. '
m4_dnl Do this always before release, and more often if needed to update
m4_dnl the manual within dev builds and at https://hledger.org/dev/hledger.html#options.
m4_dnl
m4_define({{_generaloptions_}}, {{
```
General input/data transformation flags:
  -f --file=[FMT:]FILE      Read data from FILE, or from stdin if FILE is -,
                            inferring format from extension or a FMT: prefix.
                            Can be specified more than once. If not specified,
                            reads from $LEDGER_FILE or $HOME/.hledger.journal.
     --rules=RULESFILE      Use rules defined in this rules file for
                            converting subsequent CSV/SSV/TSV files. If not
                            specified, uses FILE.csv.rules for each FILE.csv.
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
     --txn-balancing=...    how to check that transactions are balanced:
                            'old':   use global display precision
                            'exact': use transaction precision (default)
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
  -E --empty                Show zero items, which are normally hidden.
                            In hledger-ui & hledger-web, do the opposite.
     --depth=DEPTHEXP       if a number (or -NUM): show only top NUM levels
                            of accounts. If REGEXP=NUM, only apply limiting to
                            accounts matching the regular expression.
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
     --pretty[=YN]          Use box-drawing characters in text output? Can be
                            'y'/'yes' or 'n'/'no'.
                            If YN is specified, the equals is required.

General help flags:
  -h --help                 show command line help
     --tldr                 show command examples with tldr
     --info                 show the manual with info
     --man                  show the manual with man
     --version              show version information
     --debug=[1-9]          show this much debug output (default: 1)
     --pager=YN             use a pager when needed ? y/yes (default) or n/no
     --color=YNA --colour   use ANSI color ? y/yes, n/no, or auto (default)
```
}} )m4_dnl '
