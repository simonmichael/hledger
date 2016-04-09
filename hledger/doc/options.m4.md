# OPTIONS

To see general help and the command list: `hledger --help` or `hledger`

To see all options available with a command: `hledger COMMAND --help`

Except for the General options below, options must be written after
COMMAND, not before it.

Also, when invoking external add-on commands, their options must be
written after a double hyphen. (Or, you can invoke the external command
directly.) Eg:
```{.shell .bold}
$ hledger ui -- --register cash
$ hledger-ui --register cash
```

Options and command arguments can be intermixed. Arguments are usually
interpreted as a search query which filters the data, see QUERIES.

## General flags:

These can appear anywhere in the command line.

`-h --help`
: show general help or (after command) command help

`--version`
: show version information

`-f FILE --file=FILE`
: use a different input file. For stdin, use -

`--rules-file=RULESFILE`
: Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
: display accounts named OLD as NEW

`--ignore-assertions`
: ignore any failing balance assertions in the journal

`--debug=N`
: : show debug output if N is 1-9 (default: 0)

## Common reporting flags:

These are supported by most commands, where applicable.
They must be written after the command name.
Additional command-specific flags are described in COMMANDS below.

`-b --begin=DATE              `
: include postings/txns on or after this date

`-e --end=DATE                `
: include postings/txns before this date

`-D --daily                   `
: multiperiod/multicolumn report by day

`-W --weekly                  `
: multiperiod/multicolumn report by week

`-M --monthly                 `
: multiperiod/multicolumn report by month

`-Q --quarterly               `
: multiperiod/multicolumn report by quarter

`-Y --yearly                  `
: multiperiod/multicolumn report by year

`-p --period=PERIODEXP        `
: set start date, end date, and/or reporting interval all at once (overrides the flags above)

`--date2 --aux-date`
: use postings/txns' secondary dates instead

`-C --cleared                 `
: include only cleared postings/txns

`--pending`
: include only pending postings/txns

`-U --uncleared               `
: include only uncleared (and pending) postings/txns

`-R --real                    `
: include only non-virtual postings

`--depth=N`
: hide accounts/postings deeper than N

`-E --empty                   `
: show empty/zero things which are normally omitted

`-B --cost                    `
: show amounts in their cost price's commodity

If a reporting option is repeated, the last one takes precedence. Eg -p jan -p
feb is equivalent to -p feb.

