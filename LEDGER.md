---
title: hledger Compatibility with ledger
---

# Compatibility with ledger

hledger mimics a subset of [ledger 3.x](http://ledger-cli.org), and adds some features of its own.

## Implementation

ledger is written in C++, whereas hledger is written in the
[Haskell](http://haskell.org) programming language. 
Haskell is a 20+-year-old, up-and-coming language that enables
a coding style known as pure functional programming, offering the
promise of more bug-free and maintainable software built in fewer
lines of code. Haskell also provides a more abstracted, portable
platform which can make deployment and installation easier in some
cases. Haskell also brings some new challenges such as managing memory
growth and laziness.

## File format

hledger's file format is mostly identical with ledger's, by design.
Generally, it's easy to keep a journal file that works with both hledger
and ledger if you avoid ledger's most advanced features.

Some ledger features are parsed but ignored, eg:

- automated transactions ( = ... , ~ ... )
- balance assertions ( AMT1=AMT2 )
- fixed lot prices ( {...} )
- historical prices ( P ... )

Some features are not currently parsed and will cause an error, eg:

- balance assignments
- some top level directives like "account"

There can also be subtle differences in parser behaviour, eg
comments may be permissible in different places. 

## Features

We currently support:

- ledger's journal format, mostly
- csv format
- timelog format
- regular journal transactions
- multiple commodities
- fixed prices and price history
- virtual postings
- print, register & balance commands
- filtering by many criteria, with different query syntax
- display expressions containing just a simple date predicate
- some basic output formatting

We do not support:

- automated transactions
- value expressions
- fluctuating prices
- display formats other than `d>[DATE]` or similar
- budget reports

And we add these commands:

- add
- balancesheet
- cashflow
- chart
- incomestatement
- irr
- interest
- vty
- web

ledger options and commands not supported include:

    Basic options:
    -o, --output FILE      write output to FILE
    -i, --init-file FILE   initialize ledger using FILE (default: ~/.ledgerrc)
    -a, --account NAME     use NAME for the default account (useful with QIF)
    
    Report filtering:
    -c, --current          show only current and past entries (not future)
        --period-sort EXPR sort each report period's entries by EXPR
    -L, --actual           consider only actual (non-automated) transactions
        --budget           generate budget entries based on periodic entries
        --add-budget       show all transactions plus the budget
        --unbudgeted       show only unbudgeted transactions
        --forecast EXPR    generate forecast entries while EXPR is true
    -l, --limit EXPR       calculate only transactions matching EXPR
    -t, --amount EXPR      use EXPR to calculate the displayed amount
    -T, --total EXPR       use EXPR to calculate the displayed total
    
    Output customization:
    -n, --collapse         Only show totals in the top-most accounts.
    -P, --by-payee         show summarized totals by payee
    -x, --comm-as-payee    set commodity name as the payee, for reporting
        --dow              show a days-of-the-week report
    -S, --sort EXPR        sort report according to the value expression EXPR
        --head COUNT       show only the first COUNT entries (negative inverts)
        --tail COUNT       show only the last COUNT entries (negative inverts)
        --pager PAGER      send all output through the given PAGER program
    -A, --average          report average transaction amount
    -D, --deviation        report deviation from the average
    -%, --percentage       report balance totals as a percentile of the parent
        --totals           in the "xml" report, include running total
    -j, --amount-data      print only raw amount data (useful for scripting)
    -J, --total-data       print only raw total data
    -y, --date-format STR  use STR as the date format (default: %Y/%m/%d)
        --balance-format      --register-format       --print-format
        --plot-amount-format  --plot-total-format     --equity-format
        --prices-format       --wide-register-format
    
    Commodity reporting:
        --price-db FILE    sets the price database to FILE (def: ~/.pricedb)
    -L, --price-exp MINS   download quotes only if newer than MINS (def: 1440)
    -Q, --download         download price information when needed
    -O, --quantity         report commodity totals (this is the default)
    -V, --market           report last known market value
    -g, --performance      report gain/loss for each displayed transaction
    -G, --gain             report net gain/loss
    
    Commands:
    xml      [REGEXP]...   print matching entries in XML format
    equity   [REGEXP]...   output equity entries for matching accounts
    prices   [REGEXP]...   display price history for matching commodities
    entry DATE PAYEE AMT   output a derived entry, based on the arguments

## Functionality

- hledger recognises description and negative patterns by "desc:"
  and "not:" prefixes, unlike ledger 3's free-form parser

- hledger doesn't require a space before command-line option
  values, eg `-fFILE` or `-f FILE` works

- hledger's weekly reporting intervals always start on mondays

- hledger shows start and end dates of the intervals requested,
  not just the span containing data

- hledger always shows timelog balances in hours

- hledger splits multi-day timelog sessions at midnight

- hledger doesn't track the value of commodities with varying
  price; prices are fixed as of the transaction date

- hledger's output follows the decimal point character, digit grouping,
  and digit group separator character used in the journal.

- hledger print shows amounts for all postings, and shows unit prices for
  amounts which have them. (This means that it does not currently print
  multi-commodity transactions in valid journal format.)

- hledger print ignores the --date2 flag, always showing both dates.
  ledger print shows only the secondary date with --aux-date, but not
  vice versa.

- hledger's default commodity directive (D) sets the commodity for
  subsequent commodityless amounts, and sets that commodity's display
  settings if such an amount is the first seen. ledger uses D only for
  commodity display settings and for the entry command.

- hledger generates a description for timelog sessions, instead of
  taking it from the clock-out entry

- hledger's [include directive](MANUAL.html#including-other-files) does not support
  shell glob patterns (eg `include *.journal` ), which ledger does.

