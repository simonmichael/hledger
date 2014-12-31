* toc

# Frequently asked questions

## hledger and Ledger

### How does hledger relate to Ledger ?

hledger was inspired by John Wiegley's
[Ledger](http://ledger-cli.org).  It is a friendly, mostly compatible
rewrite of Ledger in Haskell, begun in 2007 (Ledger started in 2003),
focussing on robustness, usability, ease of development, long-term
maintainability, and new experiments such as the
[web interface](manual.html#web).  It currently lacks some of Ledger's
power-user features and speed.  hledger stays compatible with Ledger
wherever possible, so that with a little care you can use both tools
on the same data files.

Longer answer: I was a happy Ledger user and contributor for some
time, then became too dissatisfied with bugs, missing/wrong
documentation and a long period of stagnation. I also wanted to try
implementing Ledger's brilliant design in the Haskell programming
language and ecosystem, which I believe has compelling advantages. I
try to build on Ledger's experience to make hledger easier to learn
and use, better documented, more appealing to work on; and to provide
alternate user interfaces (interactive, curses, web) to make it useful
to more people.

hledger builds quickly and has a complete and accurate manual, an
easier report query syntax, a data entry assistant, an optional web
interface (which often works on Ledger files too), and multi-column
balance reports.  Ledger has additional power-user features (capital
gains tracking, periodic and modifier transactions, budget reports,
custom value expressions..)  and it remains faster and more memory
efficient (for now!).

The two projects collaborate freely.  For some time we shared the
[#ledger](irc://irc.freenode.net/#ledger) IRC channel; in 2014 I added
a dedicated [#hledger](irc://irc.freenode.net/#hledger) channel.
I give back to Ledger by providing infrastructure
([ledger-cli.org](http://ledger-cli.org)), IRC support, [LedgerTips](http://twitter.com/LedgerTips) etc.

### And Ledger 4 ?

There is also a [ledger4](https://github.com/ledger/ledger4) on github; this is 
John's own rewrite of the core of
Ledger 3 in haskell. It's an early library prototype, not a usable tool.
Perhaps some day hledger or something like it would use this as its foundation. 

### File format differences ?

hledger's file format is mostly identical with Ledger's, by design.
Generally, it's easy to keep a journal file that works with both hledger
and Ledger if you avoid Ledger's and hledger's more specialised syntax
(or keep it in separate files which you include only when appropriate).

Some Ledger syntax is parsed but ignored (such as
[automated transactions](http://ledger-cli.org/3.0/doc/ledger3.html#Automated-Transactions), [periodic transactions](http://ledger-cli.org/3.0/doc/ledger3.html#Periodic-Transactions), and
[historical prices](manual.html#historical-prices)).
Some features are not currently parsed and will cause an error, eg
Ledger's more recent top-level directives. There can also be subtle
differences in parser behaviour, such as with
[hledger comments](manual.html#comments) vs [Ledger comments](http://ledger-cli.org/3.0/doc/ledger3.html#Commenting-on-your-Journal),
or [balance assertions](manual.html#assertions-and-ordering).

### Feature differences ?

hledger mimics a subset of [Ledger 3.x](http://ledger-cli.org), and adds some features of its own.

We currently support:

- Ledger's journal format, mostly
- csv format
- timelog format
- regular journal transactions
- multiple commodities
- fixed prices
- virtual postings
- print, register & balance commands
- filtering by many criteria, with different query syntax
- display expressions containing just a simple date predicate
- some basic output formatting

We do not support:

- automated transactions
- value expressions
- fluctuating prices and historical price records
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

### Option/command differences ?

Ledger options and commands not supported include:
```
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
```

### Other functionality differences ?

- hledger recognises description and negative patterns by "desc:"
  and "not:" prefixes, unlike Ledger 3's free-form parser

- hledger does not require a space between command-line flags and their values,
  eg `-fFILE` works as well as `-f FILE`

- hledger's weekly reporting intervals always start on mondays

- hledger shows start and end dates of the intervals requested,
  not just the span containing data

- hledger always shows timelog balances in hours

- hledger splits multi-day timelog sessions at midnight by default (Ledger does this with an option)

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

- hledger's default commodity directive (D) sets the commodity to be
  used for subsequent commodityless amounts, and also sets that
  commodity's display settings if such an amount is the first
  seen. Ledger uses D only for commodity display settings and for the
  entry command.

- hledger generates a description for timelog sessions, instead of
  taking it from the clock-out entry

- hledger's [include directive](manual.html#including-other-files) does not support
  shell glob patterns (eg `include *.journal` ), which Ledger does.

- when checking [balance assertions](manual.html#balance-assertions)
  hledger sorts the account's postings first by date and then (for
  postings with the same date) by parse order. Ledger goes strictly by
  parse order.

- Ledger allows amounts to have a
  [fixed lot price](manual.html#fixed-lot-prices) and a regular price in any
  order (and uses whichever appears first). hledger requires the fixed
  lot price to come last (and ignores it).

### Implementation differences ?

Ledger is written in C++, whereas hledger is written in [Haskell](http://haskell.org).
Haskell is a highly regarded up-and-coming programming language that enables
a coding style known as pure functional programming, offering the
promise of more bug-free and maintainable software built in fewer
lines of code. Haskell also provides a more abstracted, portable
platform which can make deployment and installation easier in some
cases.
