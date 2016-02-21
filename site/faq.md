* toc

# Frequently asked questions

## hledger & Ledger

### History

I discovered John Wiegley's [Ledger](http://ledger-cli.org) in 2006,
and was very happy to find this efficient command-line reporting tool with a transparent data format.

Initially, I used it to generate time reports for my job.
Before long I wanted that to work differently - splitting sessions at day boundaries, reporting in hours, etc.
John had got busy elsewhere and the Ledger project now stalled, with unfixed bugs, wrong documentation and a confusing release situation persisting for a long time.
I did what I could to help build momentum, reporting bugs, supporting newcomers, and contributing a new domain and website.
But, I didn't want to spend time learning C++.

I was learning Haskell, which I did want to spend time in.
I felt Ledger could be implemented well and, in the long run, more efficiently in that language,
which has some compelling advantages such as lower maintenance costs.
<!-- ([eg](http://neilmitchell.blogspot.com/2016/02/selling-haskell-in-pub.html)). -->
<!-- (It encourages the coding style known as pure functional programming, -->
<!-- allowing more bug-free, concise and maintainable software. It provides -->
<!-- a more abstracted, portable platform making installation easier. It is -->
<!-- attractive for contributors to work on.) -->
I urgently needed a reliable accounting tool that I enjoyed using.
I also wanted to see what I could do to reduce roadbumps and confusion for newcomers.

I couldn't expect John to start over - at that time he was not the Haskell fan he is now!
So in 2007 I began experimenting.
I built a toy parser in a few different languages, and it was easiest in Haskell.
I kept tinkering.
Goals included:

- to get better at Haskell by building something useful to me,
- to learn how well Haskell could work for real-world applications,
- and eventually: to provide a new implementation focussing more on
  ease of use, absence of user-visible bugs, and high-quality documentation and web presence.
  Also to experiment with new user interfaces, APIs, etc.

Before too long I had a tool that was useful to me. With Ledger still installed,
and by maintaining high compatibility, I now had two tools  with different strengths,
each providing a comparison for the other in case of confusion or suspected bugs,
which was itself quite valuable.

Happily, the Ledger project later revived and has attracted new active contributors.
I have remained active in that community, sharing discoveries and
design discussions, and we have seen many ideas travelling in both directions.
hledger shared #ledger's IRC channel until 2014, when I added
[#hledger](irc://irc.freenode.net/#hledger) to allow us more space.

I think having independent but compatible implementations has been
quite helpful for troubleshooting, exploring the design space, and
growing the "Ledger-likes" community.
My other projects in that direction include
the [ledger-cli.org](http://ledger-cli.org) site,
[LedgerTips](http://twitter.com/LedgerTips),
IRC support on #ledger,
and now [plaintextaccounting.org](http://plaintextaccounting.org).

### Features

Compared to Ledger, hledger builds quickly and has a complete and
accurate manual, an easier report query syntax, multi-column balance
reports, better depth limiting, an interactive data entry assistant,
and optional web and curses interfaces.

Compared to hledger, Ledger has additional power-user features such as
periodic and modifier transactions, budget reports, and the built in
value expressions language, and it remains faster and more memory
efficient (for now).

We currently support:

- Ledger's journal format, mostly
- csv format
- timelog format
- regular journal transactions
- multiple commodities
- fixed transaction prices
- varying market prices
- virtual postings
- some basic output formatting
- the print, register & balance commands
- report filtering, using a different query syntax

We do not support:

- automated transactions
- value expressions
- budget reports

And we add these commands:

- add
- balancesheet
- cashflow
- chart
- incomestatement
- irr
- interest
- ui
- web

### File formats

hledger's journal file format is mostly identical with Ledger's, by design.
Generally, it's easy to keep a journal file that works with both hledger
and Ledger if you avoid Ledger's and hledger's more specialised syntax
(or keep it in separate files which you include only when appropriate).

Some Ledger syntax is parsed but ignored (such as
[automated transactions](http://ledger-cli.org/3.0/doc/ledger3.html#Automated-Transactions)
and [periodic transactions](http://ledger-cli.org/3.0/doc/ledger3.html#Periodic-Transactions)).
Some features are not currently parsed and will cause an error, eg
Ledger's more recent top-level directives. There can also be subtle
differences in parser behaviour, such as with
[hledger comments](manual.html#comments) vs [Ledger comments](http://ledger-cli.org/3.0/doc/ledger3.html#Commenting-on-your-Journal),
or [balance assertions](manual.html#assertions-and-ordering).

### Functional differences

- hledger recognises description and negative patterns by "desc:"
  and "not:" prefixes, unlike Ledger 3's free-form parser

- hledger does not require a space between command-line flags and their values,
  eg `-fFILE` works as well as `-f FILE`

- hledger's weekly reporting intervals always start on mondays

- hledger shows start and end dates of the intervals requested,
  not just the span containing data

- hledger always shows timelog balances in hours

- hledger splits multi-day timelog sessions at midnight by default (Ledger does this with an option)

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
  shell glob patterns (eg `include *.journal` ), which Ledger's does.

- hledger's include directive works only in journal files, and currently can only include
  journal files, not eg timelog or CSV files

- when checking [balance assertions](manual.html#balance-assertions)
  hledger sorts the account's postings first by date and then (for
  postings with the same date) by parse order. Ledger goes strictly by
  parse order.

- Ledger allows amounts to have a fixed lot price (the {} syntax ?)
  and a regular price in any order (and uses whichever appears
  first). hledger requires the fixed lot price to come last (and
  ignores it).

### Future ?

There is a [ledger4](https://github.com/ledger/ledger4) repo on
github; this is John's 2012/2013 rewrite of some parts of Ledger 3,
including the parser, in Haskell. We have a plan to add this parser to
hledger in 2015/2016, increasing its ability to read Ledger's files.


## UI surprises

### Why does it complain about missing amounts even though I wrote one ?

This is an easy mistake at first. This journal entry:
```journal
1/1
  a 1
  b
```
will give a parse error (`...can't have more than one real posting with no amount...`).

There must always be at least two spaces between the account name and amount. So instead, it should be:
```journal
1/1
  a  1
  b
```


### Why do some amounts appear on their own line with no account name ?

When hledger needs to show a multi-commodity amount, each commodity is displayed on its own line, one above the other (like Ledger).

Here are some examples. With this journal, the implicit balancing amount drawn from the `b` account will be a multicommodity amount (a euro and a dollar):
```journal
2015/1/1
    a         EUR 1
    a         USD 1
    b
```
the `print` command shows the `b` posting's amount on two lines, bottom-aligned:
```shell
$ hledger -f t.j print
2015/01/01
    a         USD 1
    a         EUR 1
             EUR -1  ; <-
    b        USD -1  ; <- a euro and a dollar is drawn from b
```
the `balance` command shows that both `a` and `b` have a multi-commodity balance (again, bottom-aligned):
```shell
$ hledger -f t.j balance
               EUR 1     ; <-
               USD 1  a  ; <- a's balance is a euro and a dollar
              EUR -1     ; <-
              USD -1  b  ; <- b's balance is a negative euro and dollar
--------------------
                   0
```
while the `register` command shows (top-aligned, this time) a multi-commodity running total after the second posting,
and a multi-commodity amount in the third posting:
```shell
$ hledger -f t.j register --width 50
2015/01/01       a             EUR 1         EUR 1
                 a             USD 1         EUR 1  ; <- the running total is now a euro and a dollar        
                                             USD 1  ;                                                        
                 b            EUR -1                ; <- the amount posted to b is a negative euro and dollar
                              USD -1             0  ;
```

Newer reports like [multi-column balance reports](manual.html#multicolumn-balance-reports) show multi-commodity amounts on one line instead, comma-separated.
Although wider, this seems clearer and we should probably use it more:
```shell
$ hledger -f t.j balance --yearly
Balance changes in 2015:

   ||           2015 
===++================
 a ||   EUR 1, USD 1 
 b || EUR -1, USD -1 
---++----------------
   ||              0 
```

You will also see amounts without a corresponding account name if you remove too many account name segments with [`--drop`](manual.html#balance):
```shell
$ hledger -f t.j balance --drop 1
               EUR 1  
               USD 1  
              EUR -1  
              USD -1  
--------------------
                   0
```
