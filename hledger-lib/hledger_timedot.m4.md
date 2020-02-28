% hledger_timedot(5) hledger _version_
% _author_
% _monthyear_

_man_({{
# NAME
}})

Timedot - hledger's human-friendly time logging format

_man_({{
# DESCRIPTION
}})

Timedot is a plain text format for logging dated, categorised quantities (of time, usually), supported by hledger.
It is convenient for approximate and retroactive time logging,
eg when the real-time clock-in/out required with a timeclock file is too precise or too interruptive.
It can be formatted like a bar chart, making clear at a glance where time was spent.

Though called "timedot", this format is read by hledger as commodityless quantities,
so it could be used to represent dated quantities other than time.
In the docs below we'll assume it's time.

A timedot file contains a series of day entries.
A day entry begins with a non-indented hledger-style
[simple date](journal.html#simple-dates) (Y-M-D, Y/M/D, Y.M.D..)
Any additional text on the same line is used as a transaction description for this day.

This is followed by optionally-indented timelog items for that day, one per line.
Each timelog item is a note, usually a hledger:style:account:name representing a time category,
followed by two or more spaces, and a quantity.
Each timelog item generates a hledger transaction.

Quantities can be written as:

- time dots: a sequence of dots (.) representing quarter hours.
  Spaces may optionally be used for grouping.
  Eg: .... ..

- an integral or decimal number, representing hours.
  Eg: 1.5

- an integral or decimal number immediately followed by a unit symbol
  `s`, `m`, `h`, `d`, `w`, `mo`, or `y`, representing seconds, minutes, hours, days
  weeks, months or years respectively.
  Eg: 90m.
  The following equivalencies are assumed, currently:
  1m = 60s, 1h = 60m, 1d = 24h, 1w = 7d, 1mo = 30d, 1y=365d.

There is some flexibility allowing notes and todo lists to be kept
right in the time log, if needed:

- Blank lines and lines beginning with `#` or `;` are ignored.

- Lines not ending with a double-space and quantity are parsed as
  items taking no time, which will not appear in balance reports by
  default. (Add -E to see them.)

- Org headline prefixes (stars followed by at least one space, at the
  start of a line) are ignored, so a timedot file can also be an org
  outline. Emacs org mode users can use these to add structure and
  control which parts of the file are visible.

Examples:

```timedot
# on this day, 6h was spent on client work, 1.5h on haskell FOSS work, etc.
2016/2/1
inc:client1   .... .... .... .... .... ....
fos:haskell   .... ..
biz:research  .

2016/2/2
inc:client1   .... ....
biz:research  .
```

```timedot
2016/2/3
inc:client1   4
fos:hledger   3
biz:research  1
```

```timedot
* Time log
** 2020-01-01
*** adm:time  .
*** adm:finance  .
```

```timedot
** 2020-02-29
*** DONE
0700 yoga
*** UNPLANNED
*** BEGUN
hom:chores
 cleaning  ...
 water plants
  outdoor - one full watering can
  indoor - light watering
*** TODO
adm:planning: trip
*** LATER

```

Reporting:

```shell
$ hledger -f t.timedot print date:2016/2/2
2016-02-02 *
    (inc:client1)          2.00

2016-02-02 *
    (biz:research)          0.25
```
```shell
$ hledger -f t.timedot bal --daily --tree
Balance changes in 2016-02-01-2016-02-03:

            ||  2016-02-01d  2016-02-02d  2016-02-03d 
============++========================================
 biz        ||         0.25         0.25         1.00 
   research ||         0.25         0.25         1.00 
 fos        ||         1.50            0         3.00 
   haskell  ||         1.50            0            0 
   hledger  ||            0            0         3.00 
 inc        ||         6.00         2.00         4.00 
   client1  ||         6.00         2.00         4.00 
------------++----------------------------------------
            ||         7.75         2.25         8.00 
```

I prefer to use period for separating account components.
We can make this work with an [account alias](journal.html#rewriting-accounts):

```timedot
2016/2/4
fos.hledger.timedot  4
fos.ledger           ..
```
```shell
$ hledger -f t.timedot --alias /\\./=: bal date:2016/2/4
                4.50  fos
                4.00    hledger:timedot
                0.50    ledger
--------------------
                4.50
```

Here is a
[sample.timedot](https://raw.github.com/simonmichael/hledger/master/examples/sample.timedot).
<!-- to download and some queries to try: -->

<!-- ```shell -->
<!-- $ hledger -f sample.timedot balance                               # current time balances -->
<!-- $ hledger -f sample.timedot register -p 2009/3                    # sessions in march 2009 -->
<!-- $ hledger -f sample.timedot register -p weekly --depth 1 --empty  # time summary by week -->
<!-- ``` -->
