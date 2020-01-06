stats\
Show some journal statistics.

_FLAGS

The stats command displays summary information for the whole journal, or
a matched part of it. With a [reporting interval](#reporting-interval),
it shows a report for each report period.

Example:

```shell
$ hledger stats
Main journal file        : /src/hledger/examples/sample.journal
Included journal files   : 
Transactions span        : 2008-01-01 to 2009-01-01 (366 days)
Last transaction         : 2008-12-31 (2333 days ago)
Transactions             : 5 (0.0 per day)
Transactions last 30 days: 0 (0.0 per day)
Transactions last 7 days : 0 (0.0 per day)
Payees/descriptions      : 5
Accounts                 : 8 (depth 3)
Commodities              : 1 ($)
Market prices            : 12 ($)
```

This command also supports
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) selection.
