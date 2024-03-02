## stats

Show journal and performance statistics.

_FLAGS

The stats command shows summary information for the whole journal, or
a matched part of it. With a [reporting interval](#reporting-interval),
it shows a report for each report period. 

The default output is fairly impersonal, though it reveals the main file name.
With `-v/--verbose`, more details are shown, like file paths, included files,
and commodity names.

It also shows some run time statistics:

- elapsed time
- throughput: the number of transactions processed per second
- live:  the peak memory in use by the program to do its work
- alloc: the peak memory allocation from the OS as seen by GHC.
  Measuring this externally, eg with GNU time, is more accurate;
  usually that will be a larger number; sometimes (with swapping?) smaller.

The `stats` command's run time is similar to that of a balance report.

Example:

```cli
$ hledger stats -f examples/1ktxns-1kaccts.journal 
Main file           : .../1ktxns-1kaccts.journal
Included files      : 0
Txns span           : 2000-01-01 to 2002-09-27 (1000 days)
Last txn            : 2002-09-26 (7827 days ago)
Txns                : 1000 (1.0 per day)
Txns last 30 days   : 0 (0.0 per day)
Txns last 7 days    : 0 (0.0 per day)
Payees/descriptions : 1000
Accounts            : 1000 (depth 10)
Commodities         : 26
Market prices       : 1000
Runtime stats       : 0.12 s elapsed, 8266 txns/s, 4 MB live, 16 MB alloc
```

This command supports
the [-o/--output-file](hledger.html#output-destination) option
(but not [-O/--output-format](hledger.html#output-format)).
