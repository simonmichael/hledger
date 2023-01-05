## stats

Show journal and performance statistics.

_FLAGS

The stats command displays summary information for the whole journal, or
a matched part of it. With a [reporting interval](#reporting-interval),
it shows a report for each report period. 

At the end, it shows (in the terminal) the overall run time and number of 
transactions processed per second. Note these are approximate and will vary
based on machine, current load, data size, hledger version, haskell lib 
versions, GHC version.. but they may be of interest. The `stats` command's 
run time is similar to that of a single-column balance report.

Example:

```shell
$ hledger stats -f examples/1000x1000x10.journal
Main file                : /Users/simon/src/hledger/examples/1000x1000x10.journal
Included files           : 
Transactions span        : 2000-01-01 to 2002-09-27 (1000 days)
Last transaction         : 2002-09-26 (6995 days ago)
Transactions             : 1000 (1.0 per day)
Transactions last 30 days: 0 (0.0 per day)
Transactions last 7 days : 0 (0.0 per day)
Payees/descriptions      : 1000
Accounts                 : 1000 (depth 10)
Commodities              : 26 (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)
Market prices            : 1000 (A)

Run time                 : 0.12 s
Throughput               : 8342 txns/s
```

This command supports
the [-o/--output-file](hledger.html#output-destination) option
(but not [-O/--output-format](hledger.html#output-format) selection).
