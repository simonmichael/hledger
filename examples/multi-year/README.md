# Working with multiple year files

Here are some yearly journal files demonstrating issues and techniques
discussed in the [close docs](https://hledger.org/dev/hledger.html).

`2021.journal`, `2022.journal` and `2023.journal` each have
only an opening balances transaction and some ordinary transactions. 
These are what you'd get if you started a new file each year
using hledger's `close --open` command or Ledger's `equity` command.
These files show correct balances when used individually, but they can't be combined for a multi-year report;
their opening transactions are redundant and will produce nonsense balances.

To solve this, whenever we transition to a new file we can

- add a counterbalancing [closing transaction](https://hledger.org/hledger.html#close) using `close`.

Also (not necessary for basic personal accounting, but if we want to be fully correct), we can ensure all equity is accounted for:

- wherever we have used [@/@@ notation](https://hledger.org/hledger.html#costs) to convert between commodities, 
  add [equivalent equity postings](https://hledger.org/hledger.html#equity-conversion-postings)
- and consolidate revenues and expenses into equity (AKA [retain earnings](https://hledger.org/hledger.html#close)).

Combining these in the right sequence can be tricky, so here's an example of fully migrating to a new file at the end of 2021/start of 2022.
`2021-closed.journal` and `2022-closed.journal` are the result of applying this procedure to the 2021 and 2022 journals.
(They are named differently from the original journals for clarity here; normally you wouldn't change the name.)

First, ensure all equity changes are recorded:

```cli
$ hledger -f 2021.journal print --infer-equity          > 2021-closed.journal   # add explicit equity:conversion postings where needed
$ hledger -f 2021-closed.journal close --retain -e 2022 >> 2021-closed.journal  # retain earnings (transfer RX to E)
```

Next, migrate asset/liability balances from old file to new file.
(If you want to preserve equity balances too, add a [`type:ALE` argument](https://hledger.org/hledger.html#account-types).)
Note how --open is done first and --close second; or you could --close first and `--open not:desc:closing` second:

```cli
$ hledger -f 2021-closed.journal close --open   -e 2022 >> 2022.journal         # migrate balances (add opening txn to 2022.journal)
$ hledger -f 2021-closed.journal close --close  -e 2022 >> 2021-closed.journal  # migrate balances (add closing txn to 2021.journal)
```

Finally, add a tag like this (manually for now) to the closing and opening transactions to make them easier to exclude from reports:

```journal
2021-12-31 closing balances  ; start:2022
```
```journal
2022-01-01 opening balances  ; start:2022
```

## Reports

Now we can confirm that the accounting equation was preserved in 2021,
by checking that the grand total of Assets, Liabilities, and Equity is zero.
To see the end balances, we must exclude the closing balances transaction:

```cli
$ hledger -f 2021-closed.journal bse not:tag:start=2022
Balance Sheet With Equity 2021-12-31

                                 ||   2021-12-31 
=================================++==============
 Assets                          ||              
---------------------------------++--------------
 assets                          ||           21 
---------------------------------++--------------
                                 ||           21 
=================================++==============
 Liabilities                     ||              
---------------------------------++--------------
---------------------------------++--------------
                                 ||              
=================================++==============
 Equity                          ||              
---------------------------------++--------------
 equity:conversion:A-B:A         ||         -1 A 
 equity:conversion:A-B:B         ||          1 B 
 equity:opening/closing balances ||           20 
 equity:retained earnings        || 1, 1 A, -1 B 
---------------------------------++--------------
                                 ||           21 
=================================++==============
 Net:                            ||            0 
```

We can check this for 2022 too, if we add any missing equity changes with --infer-equity and --retain (temporarily):

```cli
$ (hledger -f 2022.journal print --infer-equity; hledger -f 2022.journal close --retain) | hledger -f- bse 
Balance Sheet With Equity 2024-01-19

                          ||   2024-01-19 
==========================++==============
 Assets                   ||              
--------------------------++--------------
 assets                   ||           22 
--------------------------++--------------
                          ||           22 
==========================++==============
 Liabilities              ||              
--------------------------++--------------
--------------------------++--------------
                          ||              
==========================++==============
 Equity                   ||              
--------------------------++--------------
 equity:conversion:A-B:A  ||         -1 A 
 equity:conversion:A-B:B  ||          1 B 
 equity:retained earnings || 1, 1 A, -1 B 
 equity:start             ||           21 
--------------------------++--------------
                          ||           22 
==========================++==============
 Net:                     ||            0 
```

We can see correct balance sheets from any range of files, if we exclude all opening/closing transactions except the first:

```cli
$ hledger bs -Y -f 2021-closed.journal -f 2022-closed.journal -f 2023.journal expr:'tag:start=2021 or not tag:start'
$ hledger bs -Y -f 2021-closed.journal -f 2022-closed.journal                 expr:'tag:start=2021 or not tag:start'
$ hledger bs -Y -f 2022-closed.journal -f 2023.journal                        expr:'tag:start=2022 or not tag:start'
$ hledger bs -Y -f 2021-closed.journal                                        expr:'tag:start=2021 or not tag:start'
$ hledger bs -Y -f 2022-closed.journal                                        expr:'tag:start=2022 or not tag:start'
$ hledger bs -Y -f 2023.journal                                               # unclosed file, no query needed
```

We can see correct income statements from any range of files, if we exclude retain earnings transactions:

```cli
$ hledger is -Y -f 2021-closed.journal -f 2022-closed.journal -f 2023.journal not:desc:retain
$ hledger is -Y -f 2021-closed.journal -f 2022-closed.journal                 not:desc:retain
$ hledger is -Y -f 2022-closed.journal -f 2023.journal                        not:desc:retain
$ hledger is -Y -f 2021-closed.journal                                        not:desc:retain
$ hledger is -Y -f 2022-closed.journal                                        not:desc:retain
$ hledger is -Y -f 2023.journal                                               # unclosed file, no query needed
```

