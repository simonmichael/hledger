import\
Read new transactions added to each FILE since last run, and add them to
the main journal file. Or with --dry-run, just print the transactions 
that would be added.

_FLAGS_

The input files are specified as arguments - no need to write -f before each one.
So eg to add new transactions from all CSV files to the main journal, it's just: 
`hledger import *.csv`

New transactions are detected in the same way as print --new: 
by assuming transactions are always added to the input files in increasing date order,
and by saving `.latest.FILE` state files.

The --dry-run output is in journal format, so you can filter it, eg 
to see only uncategorised transactions: 

```shell
$ hledger import --dry ... | hledger -f- print unknown --ignore-assertions
```
