## import

Read new transactions added to each FILE provided as arguments since
last run, and add them to the journal.
Or with --dry-run, just print the transactions that would be added.
Or with --catchup, just mark all of the FILEs' current transactions 
as imported, without importing them.

_FLAGS

This command may append new transactions to the main journal file (which should be in journal format).
Existing transactions are not changed.
This is one of the few hledger commands that writes to the journal file (see also `add`).

Unlike other hledger commands, with `import` the journal file is an output file,
and will be modified, though only by appending (existing data will not be changed).
The input files are specified as arguments, so to import one or more
CSV files to your main journal, you will run `hledger import bank.csv`
or perhaps `hledger import *.csv`.

Note you can import from any file format, though CSV files are the
most common import source, and these docs focus on that case.

### Date skipping

`import` tries to import only the transactions which are new since the last import, ignoring any that it has seen in previous runs.
So if your bank's CSV includes the last three months of data, you can download and `import` it every month (or week, or day) 
and only the new transactions will be imported each time.

It works as follows: for each imported `FILE`,

- It tries to read the latest date previously seen, from `.latest.FILE` in the same directory
- Then it processes `FILE`, ignoring transactions on or before that date

And after a successful import, unless `--dry-run` was used, it updates the `.latest.FILE`(s) for next time.
This is a simple system that works for most real-world CSV files;
it assumes the following are true, or true enough:

1. the name of the input file is stable across successive downloads
2. new items always have the newest dates
3. item dates are stable across downloads
4. the order of same-date items is stable across downloads.

Tips:

- To help ensure a stable file name, remember you can use a CSV rules file as an input file.

- If you have a bank whose CSV dates or ordering occasionally change,
  you can reduce the chance of this happening in new transactions by importing more often.
  (If it happens in old transactions, that's harmless.)

Note this is just one kind of "deduplication": not reprocessing the same dates across successive runs.
`import` doesn't detect other kinds of duplication, such as 
the same transaction appearing multiple times within a single run,
or a new transaction that looks identical to a transaction already in the journal.
(Because these can happen legitimately in real-world data.)

Here's a situation where you need to run `import` with care:
say you download but forget to import `bank.1.csv`, and a week later you download `bank.2.csv` with some overlapping data.
You should not process both of these as a single import (`hledger import bank.1.csv bank.2.csv`),
because the overlapping transactions would not be deduplicated.
Instead, import one file at a time, using the same filename each time:

```cli
$ mv bank.1.csv bank.csv; hledger import bank.csv
$ mv bank.2.csv bank.csv; hledger import bank.csv
```

Normally you don't need to think about `.latest.*` files, 
but you can create or modify them to catch up to a certain date,
or delete them to mark all transactions as new.
Their format is a single ISO-format `YYYY-MM-DD` date, optionally repeated on multiple lines,
meaning "I have seen the transactions before this date, and this many of them on this date".

[`hledger print --new`](#print) also uses and updates these `.latest.*` files, but it is less often used.

Related: [CSV > Working with CSV > Deduplicating, importing](#deduplicating-importing).


### Import testing

With `--dry-run`, the transactions that will be imported are printed
to the terminal, without updating your journal or state files.
The output is valid journal format, like the print command, so you can re-parse it.
Eg, to see any importable transactions which CSV rules have not categorised:

```cli
$ hledger import --dry bank.csv | hledger -f- -I print unknown
```

or (live updating):

```cli
$ ls bank.csv* | entr bash -c 'echo ====; hledger import --dry bank.csv | hledger -f- -I print unknown'
```

Note: when importing from multiple files at once, it's currently possible for
some .latest files to be updated successfully, while the actual import fails
because of a problem in one of the files, leaving them out of sync (and causing
some transactions to be missed).
To prevent this, do a --dry-run first and fix any problems before the real import.

### Importing balance assignments

Entries added by import will have their posting amounts made explicit (like `hledger print -x`).
This means that any [balance assignments](https://hledger.org/hledger.html#balance-assignments) in imported files must be evaluated;
but, imported files don't get to see the main file's account balances.
As a result, importing entries with balance assignments
(eg from an institution that provides only balances and not posting amounts)
will probably generate incorrect posting amounts.
To avoid this problem, use print instead of import:

```cli
$ hledger print IMPORTFILE [--new] >> $LEDGER_FILE
```

(If you think import should leave amounts implicit like print does,
please test it and send a pull request.)

### Import and commodity styles

Amounts in entries added by import will be formatted according to the journal's canonical [commodity styles](#commodity-display-style),
as declared by [`commodity` directives](#commodity-directive) or inferred from the journal's amounts.

Related: [CSV > Amount decimal places](#amount-decimal-places).
