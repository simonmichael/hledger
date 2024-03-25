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

### Skipping

`import` tries to import only the transactions which are new since the last import, "skipping over" any that it saw last time.
So if your bank's CSV includes the last three months of data, you can download and `import` it every month (or week, or day) 
and only the new transactions will be imported each time.

It works as follows. For each imported `FILE` (usually CSV, but they could be any of hledger's input formats):

- It tries to find the latest date seen previously, by reading it from a hidden `.latest.FILE` in the same directory.
- Then it processes `FILE`, ignoring any transactions on or before the "latest seen" date.

And after a successful import, it updates the `.latest.FILE`(s) for next time (unless `--dry-run` was used).

This is a simple system that works fairly well for transaction data.
It assumes:

1. new items always have the newest dates
2. item dates are stable across successive CSV downloads
3. the order of same-date items is stable across CSV downloads

These are true of most CSV files representing transactions, or true enough.
If you have a bank whose CSV dates or ordering occasionally changes,
you can reduce the chance of this happening in new transactions by importing more often
(and in old transactions it doesn't matter).

Note, `import` avoids reprocessing the same dates across successive runs,
but it does not detect transactions that are duplicated within a single run.
I'll call these "skipping" and "deduplication" respectively.

So for example, say you downloaded but did not import `bank.1.csv`, and later downloaded `bank.2.csv` with overlapping data.
Then you should not import both of them at once (`hledger import bank.1.csv bank.2.csv`), as the overlapping data would appear twice and not be deduplicated.
Instead, import them one at a time (`hledger import bank.1.csv; hledger import bank.2.csv`), and the second import will skip the overlapping data.

Normally you can ignore the `.latest.*` files, 
but if needed, you can delete them (to make all transactions unseen),
or construct/modify them (to catch up to a certain date).
The format is just a single ISO-format date (`YYYY-MM-DD`), possibly repeated on multiple lines.
It means "I have seen transactions up to this date, and this many of them occurring on that date".

([`hledger print --new`](#print) also uses and updates these `.latest.*` files, but it is less often used.)

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

### Commodity display styles

Imported amounts will be formatted according to the canonical [commodity styles](hledger.html#commodity-display-style)
(declared or inferred) in the main journal file.
