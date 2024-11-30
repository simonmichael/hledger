## import

Import new transactions from one or more data files to the main journal.

```flags
Flags:
     --catchup              just mark all transactions as already imported
     --dry-run              just show the transactions to be imported
```

This command detects new transactions in each FILE argument since it was last run, 
and appends them to the main journal.

Or with `--dry-run`, it just prints a preview of the new transactions that would be added.

Or with `--catchup`, it just marks all of the FILEs' current transactions as already imported.

This is one of the few hledger commands that writes to the journal file (see also `add`).
It only appends to the journal; existing entries will not be changed.

The data files are specified as arguments, so to import one or more
CSV files to your main journal, you will run 
`hledger import bank1.csv ...` or perhaps `hledger import *.csv`.
Note you can import from any input file format, eg journal files;
but CSV/SSV/TSV files are the most common import source.

The import destination is the main journal file,
which can be specified in the usual way with `$LEDGER_FILE` or `-f/--file`.
It should be in journal format.

### Overlap detection

You could convert and append new bank transactions without `import`, by doing `hledger -f bank.csv print >>$LEDGER_FILE`.
But the `import` command has a useful feature: it tries to avoid re-importing transactions it has already seen on previous runs. 
This means you don't have to worry about overlapping data in successive downloads of your bank CSV.
Just download and import it as often as you like, and only the new transactions will be imported each time.

We don't call this "deduplication", because it's generally not possible to reliably detect duplicates in bank CSV.
Instead, `import` remembers the latest date processed from each CSV file (saving it in a hidden file).
This is a simple mechanism that works well for most real-world CSV, where:

1. the data file name is stable (does not change) across imports
2. the item dates are stable across imports
3. the order of same-date items is stable across imports
4. the newest items have the newest dates

(Occasional minor instabilities in item dates/order are usually harmless.
You can reduce the chance of disruption by downloading and importing more often.)

Here's how overlap detection works in detail:

For each `FILE` being imported with `hledger import FILE ...`,

1. hledger reads a `.latest.FILE` file in the same directory, if any.
  This file contains the latest record date previously imported from FILE, in YYYY-MM-DD format.
  If multiple records with that date were imported, the date is repeated on N lines.

2. hledger reads records from FILE.
  If a latest date was found in step 1, it skips the records before and on that date
  (or the first N records on that date).

3. After a successful import of all FILE arguments, without error and without `--dry-run`,
   hledger saves the new latest dates in each FILE's `.latest.FILE` for next time.

If overlap detection does go wrong, it's not too hard to recover from:

- You'll notice it when you try to reconcile your hledger balances with your bank.
- `hledger print FILE.csv` will show all recently downloaded transactions.
  Compare these with your journal and copy/paste if needed.
- You can manually update or remove the `.latest.FILE`, or use `--catchup`.
- You can use `--dry-run` to preview what will be imported.
- Download and import more often, eg twice a week, at least while you are learning.
  It's easier to review and troubleshoot when there are fewer transactions.

<!--
Related: 
[CSV > Working with CSV > Deduplicating, importing](#deduplicating-importing)
-->


### Import preview

With `--dry-run`, the transactions that will be imported are printed
to standard output as a preview, without updating  your journal or .latest files.

The output is valid journal format, like the print command, so hledger can re-parse it.
So you could check for new transactions not yet categorised by your CSV rules, like so:

```cli
$ hledger import --dry-run bank.csv | hledger -f- -I print unknown
```

And you could watch this while you update your rules file, eg like so:

```cli
$ watchexec -- 'hledger import --dry-run data.csv | hledger -f- -I print unknown'
```

There is another command which does the same kind of overlap detection: [`hledger print --new`](#print).
But generally `import` or `import --dry-run` are used instead.

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

### Import special cases

If you have a download whose file name does vary, you could rename it after download.
Or you could use a [`source` rule](#source) with a suitable glob pattern,
and import from the .rules file instead of the data file.

Here's a situation where you need to run `import` with care:
say you download `bank.csv`, but forget to import it or delete it.
And next month you download it again. This time your web browser may save it as `bank (2).csv`.
So now each of these may have data not included in the other.
And a `source` rule with a glob pattern would match only the most recent file.
So in this case you should import from each one in turn, in the correct order, taking care to use the same filename each time:

```cli
$ hledger import bank.csv
$ mv 'bank (2).csv' bank.csv
$ hledger import bank.csv
```

As mentioned above, general "deduplication" is not what `import` does.
For example, here are two cases which will not be deduplicated
(and normally should not be, since these can happen legitimately in financial data):

- Two or more of the new CSV records are identical.
- Or a new CSV record generates a journal entry identical to one already in the journal.

