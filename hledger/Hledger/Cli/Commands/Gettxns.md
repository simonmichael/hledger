## gettxns

Fetch transaction data for importing to the journal, by running a `gettxns_` helper script.

```flags
Flags:
     --dry-run              just print the command that would be run
```

This command:

1. Creates a `data/` directory next to the main journal file, if it does not exist.
2. Locates a `gettxns_` helper script in that directory, or in PATH.
3. Runs the script with the `data/` directory as its working directory, so it can save downloaded files there.

(Then you can use `import` to import any new transactions from the data.)

The script's stdout is forwarded to hledger's stdout, and its stderr is forwarded to hledger's stderr.
The command being run is logged to stderr.
With `--dry-run`, you can preview the command without running it.

### The gettxns_ helper

The `gettxns` command runs a `gettxns_` helper program to do the actual fetching.
This allows customisation and avoids hardcoding too much into hledger.

You can make your own `gettxns_` script (a shell script, executable, or anything PATH can run).
A typical script will download multiple bank/brokerage statements as CSV or TSV files, perhaps using a bank aggregator.

A sample `gettxns_` script may be available in the [hledger repo's bin directory](https://github.com/simonmichael/hledger/tree/master/bin).
Install it in a `data/` directory next to your journal file, or anywhere in PATH.
