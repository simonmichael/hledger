## getprices

Fetch market prices for the commodities used in the journal, up to today's date, where possible,
and add them to per-commodity prices files.

```flags
Flags:
     --dry-run              just print the commands that would be run
  -o --output=FILE          write all prices to FILE (or - for stdout)
                            instead of per-commodity P<COMM>.prices files
```

This command first guesses the journal's base currency.
That is, the commodity most used as the target in P price directives;
or if there are no P directives, the commodity most used in postings;
or if there are no postings, "USD".
(You can check this with the `stats` command.)

Then for each other commodity in the journal, it fetches daily market prices in the base currency,
from that commodity's earliest transaction until today.
It uses a `getprices_` helper script in PATH (described below).

Then each commodity's prices are saved, merging them with previously saved prices if any.
By default they are saved in separate `P<COMM>.prices` files next to the main journal file.
Or with `-o FILE`, they are saved into a single file of your choice.
Or with `-o -`, they are printed on stdout.

Commands being run, and files being updated, are logged to stderr.
With `--dry-run`, you can preview the commands without running them.
The standard `getprices_` script will also log `pricehist` commands as they are run.
You can run any of these commands yourself, for troubleshooting.

### Merging new prices

Complete historical price data can be hard to get, so getprices is careful to preserve older prices.
If prices files already exist, the newly fetched prices will be merged as follows:

- Prices files will be updated only if they contain nothing but P directives (or blank lines).
- New prices for dates already seen are discarded. (So if you want to replace old prices, you must delete them before running getprices.)
- If no new prices remain to be added, the prices file is left as-is.
- Otherwise, the old prices plus the new prices are saved to the file, ordered by: date, from commodity, to commodity.

### The getprices_ helper

The `getprices` command runs a `getprices_` helper program to fetch each commodity's prices.
This allows customisation and avoids hardcoding too much into hledger.
It may become optional in future, but for now it's required.

You can find a [getprices_](https://github.com/simonmichael/hledger/blob/master/bin/getprices_) script 
in the hledger repo's bin directory.
It uses the third party [pricehist](https://github.com/chrisberkhout/pricehist) tool, which you can install with eg `uv tool install pricehist`.
You might also want to set up price provider API keys in your environment.
Providers may limit the commodity pairs, history, or number of requests unless you have a paid API key.

Or, you can make your own `getprices_` script. It should accept 3-4 arguments:

- ISO code for base currency
- ISO code for commodity to be priced
- ISO start date
- optional inclusive ISO end date (default: today)
