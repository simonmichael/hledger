DebConf does their accounting with Ledger.

Eg: <https://salsa.debian.org/debconf-team/public/data/dc25/-/tree/main/budget>

These repos are big; to clone just the accounting data for a few recent years, run `make` here.
Or to clone all years (starting 2017), `make all`.

Some years (eg 24 and 25) can be read by hledger with no changes needed;
others may need some conversion, eg [amount expressions](https://hledger.org/ledger.html#amount-expressions).
