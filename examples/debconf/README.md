# DebConf ledgers

DebConf does their accounting with Ledger.
Eg: <https://salsa.debian.org/debconf-team/public/data/dc25/-/tree/main/budget>

Each year has its own repo, and the repos are big.
To clone just the accounting data for a few recent years: `make`
To clone all years, starting 2017: `make all`

Some years (eg 24 and 25) can be read by hledger as-is;
others may need some conversion, eg of [amount expressions](https://hledger.org/ledger.html#amount-expressions).
To check hledger readability for the years you have downloaded: `make check-hledger`
To check ledger readability: `make check-ledger`

