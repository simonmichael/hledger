# DebConf ledgers

DebConf does their accounting with Ledger.
Eg: <https://salsa.debian.org/debconf-team/public/data/dc25/-/tree/main/budget>

Each year has its own repo, and the repos are big.
To clone just the accounting data for a few recent years: `make`\
Or to clone all years, starting 2017: `make all`

Some patches for hledger readability will be applied; those might need updating from time to time.\
To check hledger readability: `make check-hledger`\
To check ledger  readability: `make check-ledger`
