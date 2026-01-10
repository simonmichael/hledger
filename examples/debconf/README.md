# DebConf ledgers

DebConf has done their accounting with Ledger since 2017.\
Eg: <https://salsa.debian.org/debconf-team/public/data/dc25/-/tree/main/budget>\
Here are some scripts and notes to help view them with hledger.

Each year has its own repo, and the repos are big.\
To clone just the accounting data for all years: `make`\
Or to clone just a few recent years: `make recent`

Some patches for hledger readability will be applied; those might need updating from time to time.\
To check hledger readability: `make check-hledger`\
To check ledger  readability: `make check-ledger`

For convenient reporting, a top-level journal file is created for each year's repo.\
These also include the forex.db files, allowing currency conversion (when rates exist; you might need to fetch more).

The `hledger.conf` file sets a few non-essential defaults when you are in this directory.

Here are hledger's 
[manual](https://hledger.org/hledger.html)
and [other docs](https://hledger.org/doc.html).

## Report examples

Single year reports:

- `hledger -f 2025.ledger is`
- `hledger -f 2025.ledger is -V`
- `hledger -f 2025.ledger is -VQSTt --drop 1`

The `all.ledger` file reads from all the downloaded repos:

- `hledger -f all.ledger is -1 -YTN`
- `hledger -f all.ledger is -1 -YT -X USD`

`make hledger-'ARGS'` runs a hledger command in each downloaded repo:

- `make hledger-accounts`
- `make hledger-stats | grep 'Txns  '`
- `make hledger-'bs -2'`
- `make hledger-'is -2`
