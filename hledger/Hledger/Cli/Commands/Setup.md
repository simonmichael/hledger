## setup

Check the status of the hledger installation.

```flags
Flags:
no command-specific flags
```

`setup` checks your hledger installation and reports the results, sometimes with helpful hints.

This is a great command to run after installing hledger.
Or after upgrading, or when something's not working, or just when you want a reminder of where your files are.

`setup` attempts to make one HTTP request, to hledger.org to detect the latest hledger release version.
It will use ANSI color by default, unless that is disabled by `NO_COLOR` or `--color=no`.
It does not use a pager or a config file.

Example output:
```
$ hledger setup
Checking your hledger setup..
Legend: good ✅, info ℹ️ , warning 🔸, problem ❗

hledger
* is a released version ?                   no ℹ️   hledger 1.50.99-gd8996c05c-20251013, mac-aarch64
* is up to date ? checking...              yes ✅  1.50.99 installed, latest is 1.50.2
* is a native binary for this machine ?    yes ✅  aarch64
* is installed in PATH (this version) ?    yes ✅  /Users/simon/.local/bin/hledger
* has a system text encoding configured ?  yes ✅  UTF-8, data files must use this encoding
* has a user config file ?                 yes ℹ️   /Users/simon/.hledger.conf (overridden)
* has a local config file ?                yes ℹ️   /Users/simon/src/hledger/hledger.conf
* the config file is readable ?            yes ✅  

terminal
* the NO_COLOR variable is defined ?        no ℹ️   
* --color is configured by config file ?    no ℹ️   
* hledger will use color by default ?      yes ✅  
* the PAGER variable is defined ?          yes ℹ️   cat
* --pager is configured by config file ?    no ℹ️   
* hledger will use a pager when needed ?   yes ✅  /bin/cat
* tables will use box-drawing chars ?       no ℹ️   you can use --pretty to enable them

journal
* the LEDGER_FILE variable is defined ?    yes ℹ️   examples/sample.journal
* a default journal file is readable ?     yes ✅  ./examples/sample.journal
* it includes additional files ?            no ℹ️   
* all commodities are declared ?            no 🔸  1 undeclared commodities
* all accounts are declared ?               no 🔸  8 undeclared accounts
* all accounts have types ?                yes ✅  
* accounts of all types exist ?             no 🔸  no EV accounts found, some features may not work
* commodities/accounts are being checked ?  no ℹ️   you can use -s to check them
* balance assertions are being checked ?   yes ✅  you can use -I to ignore them

```
