# COMMANDS

hledger provides a number of subcommands; `hledger` with no arguments
shows a list.

If you install additional `hledger-*` packages, or if you put programs
or scripts named `hledger-NAME` in your PATH, these will also be
listed as subcommands.

Run a subcommand by writing its name as first argument (eg `hledger
incomestatement`). You can also write one of the standard short aliases
displayed in parentheses in the command list (`hledger b`), or any
any unambiguous prefix of a command name (`hledger inc`). 

Here are all the builtin commands in alphabetical order.
See also `hledger` for a more organised command list,
and `hledger CMD -h` for detailed command help.  

## accounts

_include_(Hledger/Cli/Commands/Accounts.md)

## activity

_include_(Hledger/Cli/Commands/Activity.md)

## add

_include_(Hledger/Cli/Commands/Add.md)

## balance

_include_({{Hledger/Cli/Commands/Balance.md}})

## balancesheet

_include_({{Hledger/Cli/Commands/Balancesheet.md}})

## balancesheetequity

_include_({{Hledger/Cli/Commands/Balancesheetequity.md}})

## cashflow

_include_({{Hledger/Cli/Commands/Cashflow.md}})

## check-dates

_include_({{Hledger/Cli/Commands/Checkdates.md}})

## check-dupes

_include_({{Hledger/Cli/Commands/Checkdupes.md}})

## close

_include_({{Hledger/Cli/Commands/Close.md}})

## commodities

_include_({{Hledger/Cli/Commands/Commodities.md}})

## descriptions

_include_({{Hledger/Cli/Commands/Descriptions.md}})

## diff

_include_({{Hledger/Cli/Commands/Diff.md}})

## files

_include_({{Hledger/Cli/Commands/Files.md}})

## help

_include_({{Hledger/Cli/Commands/Help.md}})

## import

_include_({{Hledger/Cli/Commands/Import.md}})

## incomestatement

_include_({{Hledger/Cli/Commands/Incomestatement.md}})

## notes

_include_({{Hledger/Cli/Commands/Notes.md}})

## payees

_include_({{Hledger/Cli/Commands/Payees.md}})

## prices

_include_({{Hledger/Cli/Commands/Prices.md}})

## print

_include_({{Hledger/Cli/Commands/Print.md}})

## print-unique

_include_({{Hledger/Cli/Commands/Printunique.md}})

## register

_include_({{Hledger/Cli/Commands/Register.md}})

## register-match

_include_({{Hledger/Cli/Commands/Registermatch.md}})

## rewrite

_include_({{Hledger/Cli/Commands/Rewrite.md}})

## roi

_include_({{Hledger/Cli/Commands/Roi.md}})

## stats

_include_({{Hledger/Cli/Commands/Stats.md}})

## tags

_include_({{Hledger/Cli/Commands/Tags.md}})

## test

_include_({{Hledger/Cli/Commands/Test.md}})


## Add-on Commands

hledger also searches for external add-on commands, and will include these in the commands list.
These are programs or scripts in your PATH whose name starts with `hledger-`
and ends with a recognised file extension 
(currently: no extension, `bat`,`com`,`exe`, `hs`,`lhs`,`pl`,`py`,`rb`,`rkt`,`sh`).

Add-ons can be invoked like any hledger command, but there are a few things to be aware of.
Eg if the `hledger-web` add-on is installed,

- `hledger -h web` shows hledger's help, while `hledger web -h` shows hledger-web's help.
  
- Flags specific to the add-on must have a preceding `--` to hide them from hledger.
  So `hledger web --serve --port 9000` will be rejected; you must use `hledger web -- --serve --port 9000`.

- You can always run add-ons directly if preferred: `hledger-web --serve --port 9000`.

Add-ons are a relatively easy way to add local features or experiment with new ideas.
They can be written in any language, but haskell scripts have a big advantage:
they can use the same hledger (and haskell) library functions that built-in commands do,
for command-line options, journal parsing, reporting, etc.

Two important add-ons are the hledger-ui and hledger-web user interfaces.
These are maintained and released along with hledger:

### ui
[hledger-ui](hledger-ui.html) provides an efficient terminal interface. 

### web
[hledger-web](hledger-web.html) provides a simple web interface.

Third party add-ons, maintained separately from hledger, include:

### iadd

[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd)
is a more interactive, terminal UI replacement for the [add command](hledger.html#add). 

### interest

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
generates interest transactions for an account according to various schemes. 

<!-- ### autosync -->

<!-- [hledger-autosync](https://github.com/simonmichael/hledger/blob/master/bin/hledger-autosync)  -->
<!-- is a symbolic link for easily running  -->
<!-- [ledger-autosync](https://pypi.python.org/pypi/ledger-autosync) if you make a symbolic -->
<!-- ledger-autosync does deduplicating conversion of OFX data and some CSV formats, -->
<!-- and can also download the data  -->
<!-- [if your bank offers OFX Direct Connect](http://wiki.gnucash.org/wiki/OFX_Direct_Connect_Bank_Settings).  -->

A few more experimental or old add-ons can be found in hledger's bin/
directory. These are typically prototypes and not guaranteed to work.

