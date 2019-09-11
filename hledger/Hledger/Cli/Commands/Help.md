help\
Show any of the hledger manuals.

_FLAGS_

The `help` command displays any of the main [hledger manuals](/docs.html), in one of several ways.
Run it with no argument to list the manuals, or provide a full or partial manual name to select one.

hledger manuals are available in several formats.
hledger help will use the first of these display methods that it finds: 
info, man, $PAGER, less, stdout (or when non-interactive, just stdout). 
You can force a particular viewer with the `--info`, `--man`, `--pager`, `--cat` flags.

Examples:

```shell
$ hledger help
Please choose a manual by typing "hledger help MANUAL" (a substring is ok).
Manuals: hledger hledger-ui hledger-web journal csv timeclock timedot
```

```shell
$ hledger help h --man

hledger(1)                    hledger User Manuals                    hledger(1)

NAME
       hledger - a command-line accounting tool

SYNOPSIS
       hledger [-f FILE] COMMAND [OPTIONS] [ARGS]
       hledger [-f FILE] ADDONCMD -- [OPTIONS] [ARGS]
       hledger

DESCRIPTION
       hledger  is  a  cross-platform  program  for tracking money, time, or any
...
```
