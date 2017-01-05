# hledger-api

This doc is for version **1.0**. <span class="docversions"></span>

-   toc

## NAME

hledger-api - web API server for the hledger accounting tool

## SYNOPSIS

`hledger-api [OPTIONS]`\
`hledger-api --swagger`\
`hledger api -- [OPTIONS]`

## DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any
other commodity, using double-entry accounting and a simple, editable
file format. hledger is inspired by and largely compatible with
ledger(1).

hledger-api is a simple web API server, intended to support client-side
web apps operating on hledger data. It comes with a series of simple
client-side app examples, which drive its evolution.

Like hledger, it reads data from one or more files in hledger journal,
timeclock, timedot, or CSV format specified with `-f`, or
`$LEDGER_FILE`, or `$HOME/.hledger.journal` (on windows, perhaps
`C:/Users/USER/.hledger.journal`). For more about this see hledger(1),
hledger\_journal(5) etc.

The server listens on port 8001, or another specified with `-p PORT`.
Note there is no built-in access control, so you will need to hide
hledger-api behind an authenticating proxy if you want to restrict
access.

If invoked as `hledger-api --swagger`, instead of starting a server the
API docs will be printed in Swagger 2.0 format.

## OPTIONS

Note: if invoking hledger-api as a hledger subcommand, write `--` before
options as shown above.

`-d --static-dir=DIR`
:   serve files from a different directory (default: `.`)

`-p --port=PORT`
:   use a different TCP port (default: 8001)

`--swagger`
:   print API docs in Swagger 2.0 format, and exit

hledger general options:

`-h`
:   show general usage (or after COMMAND, the command's usage)

`--help`
:   show the current program's manual as plain text (or after an add-on
    COMMAND, the add-on's manual)

`--man`
:   show the current program's manual with man

`--info`
:   show the current program's manual with info

`--version`
:   show version

`--debug[=N]`
:   show debug output (levels 1-9, default: 1)

`-f FILE --file=FILE`
:   use a different input file. For stdin, use -

`--rules-file=RULESFILE`
:   Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
:   display accounts named OLD as NEW

`-I --ignore-assertions`
:   ignore any failing balance assertions in the journal

## ENVIRONMENT

**LEDGER\_FILE** The journal file path when not specified with `-f`.
Default: `~/.hledger.journal` (on windows, perhaps
`C:/Users/USER/.hledger.journal`).

## FILES

Reads data from one or more files in hledger journal, timeclock,
timedot, or CSV format specified with `-f`, or `$LEDGER_FILE`, or
`$HOME/.hledger.journal` (on windows, perhaps
`C:/Users/USER/.hledger.journal`).

## BUGS

The need to precede options with `--` when invoked from hledger is
awkward.
