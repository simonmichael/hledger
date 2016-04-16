% hledger-api(1)
%
% January 2016

<div class="web">
_versions_({{hledger-api}})

* toc
</div>
<div class="man">

# NAME

hledger-api - web API server for the hledger accounting tool

# SYNOPSIS

`hledger-api [OPTIONS]`\
`hledger api -- [OPTIONS]`

# DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1).

</div>

hledger-api is a simple web API server, intended to support
client-side web apps operating on hledger data. It comes with a series
of simple client-side app examples, which drive it's evolution.

Data is served from the usual hledger journal file:
`~/.hledger.journal`, `$LEDGER_FILE`, or another file specified with -f.
For more about the format, see hledger(1) or hledger_journal(5).

The server listens for requests on port ...

Note there is no built-in access control, so you will need to hide
hledger-api behind an authenticating proxy if you want to restrict
access.

<!-- With journal and timeclock files (but not CSV files, currently) -->
<!-- the web app detects changes and will show the new data on the next request. -->
<!-- If a change makes the file unparseable, hledger-api will show an error -->
<!-- until the file has been fixed. -->

<!-- # OPTIONS -->

<!-- Note: if invoking hledger-web as a hledger subcommand, write `--` before options as shown above. -->

<!-- `--port=PORT` -->
<!-- : set the TCP port to listen on (default: 5000) -->

<!-- `-h --help` -->
<!-- : show help -->

<!-- `--version` -->
<!-- : show version information -->

<!-- ## hledger options: -->

<!-- The following common hledger options should also work: -->

<!-- `-f FILE --file=FILE` -->
<!-- : use a different input file. For stdin, use - -->

<!-- `--rules-file=RULESFILE` -->
<!-- : Conversion rules file to use when reading CSV (default: FILE.rules) -->

<!-- `--alias=OLD=NEW` -->
<!-- : display accounts named OLD as NEW -->

<!-- `--ignore-assertions` -->
<!-- : ignore any failing balance assertions in the journal -->

<!-- `--debug=N` -->
<!-- : show debug output if N is 1-9 (default: 0) -->

<!-- `-b --begin=DATE` -->
<!-- : include postings/txns on or after this date -->

<!-- `-e --end=DATE` -->
<!-- : include postings/txns before this date -->

<!-- `-p --period=PERIODEXP` -->
<!-- : set start date, end date, and/or reporting interval all at once (overrides the flags above) -->

<!-- `--date2 --aux-date` -->
<!-- : use postings/txns' secondary dates instead -->

<!-- `-C --cleared` -->
<!-- : include only cleared postings/txns -->

<!-- `--pending` -->
<!-- : include only pending postings/txns -->

<!-- `-U --uncleared` -->
<!-- : include only uncleared (and pending) postings/txns -->

<!-- `-R --real` -->
<!-- : include only non-virtual postings -->

<!-- `--depth=N` -->
<!-- : hide accounts/postings deeper than N -->

<!-- `-E --empty` -->
<!-- : show empty/zero things which are normally omitted -->

<!-- `-B --cost` -->
<!-- : show amounts in their cost price's commodity -->

<div class="man">

# ENVIRONMENT

**LEDGER_FILE**
sets the default journal file path. If not set, it is `~/.hledger.journal`.

# FILES

Reads data from a hledger journal file (`$LEDGER_FILE` or
`~/.hledger.journal` by default), or a CSV file plus associated CSV
rules file.

# BUGS

The need to precede options with `--` when invoked from hledger is awkward.

<!-- `-f-` doesn't work (hledger-web can't read from stdin). -->

<!-- Query arguments and some applicable hledger options probably aren't supported. -->

<!-- Does not work in text-mode browsers. -->

<!-- Does not work well on small screens. -->

<!-- The auto-exit feature was added to avoid leaving stray processes, eg on Windows. -->
<!-- It is not well tested. -->

<!-- If you start two instances on the same port, the second one will -->
<!-- appear to run normally, but you will be seeing pages served from the -->
<!-- first one. -->

</div>
