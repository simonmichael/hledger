% hledger-web(1)
%
% October 2015

<div class="web">
* toc
</div>
<div class="man">
# NAME

hledger-web - web interface for the hledger accounting tool

# SYNOPSIS

`hledger-web [OPTIONS]`\
`hledger web -- [OPTIONS]`

# DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1).
</div>

hledger-web is hledger's web interface.  It starts a simple web
application for browsing and adding transactions, and optionally
opens it in a web browser window if possible.
It provides a more user-friendly UI than the hledger CLI or
hledger-ui interface, showing more at once (accounts, the current
account register, balance charts) and allowing history-aware data
entry, interactive searching, and bookmarking.

hledger-web also lets you share a ledger with multiple users, or even the public web.
There is no access control, so if you need that you should put it
behind a suitable web proxy.  As a small protection against data loss
when running an unprotected instance, it writes a numbered backup of
the main journal file (only ?) on every edit.

The journal file is `~/.hledger.journal`, `$LEDGER_FILE`, or another file specified with -f.
For more about the format, see hledger(1) or hledger_journal(5).

By default, hledger-web starts the web app in "transient mode" and
also opens it in your default web browser if possible. In this mode
the web app will keep running for as long as you have it open in a
browser window, and will exit after two minutes of inactivity (no
requests and no browser windows viewing it).

With `--server`, it starts the web app in non-transient mode and logs
requests to the console.  Typically when running hledger web as part
of a website you'll want to use `--base-url` to set the
protocol/hostname/port/path to be used in hyperlinks.  The
`--file-url` option allows static files to be served from a different
url, eg for better caching or cookie-less serving.

You can use `--port` to listen on a different TCP port, eg if you are
running multiple hledger-web instances.  This need not be the same as
the PORT in the base url.

Note there is no built-in access control, so you will need to hide
hledger-web behind an authenticating proxy (such as apache or nginx)
if you want to restrict who can see and add entries to your journal.

With journal and timelog files (but not CSV files, currently)
the web app detects changes and will show the new data on the next request.
If a change makes the file unparseable, hledger-web will show an error
until the file has been fixed.

# OPTIONS

Note: if invoking hledger-web as a hledger subcommand, write `--` before options as shown above.

`--server`
: disable browser-opening and auto-exit-on-idle, and log all requests to stdout

`--port=PORT`
: set the TCP port to listen on (default: 5000)

`--base-url=URL`
: set the base url (default: http://localhost:PORT).
You would change this when sharing over the network, or integrating within a larger website.

`--file-url=URL`
: set the static files url (default: BASEURL/static).
hledger-web normally serves static files itself, but if you wanted to
serve them from another server for efficiency, you would set the url with this.

`-h --help`
: show help

`--version`
: show version information

## hledger options:

The following common hledger options should also work:

`-f FILE --file=FILE`
: use a different input file. For stdin, use -

`--rules-file=RULESFILE`
: Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
: display accounts named OLD as NEW

`--ignore-assertions`
: ignore any failing balance assertions in the journal

`--debug=N`
: show debug output if N is 1-9 (default: 0)

`-b --begin=DATE`
: include postings/txns on or after this date

`-e --end=DATE`
: include postings/txns before this date

`-p --period=PERIODEXP`
: set start date, end date, and/or reporting interval all at once (overrides the flags above)

`--date2 --aux-date`
: use postings/txns' secondary dates instead

`-C --cleared`
: include only cleared postings/txns

`--pending`
: include only pending postings/txns

`-U --uncleared`
: include only uncleared (and pending) postings/txns

`-R --real`
: include only non-virtual postings

`--depth=N`
: hide accounts/postings deeper than N

`-E --empty`
: show empty/zero things which are normally omitted

`-B --cost`
: show amounts in their cost price's commodity

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

`-f-` doesn't work (hledger-web can't read from stdin).

Query arguments and some applicable hledger options probably aren't supported.

Does not work in text-mode browsers.

Does not work well on small screens.

The auto-exit feature was added to avoid leaving stray processes, eg on Windows.
It is not well tested.

If you start two instances on the same port, the second one will
appear to run normally, but you will be seeing pages served from the
first one.

</div>
