% hledger-api(1) hledger-api _version_
% _author_
% _monthyear_

_web_({{
m4_dnl _versions_({{hledger-api}})
<div class="versions">
version:
<a href="$1.html">dev</a>
</div>
_toc_
}})
_man_({{

# NAME

hledger-api - web API server for the hledger accounting tool

# SYNOPSIS

`hledger-api [OPTIONS]`\
`hledger-api --swagger`\
`hledger api -- [OPTIONS]`

# DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1).

}})

hledger-api is a simple web API server, intended to support
client-side web apps operating on hledger data. It comes with a series
of simple client-side app examples, which drive it's evolution.

Data is served from the usual hledger journal file:
`~/.hledger.journal`, `$LEDGER_FILE`, or another file specified with -f.
For more about the format, see hledger(1) or hledger_journal(5).

The server listens on port 8001, or another specified with `-p PORT`.
Note there is no built-in access control, so you will need to hide
hledger-api behind an authenticating proxy if you want to restrict
access.

If invoked as `hledger-api --swagger`, instead of starting a server
the API docs will be printed in Swagger 2.0 format.

# OPTIONS

Note: if invoking hledger-api as a hledger subcommand, write `--` before options as shown above.

`-f --file FILE`
: use a different input file (default: `$LEDGER_FILE` or `~/.hledger.journal`)

`-d --static-dir=DIR`
: serve files from a different directory (default: `.`)

`-p --port=PORT`
: use a different TCP port (default: 8001)

`--swagger`
: print API docs in Swagger 2.0 format, and exit

`-h`
: show usage

`--help`
: show manual

`--man`
: show manual with man

`--info`
: show manual with info

`--version`
: show version

_man_({{

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

}})
