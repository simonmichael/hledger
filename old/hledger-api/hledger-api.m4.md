% hledger-api(1) hledger-api _version_
% _author_
% _monthyear_

_web_({{
_docversionlinks_({{hledger-api}})
}})

_man_({{
# NAME

hledger-api - web API server for the hledger accounting tool

# SYNOPSIS

`hledger-api [OPTIONS]`\
`hledger api -- [OPTIONS]`

# DESCRIPTION

_hledgerdescription_
}})

hledger-api is a simple web API server, intended to support
client-side web apps operating on hledger data. It comes with a series
of simple client-side app examples, which drive its evolution.

Like hledger, it reads _files_
For more about this see hledger(1), hledger_journal(5) etc.

The server listens on IP address 127.0.0.1, accessible only to local requests, by default.
You can change this with `--host`, eg `--host 0.0.0.0` to listen on all addresses.
Note there is no other access control, and hledger-api allows file browsing,
so on shared machines you will certainly need to put it behind an authenticating proxy to restrict access.

You can change the TCP port it listens on (default: 8001) with `-p PORT`.

API methods look like:
```
/api/v1/accountnames
/api/v1/transactions
/api/v1/prices
/api/v1/commodities
/api/v1/accounts
/api/v1/accounts/ACCTNAME
```
See `/api/swagger.json` for a full list in Swagger 2.0 format.
(Or you can run `hledger-api --swagger` to print this in the console.)

hledger-api also serves files, from the current directory by default,
and the `/` path will also show a directory listing.
This is convenient for serving client-side web code, in addition to the server-side api.

# OPTIONS

Note: if invoking hledger-api as a hledger subcommand, write `--` before options as shown above.

`-f --file=FILE`
: use a different input file. For stdin, use - (default: `$LEDGER_FILE` or `$HOME/.hledger.journal`)

`-d --static-dir=DIR`
: serve files from a different directory (default: `.`)

`--host=IPADDR`
: listen on this IP address (default: 127.0.0.1)

`-p --port=PORT`
: listen on this TCP port (default: 8001)

`--swagger`
: print API docs in Swagger 2.0 format, and exit

`--version`
: show version

`-h --help`
: show usage


_man_({{

# ENVIRONMENT

_LEDGER_FILE_

# FILES

Reads _files_

# BUGS

The need to precede options with `--` when invoked from hledger is awkward.

}})
