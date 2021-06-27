% hledger-web(1)
% _author_
% _monthyear_

_info_({{INFO-DIR-SECTION User Applications}})
_info_({{START-INFO-DIR-ENTRY}})
_info_({{* HLedger-Web: (hledger-web).           Command-Line Accounting (Web UI)}})
_info_({{END-INFO-DIR-ENTRY}})

_man_({{
# NAME
}})

hledger-web is a web interface (WUI) for the hledger accounting tool.
This manual is for hledger-web _version_.

_man_({{
# SYNOPSIS
}})

`hledger-web [OPTIONS]`\
`hledger web -- [OPTIONS]`

_man_({{
# DESCRIPTION
}})

_hledgerdescription_

_web_({{
<style>
.highslide img {max-width:200px; border:thin grey solid; margin:0 0 1em 1em; }
.highslide-caption {color:white; background-color:black;}
</style>
<div style="float:right; max-width:200px; text-align:right;">
<a href="/images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>
<a href="/images/hledger-web/normal/journal.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/journal.png" title="Journal view" /></a>
<a href="/images/hledger-web/normal/help.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/help.png" title="Help dialog" /></a>
<a href="/images/hledger-web/normal/add.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-web/normal/add.png" title="Add form" /></a>
</div>
}})

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

Like hledger, it reads _files_
For more about this see hledger(1).

# OPTIONS

Command-line options and arguments may be used to set an initial
filter on the data. These filter options are not shown in the web UI,
but it will be applied in addition to any search query entered there.

Note: if invoking hledger-web as a hledger subcommand, write `--` before options,
as shown in the synopsis above.

`--serve`
: serve and log requests, don't browse or auto-exit

`--serve-api`
: like --serve, but serve only the JSON web API, without the server-side web UI

`--host=IPADDR`
: listen on this IP address (default: 127.0.0.1)

`--port=PORT`
: listen on this TCP port (default: 5000)

`--socket=SOCKETFILE`
: use a unix domain socket file to listen for requests instead of a TCP socket. Implies
`--serve`. It can only be used if the operating system can provide this type of socket.

`--base-url=URL`
: set the base url (default: http://IPADDR:PORT).
You would change this when sharing over the network, or integrating within a larger website.

`--file-url=URL`
: set the static files url (default: BASEURL/static).
hledger-web normally serves static files itself, but if you wanted to
serve them from another server for efficiency, you would set the url with this.

`--capabilities=CAP[,CAP..]`
: enable the view, add, and/or manage capabilities (default: view,add)

`--capabilities-header=HTTPHEADER`
: read capabilities to enable from a HTTP header, like X-Sandstorm-Permissions (default: disabled)

`--test`
: run hledger-web's tests and exit. hspec test runner args may follow a --, eg: hledger-web --test -- --help

hledger input options:

_inputoptions_

hledger reporting options:

_reportingoptions_

hledger help options:

_helpoptions_

A @FILE argument will be expanded to the contents of FILE,
which should contain one command line option/argument per line.
(To prevent this, insert a `--` argument before.)

By default, hledger-web starts the web app in "transient mode" and
also opens it in your default web browser if possible. In this mode
the web app will keep running for as long as you have it open in a
browser window, and will exit after two minutes of inactivity (no
requests and no browser windows viewing it).
With `--serve`, it just runs the web app without exiting, and logs
requests to the console.
With `--serve-api`, only the JSON web api (see below) is served,
with the usual HTML server-side web UI disabled.

By default the server listens on IP address 127.0.0.1, accessible only to local requests.
You can use `--host` to change this, eg `--host 0.0.0.0` to listen on all configured addresses.

Similarly, use `--port` to set a TCP port other than 5000, eg if you are
running multiple hledger-web instances.

Both of these options are ignored when `--socket` is used. In this case, it
creates an `AF_UNIX` socket file at the supplied path and uses that for communication.
This is an alternative way of running multiple hledger-web instances behind
a reverse proxy that handles authentication for different users.
The path can be derived in a predictable way, eg by using the username within the path.
As an example, `nginx` as reverse proxy can use the variable `$remote_user` to
derive a path from the username used in a [HTTP basic authentication](https://docs.nginx.com/nginx/admin-guide/security-controls/configuring-http-basic-authentication/).
The following `proxy_pass` directive allows access to all `hledger-web`
instances that created a socket in `/tmp/hledger/`:

```
  proxy_pass http://unix:/tmp/hledger/${remote_user}.socket;
```

You can use `--base-url` to change the protocol, hostname, port and path that appear in hyperlinks,
useful eg for integrating hledger-web within a larger website.
The default is `http://HOST:PORT/` using the server's configured host address and TCP port
(or `http://HOST` if PORT is 80).

With `--file-url` you can set a different base url for static files,
eg for better caching or cookie-less serving on high performance websites.

# PERMISSIONS

By default, hledger-web allows anyone who can reach it to view the journal
and to add new transactions, but not to change existing data.

You can restrict who can reach it by

- setting the IP address it listens on (see `--host` above).
  By default it listens on 127.0.0.1, accessible to all users on the local machine.
- putting it behind an authenticating proxy, using eg apache or nginx
- custom firewall rules

You can restrict what the users who reach it can do, by

- using the `--capabilities=CAP[,CAP..]` flag when you start it,
  enabling one or more of the following capabilities. The default value is `view,add`:
  - `view`   - allows viewing the journal file and all included files
  - `add`    - allows adding new transactions to the main journal file
  - `manage` - allows editing, uploading or downloading the main or included files

- using the `--capabilities-header=HTTPHEADER` flag to specify a HTTP header
  from which it will read capabilities to enable. hledger-web on Sandstorm
  uses the X-Sandstorm-Permissions header to integrate with Sandstorm's permissions.
  This is disabled by default.

# EDITING, UPLOADING, DOWNLOADING

If you enable the `manage` capability mentioned above,
you'll see a new "spanner" button to the right of the search form.
Clicking this will let you edit, upload, or download the journal
file or any files it includes.

Note, unlike any other hledger command, in this mode you (or any visitor)
can alter or wipe the data files.

Normally whenever a file is changed in this way, hledger-web saves a numbered backup
(assuming file permissions allow it, the disk is not full, etc.)
hledger-web is not aware of version control systems, currently; if you use one,
you'll have to arrange to commit the changes yourself (eg with a cron job
or a file watcher like entr).

Changes which would leave the journal file(s) unparseable or non-valid
(eg with failing balance assertions) are prevented.
(Probably. This needs re-testing.)

# RELOADING

hledger-web detects changes made to the files by other means (eg if you edit
it directly, outside of hledger-web), and it will show the new data
when you reload the page or navigate to a new page.
If a change makes a file unparseable,
hledger-web will display an error message until the file has been fixed.

(Note: if you are viewing files mounted from another machine, make
sure that both machine clocks are roughly in step.)

# JSON API

In addition to the web UI, hledger-web also serves a JSON API that can be 
used to get data or add new transactions.
If you want the JSON API only, you can use the `--serve-api` flag. Eg:

```shell
$ hledger-web -f examples/sample.journal --serve-api
...
```

You can get JSON data from these routes:

```
/version
/accountnames
/transactions
/prices
/commodities
/accounts
/accounttransactions/ACCOUNTNAME
```

Eg, all account names in the journal (similar to the [accounts](hledger.html#accounts) command).
(hledger-web's JSON does not include newlines, here we use python to prettify it):

```shell
$ curl -s http://127.0.0.1:5000/accountnames | python -m json.tool
[
    "assets",
    "assets:bank",
    "assets:bank:checking",
    "assets:bank:saving",
    "assets:cash",
    "expenses",
    "expenses:food",
    "expenses:supplies",
    "income",
    "income:gifts",
    "income:salary",
    "liabilities",
    "liabilities:debts"
]
```

Or all transactions:

```shell
$ curl -s http://127.0.0.1:5000/transactions | python -m json.tool
[
    {
        "tcode": "",
        "tcomment": "",
        "tdate": "2008-01-01",
        "tdate2": null,
        "tdescription": "income",
        "tindex": 1,
        "tpostings": [
            {
                "paccount": "assets:bank:checking",
                "pamount": [
                    {
                        "acommodity": "$",
                        "aismultiplier": false,
                        "aprice": null,
...
```

Most of the JSON corresponds to hledger's data types; for details of what the fields mean, see the
[Hledger.Data.Json haddock docs](http://hackage.haskell.org/package/hledger-lib-1.17.1/docs/Hledger-Data-Json.html)
and click on the various data types, eg 
[Transaction](http://hackage.haskell.org/package/hledger-lib-1.17.1/docs/Hledger-Data-Types.html#t:Transaction).
And for a higher level understanding, see the [journal manual](hledger.html).

In some cases there is outer JSON corresponding to a "Report" type.
To understand that, go to the
[Hledger.Web.Handler.MiscR haddock](http://hackage.haskell.org/package/hledger-web-1.17.1/docs/Hledger-Web-Handler-MiscR.html)
and look at the source for the appropriate handler to see what it returns.
Eg for `/accounttransactions` it's
[getAccounttransactionsR](http://hackage.haskell.org/package/hledger-web-1.17.1/docs/src/Hledger.Web.Handler.MiscR.html#getAccounttransactionsR),
returning a "`accountTransactionsReport ...`".
[Looking up](https://hoogle.haskell.org/?hoogle=accountTransactionsReport) the haddock for that
we can see that /accounttransactions returns an 
[AccountTransactionsReport](https://hackage.haskell.org/package/hledger-lib-1.17.1/docs/Hledger-Reports-AccountTransactionsReport.html#t:AccountTransactionsReport),
which consists of a report title and a list of AccountTransactionsReportItem (etc).

You can add a new transaction to the journal with a PUT request to `/add`,
if hledger-web was started with the `add` capability (enabled by default).
The payload must be the full, exact JSON representation of a hledger transaction
(partial data won't do).
You can get sample JSON from hledger-web's `/transactions` or `/accounttransactions`,
or you can export it with hledger-lib, eg like so:

```shell
.../hledger$ stack ghci hledger-lib
>>> writeJsonFile "txn.json" (head $ jtxns samplejournal)
>>> :q
```

Here's how it looks as of hledger-1.17
(remember, this JSON corresponds to hledger's 
[Transaction](http://hackage.haskell.org/package/hledger-lib-1.17.1/docs/Hledger-Data-Types.html#t:Transaction)
and related data types):

```json
{
    "tcomment": "",
    "tpostings": [
        {
            "pbalanceassertion": null,
            "pstatus": "Unmarked",
            "pamount": [
                {
                    "aprice": null,
                    "acommodity": "$",
                    "aquantity": {
                        "floatingPoint": 1,
                        "decimalPlaces": 10,
                        "decimalMantissa": 10000000000
                    },
                    "aismultiplier": false,
                    "astyle": {
                        "ascommodityside": "L",
                        "asdigitgroups": null,
                        "ascommodityspaced": false,
                        "asprecision": 2,
                        "asdecimalpoint": "."
                    }
                }
            ],
            "ptransaction_": "1",
            "paccount": "assets:bank:checking",
            "pdate": null,
            "ptype": "RegularPosting",
            "pcomment": "",
            "pdate2": null,
            "ptags": [],
            "poriginal": null
        },
        {
            "pbalanceassertion": null,
            "pstatus": "Unmarked",
            "pamount": [
                {
                    "aprice": null,
                    "acommodity": "$",
                    "aquantity": {
                        "floatingPoint": -1,
                        "decimalPlaces": 10,
                        "decimalMantissa": -10000000000
                    },
                    "aismultiplier": false,
                    "astyle": {
                        "ascommodityside": "L",
                        "asdigitgroups": null,
                        "ascommodityspaced": false,
                        "asprecision": 2,
                        "asdecimalpoint": "."
                    }
                }
            ],
            "ptransaction_": "1",
            "paccount": "income:salary",
            "pdate": null,
            "ptype": "RegularPosting",
            "pcomment": "",
            "pdate2": null,
            "ptags": [],
            "poriginal": null
        }
    ],
    "ttags": [],
    "tsourcepos": {
        "tag": "JournalSourcePos",
        "contents": [
            "",
            [
                1,
                1
            ]
        ]
    },
    "tdate": "2008-01-01",
    "tcode": "",
    "tindex": 1,
    "tprecedingcomment": "",
    "tdate2": null,
    "tdescription": "income",
    "tstatus": "Unmarked"
}
```

And here's how to test adding it with curl. This should add a new entry to your journal:

```shell
$ curl http://127.0.0.1:5000/add -X PUT -H 'Content-Type: application/json' --data-binary @txn.json
```

# ENVIRONMENT

_LEDGER_FILE_

# FILES

Reads _files_

# BUGS

The need to precede options with `--` when invoked from hledger is awkward.

`-f-` doesn't work (hledger-web can't read from stdin).

Query arguments and some hledger options are ignored.

Does not work in text-mode browsers.

Does not work well on small screens.
