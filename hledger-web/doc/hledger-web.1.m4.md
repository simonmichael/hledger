% hledger-web(1) hledger-web _version_
% _author_
% _monthyear_

_web_({{
_versions_({{hledger-web}})
_toc_
}})

_man_({{
# NAME

hledger-web - web interface for the hledger accounting tool

# SYNOPSIS

`hledger-web [OPTIONS]`\
`hledger web -- [OPTIONS]`

<style>
.highslide img {max-width:250px; float:right; margin:0 0 1em 1em;}
.highslide-caption {color:white; background-color:black;}
</style>
<a href="images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>
<a href="images/hledger-web/normal/journal.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/journal.png" title="Journal view" /></a>

<a href="images/hledger-web/normal/help.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/help.png" title="Help dialog" /></a>
<a href="images/hledger-web/normal/add.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/add.png" title="Add form" /></a>

# DESCRIPTION

_hledgerdescription_
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
For more about this see hledger(1), hledger_journal(5) etc.

By default, hledger-web starts the web app in "transient mode" and
also opens it in your default web browser if possible. In this mode
the web app will keep running for as long as you have it open in a
browser window, and will exit after two minutes of inactivity (no
requests and no browser windows viewing it).

```shell
$ hledger web
Starting web app on port 5000 with base url http://localhost:5000
Starting web browser if possible
Web app will auto-exit after a few minutes with no browsers (or press ctrl-c)
```

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

Command-line options and arguments may be used to set an initial
filter on the data. This is not shown in the web UI, but it will be
applied in addition to any search query entered there.

With journal and timeclock files (but not CSV files, currently) the
web app detects changes made by other means and will show the new data
on the next request. If a change makes the file unparseable,
hledger-web will show an error until the file has been fixed.

---
# disabled
# edit form
# Note: unlike any other hledger command, `web` can alter existing journal
# data, via the edit form.  A numbered backup of the file is saved on
# each edit, normally (ie if file permissions allow, disk is not full, etc.)
# Also, there is no built-in access control. So unless you run it behind an
# authenticating proxy, any visitor to your server will be able to see and
# overwrite the journal file (and included files.)
# hledger-web disallows edits which would leave the journal file not in
# valid [journal format](#journal). If the file becomes unparseable
# by other means, hledger-web will show an error until the file has been
# fixed.
...

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

hledger general options:

_generaloptions_

hledger reporting options:

_reportingoptions_

_man_({{

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

}})
