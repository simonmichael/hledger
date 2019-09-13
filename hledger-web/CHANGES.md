User-visible changes in hledger-web.
See also the hledger changelog.

# aa20f34b


# 1.15 2019-09-01

- --serve-api disables the usual server-side web UI (leaving only the API routes)

- register page: account names are hyperlinked

- ?sidebar= now hides the sidebar, same as ?sidebar=0

- fix "_create_locale could not be located" error on windows 7 (#1039)

- use hledger 1.15

# 1.14.1 2019-03-20

- /accounts JSON: return all accounts, not just top-level ones (#985)
  Accounts are returned as a flat list, in tree order, with asubs fields empty.

- use hledger 1.14.2

# 1.14 2019-03-01

- serve the same JSON-providing routes as in hledger-api:
  ```
  /accountnames
  /transactions
  /prices
  /commodities
  /accounts
  /accounttransactions/ACCT
  ```
  And allow adding a new transaction by PUT'ing JSON (similar to the
  output of /transactions) to /add. This requires the `add` capability
  (which is enabled by default). Here's how to test with curl:
  ```
  $ curl -s http://127.0.0.1:5000/add -X PUT -H 'Content-Type: application/json' --data-binary @in.json; echo
  ```
  (#316)

- fix unbalanced transaction prevention in the add form

- fix transaction-showing tooltips (#927)

- manual updates: document --capabilities/--capabilities-header and
  editing/uploading/downloading.

- use hledger 1.14

# 1.13 (2019/02/01)

- use hledger 1.13

# 1.12 (2018/12/02)

-   fix duplicate package.yaml keys warned about by hpack

-   use hledger 1.12

# 1.11.1 (2018/10/06)

-   use hledger 1.11.1

# 1.11 (2018/9/30)

-   use hledger 1.11

# 1.10 (2018/6/30)

-   multiple -f options, and --auto, work again

-   view, add, edit permissions can be set at CLI or by Sandstorm HTTP header

-   the edit form has been revived, for whole-journal editing

-   the journal can now be uploaded and downloaded

-   the e key toggles empty accounts in the sidebar

-   use hledger-lib 1.10

# 1.9.2 (2018/4/30)

-   use hledger-lib 1.9.1

# 1.9.1 (2018/4/13)

-   fix compilation with yesod < 1.6

# 1.9 (2018/3/31)

-   support ghc 8.4, latest deps

-   when the system text encoding is UTF-8, ignore any UTF-8 BOM prefix
    found when reading files

-   -E/--empty toggles zeroes at startup (with opposite default to cli)

# 1.5 (2017/12/31)

-   add form account fields now suggest implied and declared account names also

-   add form date field now uses a datepicker (Eli Flanagan)

-   don't write a session file at startup, don't require a writable working directory

-   support -V/--value, --forecast, --auto

-   remove upper bounds on all but hledger* and base (experimental)

# 1.4 (2017/9/30)

-   a @FILE argument reads flags & args from FILE, one per line

-   enable --pivot and --anon options, like hledger CLI (#474) (Jakub Zárybnický)

-   web: Make "Add transaction" button tabbable (#430) (Jakub Zárybnický)

-   accept -NUM as a shortcut for --depth NUM

-   deps: drop oldtime flag, require time 1.5+, remove ghc < 7.6 support

# 1.3.2 (2017/8/25)

-   remove unnecessary bound to satisfy hackage server

# 1.3.1 (2017/8/25)

-   allow megaparsec 6 (#594, Simon Michael, Hans-Peter Deifel)

-   allow megaparsec-6.1 (Hans-Peter Deifel)

-   restore upper bounds on hledger packages

# 1.3 (2017/6/30)

Depends on hledger\[-lib\] 1.3, see related changelogs.

# 1.2 (2017/3/31)

Accounts with ? in name had empty registers (fixes #498) (Bryan Richter)

Allow megaparsec 5.2 (fixes #503)

# 1.1 (2016/12/31)

-   add --host option (#429)

    This came up in the context of Docker, but it seems it wasn't
    possible for hledger-web to serve remote clients directly (without
    a proxy) because of 127.0.0.1 being hardcoded. That can now be
    changed with --host=IPADDR. Also, the default base url uses this
    address rather than a hard-coded "localhost".

-   rename --server to --serve

    The --server flag sounded too close in meaning to --host so
    I've renamed it to --serve. The old spelling is still accepted,
    but deprecated and will be removed in the next release.

# 1.0.1 (2016/10/27)

-   allow megaparsec 5.0 or 5.1

# 1.0 (2016/10/26)

## ui

-   use full width on large screens, hide sidebar on small screens, more standard bootstrap styling (#418, #422) (Dominik Süß)

-   show the sidebar by default (#310)

-   fix the add link's tooltip

-   when the add form opens, focus the first field (#338)

-   leave the add form's date field blank, avoiding a problem with tab clearing it (#322)

-   use transaction id instead of date in transaction urls (#308) (Thomas R. Koll)

-   after following a link to a transaction, highlight it (Thomas R. Koll)

-   misc. HTML/CSS/file cleanups/fixes (Thomas R. Koll)

## misc

-   startup is more robust (#226).

    Now we exit if something is already using the specified port,
    and we don't open a browser page before the app is ready.

-   termination is more robust, avoiding stray background threads.

    We terminate the server thread more carefully on exit, eg on control-C in GHCI.

-   more robust register dates and filtering in some situations (see hledger-ui notes)

-   reloading the journal preserves options, arguments in effect (#314).

    The initial query specified by command line arguments is now preserved
    when the journal is reloaded. This does not appear in the web UI, it's
    like an invisible extra filter.

-   show a proper not found page on 404

-   document the special \`inacct:\` query (#390)

0.27 (2015/10/30)

-   Fix keyboard shortcut for adding a transaction (Carlos Lopez-Camey)

-   Clear the form when clicking 'Add a transaction' (just like the shortcut) (Carlos Lopez-Camey)

-   Disallow -f- (reading from standard input) which currently doesn't work (#202)

-   Fix broken links when using --base-url (#235)

-   Fix the --file-url option (#285)

-   Show fewer "other accounts" in the account register: to reduce
    clutter in the "other accounts" field, if there are both real and
    virtual postings to other accounts, show only the accounts posted to
    by real postings.

0.26 (2015/7/12)

-   make the j keybinding respect --base-url (fixes #271)
-   respect command line options (fixes #225)
-   include the unminified jquery source again (#161)
-   fix build breakage from #165 (fixes #268)
-   fix a js error breaking add form in browsers other than firefox (#251, Carlos Lopez-Camey <c.lopez@kmels.net>)
-   drop deprecated network-conduit dependency

0.25 (2015/4/7)

-   GHC 7.10 compatibility (#239)

-   fix the add form when there are included files (#234)

    NB to make this work, the add form now shows the full file path of
    the main and included journal files.

-   improve add form validation (#223, #234)

    All add form errors are displayed as form errors, not internal
    server errors, and when there are errors the add form is redisplayed
    (form inputs are not preserved, currently).

-   keep the add button right-aligned when pressing ctrl - on the add form

0.24.1 (2015/1/10)

-   add missing modules to fix cabal tests (#232)

0.24 (2014/12/25)

General:
- fix: add missing hs/js files to package
- the web UI has been streamlined, dropping the raw and entries views and
the edit form
- the help dialog has been improved
- keyboard shortcuts are now available
- the sidebar can be toggled open or closed (press s)

Journal view:
- layout tweaks for less truncation of descriptions and account names

Register view:
- fix: don't show all zero amounts when searching by account within an
account register view
- chart improvements: show zero balances with correct commodity; show
accurate balance at all dates; show transaction events & tooltips;
show zero/today lines & background colors

Add form:
- parses data more strictly and gives better errors (eg #194)
- allows any number of postings, not just two
- after adding a transaction, goes back to the journal
- keyboard shortcut (a) allows quick access

Dependencies:
- allow warp 3*, wai-handler-launch 3*
- require yesod 1.4* (fixes #212)
- js updated (jquery, bootstrap, flot), added (typeahead, cookie, hotkeys),
removed (select2)

0.23.3 (2014/9/12)

-   remove warp, wai-handler-launch upper bounds (fixes #205)

0.23.2 (2014/5/8)

-   depend on latest hledger

0.23.1 (2014/5/7)

-   depend on latest hledger

0.23 (2014/5/1)

-   The --static-root flag has been renamed to --file-url.
-   hledger-web now builds with Cabal's default -O, not -O2,
    so may be a little quicker/less memory-hungry to install.

0.22.8 (2014/4/29)

-   allow shakespeare 2.* (#179)

0.22.7 (2014/4/17)

-   add Peter Simons' patch fixing Data.Conduit.Network HostIPv4 error (#171)

0.22.6 (2014/4/16)

-   depend on hledger\[-lib\] 0.22.2

0.22.5 (2014/4/15)

-   allow http-client 0.3.*, fixing cabal install again with GHC <= 7.6 (not yet 7.8)
-   use pretty-show only with GHC 7.4+, fixing GHC 7.2 (fixes #155)
-   allow warp 2.1, fixing cabal install

0.22.4 (2014/2/10)

-   Fix: include the right unminified version of jquery.url.js (1.1) to avoid js breakage

0.22.3 (2014/2/10)

-   Fix: version number reported by --version

0.22.2 (2014/2/10)

-   new option --static-root to set the base url for static files
-   allow blaze-html 0.7 (#159)
-   Fix: include unminified source of all javascript to help packagers (#161)
-   Fix: work around clang-related build failures with OS X mavericks/XCode 5

0.22.1 (2014/1/6) and older

See http://hledger.org/release-notes or doc/RELNOTES.md.
