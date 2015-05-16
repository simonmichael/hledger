<!-- hledger repo and http://hledger.org versions of this document are periodically bidirectionally synced -->

* toc

# Developer guide

## Quick links

<style>
tr {
    border-top:thin solid #bbb;
}
</style>
<!-- | hledger.org&nbsp;&nbsp; | [combined release notes](release notes), [pre-compiled binaries](download) | -->
<!-- [web ui demo](http://demo.hledger.org/register?q=inacct%3Aassets%3Abank%3Achecking+sym%3A\%24) -->
|-------------------------|----------------------------------------------------------------------------|
| IRC                     | Join [#hledger](http://hledger.org/irc) ([chat log](http://ircbrowse.net/browse/hledger); see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) |
| Mail list               | [list.hledger.org](http://list.hledger.org) ([Gmane](http://dir.gmane.org/gmane.comp.finance.ledger.hledger)) |
| Twitter                 | [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a> |
| hledger-web demo&nbsp;&nbsp;        | [demo.hledger.org](http://demo.hledger.org) |
| Trello                  | [planning board](http://hledger.org/trello) |
| Github                  | [code.hledger.org](http://github.com/simonmichael/hledger) <br> [commits](http://github.com/simonmichael/hledger/commits), [COMMITS](http://starlogs.net/#simonmichael/hledger) (turn up your volume), [unreleased commits](https://github.com/simonmichael/hledger/compare/0.23...master), [release branch commits](https://github.com/simonmichael/hledger/compare/master...0.23) <br> [build status (hydra)](http://hydra.cryp.to/jobset/hledger/master#tabs-jobs) <br> [open bugs](https://github.com/simonmichael/hledger/issues?direction=desc&labels=BUG&page=1&sort=created&state=open), [all bugs](https://github.com/simonmichael/hledger/issues?direction=desc&labels=BUG&page=1&sort=created), [open issues](https://github.com/simonmichael/hledger/issues?direction=desc&labels=&page=1&sort=created&state=open), [all issues](https://github.com/simonmichael/hledger/issues?direction=desc&labels=&page=1&sort=created), <a href="https://github.com/simonmichael/hledger/issues?q=label%3Abounty+">issues with bounties</a>, <a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger">issue bounties @ bountysource.com</a> |
| Hackage                 | [hledger-lib](http://hackage.haskell.org/package/hledger-lib), [hledger](http://hackage.haskell.org/package/hledger), [hledger-web](http://hackage.haskell.org/package/hledger-web), [hledger-interest](http://hackage.haskell.org/package/hledger-interest), [hledger-irr](http://hackage.haskell.org/package/hledger-irr), [hledger-vty](http://hackage.haskell.org/package/hledger-vty), [hledger-chart](http://hackage.haskell.org/package/hledger-chart), [\*hledger\*](http://hackage.haskell.org/packages/search?terms=hledger) <br> reverse dependencies: [hledger-lib](http://packdeps.haskellers.com/reverse/hledger-lib), [hledger](http://packdeps.haskellers.com/reverse/hledger), [outdated dependencies](http://packdeps.haskellers.com/feed/?needle=hledger) <br> [download stats](http://best-haskell.herokuapp.com/#/category/Finance) |
| Stackage                | [hledger entry](https://github.com/fpco/stackage/blob/master/Stackage/Config.hs#L449-450), [issues](https://github.com/fpco/stackage/search?q=hledger&ref=cmdform&type=Issues) <br> [build status (jenkins)](http://jenkins.stackage.org/job/Stackage/), last build output: [7.4](http://jenkins.stackage.org/job/Stackage/ghcversion=7.4.2/lastBuild/console), [7.6](http://jenkins.stackage.org/job/Stackage/ghcversion=7.6.3/lastBuild/console), [7.8](http://jenkins.stackage.org/job/Stackage/ghcversion=7.8.2/lastBuild/console) |
| Debian                  | source packages <br> [haskell-hledger-lib](http://packages.qa.debian.org/h/haskell-hledger-lib.html), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-lib), [haskell-hledger](http://packages.qa.debian.org/h/haskell-hledger.html), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger), [haskell-hledger-web](http://packages.qa.debian.org/h/haskell-hledger-web.html), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-web) <br> binary packages <br> testing: [hledger](https://packages.debian.org/testing/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=testing), [hledger-web](https://packages.debian.org/testing/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=testing) <br> unstable: [hledger](https://packages.debian.org/unstable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=unstable), [hledger-web](https://packages.debian.org/unstable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=unstable) <br> all: [\*hledger\*](https://packages.debian.org/search?searchon=names&keywords=hledger) <br> popularity stats: [hledger](https://qa.debian.org/popcon.php?package=haskell-hledger), [hledger-web](https://qa.debian.org/popcon.php?package=haskell-hledger-web) <br> [PTS help](https://www.debian.org/doc/manuals/developers-reference/resources.html#pkg-tracking-system) |
| Ubuntu                  | source packages <br> [haskell-hledger-lib](https://launchpad.net/ubuntu/+source/haskell-hledger-lib), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-lib), [haskell-hledger](https://launchpad.net/ubuntu/+source/haskell-hledger), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger), [haskell-hledger-web](https://launchpad.net/ubuntu/+source/haskell-hledger-web), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-web) <br> binary packages <br> [\*hledger\*](http://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger) |
| Gentoo                  | [hledger](http://gpo.zugaina.org/dev-haskell/hledger), [hledger-web](http://gpo.zugaina.org/dev-haskell/hledger-web), [\*hledger\*](http://gpo.zugaina.org/Search?search=hledger) |
| Fedora                  | [hledger](https://apps.fedoraproject.org/packages/hledger), [\*hledger\*](https://apps.fedoraproject.org/packages/s/hledger), [Haskell SIG](http://fedoraproject.org/wiki/Haskell_SIG) |

<!-- list the debian packages for clarity:
3 source:
haskell-hledger-lib
haskell-hledger
haskell-hledger-web
8 binary:
hledger
hledger-web
libghc-hledger-dev
libghc-hledger-doc
libghc-hledger-prof
libghc-hledger-lib-dev
libghc-hledger-lib-doc
libghc-hledger-lib-prof
-->

<!-- old/future links -->
<!-- [haddock coverage](http://hledger.org/profs/haddock-coverage), -->
<!-- [unit test coverage](http://hledger.org/profs/coverage/hpc_index_fun.html), -->
<!-- [benchmark](http://hledger.org/profs/latest.bench) -->
<!-- [profile](http://hledger.org/profs/latest.prof), -->
<!-- [heap](http://hledger.org/profs/latest.ps) -->
<!-- [developer notes](http://github.com/simonmichael/hledger/NOTES.org)\ -->
<!-- [browse dev API docs](http://hledger.org/api/frames.html) -->
<!-- [How to clone it](developer-guide#set-up-for-development) -->
<!-- [hledger-web dev demo](http://demo.hledger.org:5001) -->

<!-- hoogle search form
<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/jquery-1.4.2.js"></script>
<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/hoogle.js"></script>
<form action="http://haskell.org/hoogle/" method="get" style="display:inline; margin:0; padding:0;">
<input type="hidden" name="prefix" value="+hledger +hledger-lib +hledger-web +hledger-vty +hledger-chart" />
<span style="white-space:nowrap;"
><input type="text" name="hoogle" id="hoogle" accesskey="1" size="30"
/><input type="submit" value="search API with hoogle"
/></span>
</form>
-->


## Project overview

A rough overview/blueprint for the hledger project.

### Mission, principles, goals

The hledger project aims to produce:

- a practical, accessible, dependable tool for end users
- a useful library and toolbox for finance-minded haskell programmers
- a successful, time-and-money-solvent project within a thriving ecosystem of financial software projects.

### Roles and activities

- newcomer/potential user
- user
- library user
- field tester
- bug wrangler
- support
- documentor
- qa
- developer
- packager
- communicator
- project manager

### Documentation

Project documentation lives in a number of places:

- `doc/*.md` and `doc/site/*.md` form the hledger.org website, which is generated with hakyll[-std] and pandoc
- haddock documentation in the code appears on Hackage
- short blurbs: cabal files, module headers, HCAR, GSOC project, ..
- `doc/notes.org` has some old developer notes
- developer reports (profiles, benchmarks, coverage..) in doc/profs, sometimes published at hledger.org/profs

### Code

The hledger repo is hosted on Github, at <http://github.com/simonmichael/hledger>.
You can also jump there via `code.hledger.org[/commits]`.

### Quality control

Relevant tools include:

- unit tests (HUnit, make unittest)
- functional tests (shelltestrunner, make functest)
- performance tests (simplebench, make bench)
- documentation tests (make haddocktest + manual)
- ui tests (manual)
- installation tests (manual)
- code reviews

### Code reviews

We held a code review party, hopefully the first of many, in July 2014 on the mail list and IRC channel.
Here's the original [proposal](http://article.gmane.org/gmane.comp.finance.ledger.hledger/1070) giving some motivation, and the discussion logs, note these are a good source of hledger development tips:

- 2014/7/21-25 **hledger-web code & UI**
  [mail thread](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1070),
  [IRC log](http://hledger.org/static/irc-20140725-code-review.html)


<!-- ### release process -->

<!-- ### roadmap -->

<!-- ### communication and collaboration -->

<!-- ### web presence and hosting setup -->

<!-- ### finances and other resources -->

<!-- ### licensing and legal issues -->

<!-- ### contributors and credits -->

---

## Implementation notes

### hledger

There are two core cabal packages:

**[hledger-lib](http://hackage.haskell.org/package/hledger-lib)** - data model, parsing, manipulation, standard reports
([github](https://github.com/simonmichael/hledger/tree/master/hledger-lib))\
**[hledger](http://hackage.haskell.org/package/hledger)** - command line interface, reusable cli options & helpers
([github](https://github.com/simonmichael/hledger/tree/master/hledger))

Most data types are defined in hledger-lib:Hledger.Data.Types,
while functions that operate on them are defined in
hledger-lib:Hledger.Data.TYPENAME.
Here's a diagram of the main data model:

<uml>
hide empty members
hide circle
skinparam packageStyle Rect

Ledger *-- Journal
Ledger *-- "*" Account
note top of Ledger: A Journal and all its accounts with their balances.\nUsed for balance report
note top of Journal: A journal file and parsed transactions & directives.\nUsed for print & register reports
note bottom of Account: An account's name, balance (inclusive &\nexclusive), parent and child accounts
Account o-- "*" Account :subaccounts, parent
Journal o-- File
File o-- "*" File :include
Journal *-- "*" HistoricalPrice
Journal *-- "*" Transaction
HistoricalPrice -- Date
HistoricalPrice -- Amount
Transaction -- Date
Transaction *-- "*" Posting
Transaction o-- "*" Tag
Posting o- "*" Tag
Posting -- "0..1" Date
Account -- AccountName
Posting -- AccountName
Account -- "2" MixedAmount
Posting -- MixedAmount
MixedAmount *-- "*" Amount
Amount -- Commodity
Amount -- Quantity
Amount -- Price
Amount -- AmountStyle
</uml>

hledger parses the journal file into a
[Journal](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Journal),
which contains a list of
[Transactions](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Transaction),
each containing multiple
[Postings](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Posting)
of some
[MixedAmount](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:MixedAmount)
(multiple
single-[Commodity](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Commodity)
[Amounts](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Amount))
to some
[AccountName](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:AccountName).
Commands get and render 
[Reports](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Reports.html)
from the Journal, or sometimes from a
[Ledger](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Ledger),
which contains
[Accounts](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Account)
representing the summed balances and other details of each account.

After surveying the packages, modules, and data types, try tracing the execution of a hledger command:

1. CLI stuff is in [hledger:Hledger.Cli](https://github.com/simonmichael/hledger/tree/master/hledger/Hledger/Cli).
2. [hledger:Hledger.Cli.Main:main](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Main.hs#L179)
parses the command line to select a command, then
3. gives it to
[hledger:Hledger.Cli.Utils:withJournalDo](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Utils.hs#L61),
which runs it after doing all the initial parsing.
4. Parsing code is under
[hledger-lib:Hledger.Read](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read.hs),
eg the
[hledger-lib:Hledger.Read.JournalReader](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read/JournalReader.hs).
5. Commands extract useful information from the parsed data model using
[hledger-lib:Hledger.Reports](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Reports),
and
6. render it to the console.
7. Everything uses the types and data
utilities under
[hledger-lib:Hledger.Data](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Data),
and the general helpers from
[hledger-lib:Hledger.Utils](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Utils.hs)
and below.

### hledger-web

hledger-web is in a third cabal package:

**[hledger-web](http://hackage.haskell.org/package/hledger-web)** - web interface
([github](https://github.com/simonmichael/hledger/tree/master/hledger-web))

It is a single-executable web application using the
[yesod](http://yesodweb.com) framework.  It runs a built-in web server
serving some views of the journal file, reading it at startup and
again whenever it changes. It can also append new transactions to the journal file.
There are two main views, which can be filtered with [query arguments](manual#query-arguments):

- [/journal](http://demo.hledger.org/journal), showing general journal entries (like `hledger print`)

- [/register](http://demo.hledger.org/register?q=inacct:Assets:Bank:Checking),
  showing transactions affecting an account (slightly different from
  `hledger register`, which shows postings).

There is also:

- a sidebar (toggled by pressing `s`) showing the chart of accounts (like `hledger balance`)
- an [add form](http://demo.hledger.org/journal?add=1) for adding new transactions (press `a`)
- a help dialog showing quick help and keybindings (press `h` or click ?)

Most of the action is in

- [config/routes](https://github.com/simonmichael/hledger/tree/master/hledger-web/config/routes)
- [templates/default-layout-wrapper.hamlet](https://github.com/simonmichael/hledger/tree/master/hledger-web/templates/default-layout-wrapper.hamlet)
- [Foundation](https://github.com/simonmichael/hledger/tree/master/hledger-web/Foundation.hs)
- [Handler.*](https://github.com/simonmichael/hledger/tree/master/hledger-web/Handler)
- [static/hledger.js](https://github.com/simonmichael/hledger/tree/master/hledger-web/static/hledger.js)
- [static/hledger.css](https://github.com/simonmichael/hledger/tree/master/hledger-web/static/hledger.css)

Handler module and function names end with R, like the Yesod-generated route type they deal with.

Dynamically generated page content is mostly inline hamlet.
Lucius/Julius files and widgets generally are not used, except for the default layout.

The quickest way to test changes is `make ghciweb`, `:main --serve`, control-C, `:r`, repeat.
No linking is required, and changes to static files like hledger.js are visible after reloading a page.

Another way is `yesod devel`, which rebuilds automatically when files
change, including config files, templates and static files (but only in the hledger-web package).

A third way is `make autoweb`, if you can get it working (see the
makefile for instructions). This rebuilds automatically when haskell
files change in any of the hledger{-lib,,-web} packages.


---

## How to..

New contributors of all levels are most welcome.
Here are some tips/checklists/procedures to help you get productive on the hledger project.

### Suggest enhancements

Suggestions and feature requests (aka wishes) are easy to make, and can be valuable,
but we don't want them to pile up ad infinitum and obscure bugs and
other developer priorities. Before opening a github issue, consider:

1. Perhaps discussion is most appropriate at this stage ?
   [#hledger](irc://irc.freenode.net/#ledger) or the
   [mail list](http://list.hledger.org) are excellent places for this.
   Both are archived, so the idea won't be lost.
2. We have a collection of enhancement ideas on the
   [trello planning board](http://hledger.org/trello).
   Perhaps your idea is already there, or you can add it ?
3. We have bug reports and more wishes in the [bug tracker](http:///bugs.hledger.org).
   Is your idea already there ?

When a wish does land in the bug tracker, it gets the WISH label,
and the default view given by [bugs.hledger.org](http://bugs.hledger.org) excludes these.

### Report problems

- check for related issues in the [bug tracker](http:///bugs.hledger.org) or in the [mail list archive](http://list.hledger.org)
- discuss/confirm the issue on irc or list
- report new issues in the bug tracker
<!-- - test and share problem journal snippets at paste . hledger.org -->

### Help with testing

- review and test our documentation and web presence
- download and test the binaries on your platform
- test installing via cabal
- use the tools and test functionality, usability, browser compatibility, ui layout etc.
- check that `hledger test` reports no failures
- [run the developer tests](#how-to-run-the-tests)
- discuss/report problems via irc/mail list/bug tracker

### Help with bug tracking

- get to know the [bug tracker](http://bugs.hledger.org) and its contents
- research and update issues
- some convenient url shortcuts:\
  [`bugs.hledger.org`](http://bugs.hledger.org)\
  [`bugs.hledger.org/new`](http://bugs.hledger.org/new)\
  `bugs.hledger.org/N`

### Set up for development

1. Get [GHC](https://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install) installed.
   I recommend the [stackage.org install guide](http://www.stackage.org/install).
   You can see which GHC versions are officially supported in the `tested-with` field in
   [hledger.cabal](http://hackage.haskell.org/package/hledger/hledger.cabal)
   and
   [hledger-web.cabal](http://hackage.haskell.org/package/hledger-web/hledger-web.cabal).
   Older versions may also work.
2. Get [git](http://git-scm.com) installed.
3. Get [GNU Make](http://www.gnu.org/software/make) installed (unless you don't care about the Makefile's conveniences).
   On some platforms the command will be eg `gmake` instead of `make`.
4. Get the hledger repo:

        git clone https://github.com/simonmichael/hledger.git

5. You might want to install or upgrade some of these haskell developer tools.
   If you're not sure, skip this step and return to it as needed.
   Be sure the cabal bin directory where these are installed (eg ~/.cabal/bin) is in your PATH.

        cabal update
        cabal install alex happy       # if you get alex/happy-related errors when building hledger
        cabal install haddock          # needed to build hledger API docs
        cabal install shelltestrunner  # needed to run hledger functional tests (may need latest git version)
        cabal install hoogle hlint     # maybe useful for searching API docs and checking code

    You'll also want a comfortable code editor, preferably with Haskell support.
    (I use emacs + [haskell-mode](https://github.com/haskell/haskell-mode),
    or occasionally [IntelliJ IDEA](https://www.jetbrains.com/idea/download) + one of the [plugins](https://www.google.com/search?hl=en&q=intellij+plugins+haskell)).

6. Install haskell libs required by hledger:

        cabal update
        cd hledger
        cabal sandbox init   # optional
        make installdeps     # or cabal install --only-dep ./hledger-lib ./hledger [./hledger-web]

    This will install the required dependencies from Hackage.
    If you're new to cabal, you can expect problems at this stage.
    The usual remedy is to ensure you start with a clean package db, eg by doing `cabal sandbox init`.
    You can simplify and speed up this step a lot by commenting out
    hledger-web in the `PACKAGES` list in the [Makefile](https://github.com/simonmichael/hledger/blob/master/Makefile#L41).

7. Build with cabal:

        make cabalbuild

    (Tip: `make cabalCMD` runs `cabal CMD` in each of the hledger packages).

8. Build with GHC:

        make bin/hledgerdev

    This builds hledger (and hledger-lib) with GHC directly, without using cabal,
    and as quickly as possible, without optimizations (the "dev" suffix is a reminder of this).
    I use and recommend this method for development, as it crosses package boundaries and ensures you are building the latest code.
    However it needs some files generated by cabal build, which is why we did that first.


### Get your changes accepted

Follow the usual github workflow:

- fork the main hledger repo on github,
- git clone it to your local machine,
- git commit, after (?) pulling and merging the latest upstream changes
- git push back to github,
- open a pull request on github,
- follow up on any discussion there.

If you're new to this process, [help.github.com](http://help.github.com) may be useful.

### Improve the documentation

- get familiar with the website and documentation online, review and test
- get familiar with the site/doc source files (see Makefile)
- set up for hledger development
- send patches with names prefixed with "doc: " (or "site: ")

### Run the tests

- set up for hledger development
- cabal install shelltestrunner
- make test

### Add a test

- identify what to test
- choose the test type: unit ? functional ? benchmark ?
- currently expected to pass or fail ?
- figure out where it goes
- write test, verify expected result
- get it committed

### Fix a bug or add a feature

- research, discuss, validate the issue/feature on irc/list/bug tracker
- look for related tests, run the tests and check they are passing
- add a test ?
- develop a patch
- include any related issue numbers in the patch name, eg: "fix for blah blah (#NNN)"
- get it committed

### Become a contributor

- after getting one or more patches committed, read and sign the [contributor list & agreement](contributors.html)
- or, [ask](#how-to-get-help) to be added

### Do code review

- review and discuss new pull requests and commits on github
- set up for development and test the latest changes in your own repo
- read the existing [code docs and source](#quick-links)
- send feedback or discuss via irc or list

### Help with packaging

- package hledger for linux distros, macports, etc.
- develop mac/windows installers
- find and assist distro packagers/installer developers

### Help with project management

- clarify/update goals and principles
- monitor, report on project progress and performance
- research, compare and report on successful projects, related projects
- identify collaboration opportunities
- marketing, communication, outreach
- release management, roadmap planning

### Do a major release

1. cleanup
    - review working copies (laptop, server, website) & branches, commit pending changes
2. document
    - */*.cabal (descriptions, tested-with, files..)
    - haddocks
    - */CHANGES
    - doc/contributors.md
    - doc/site/release-notes.md
    - doc/manual.md (commands, options, --help, ledger compatibility..)
    - doc/site/step-by-step.md
    - doc/site/how-to-*
    - doc/site/faq.md (ledger compatibility)
    - doc/site/installing.md
    - doc/ANNOUNCE
3. test
    - coarse tests
      - make unittest
      - make functest
      - make haddocktest
4. branch
    - start release branch (git checkout -b X.Y)
5. version
    - edit .version
    - make setversion
    - double-check (cabal files, manual, download page..)
6. package
    - check Makefile's PACKAGES includes all
    - make cabalsdist
    - [make windows binaries]
    - [make osx binaries]
7. test
    - fine tests
      - install from tarballs into a clean directory
8. tag
    - make tagrelease
9. push
    - git push --tags
10. upload
    - make cabalupload
11. announce
    - email hledger haskell-cafe haskell [ledger]
    - tweet
    - [blog]
    - [reddit]

### Do a minor release

Differences from a major release: set PACKAGES only to the affected package(s),
don't run make setversion. Use make -n if unsure.

1. cleanup
    - review working copies (laptop, server, website) & branches, commit pending changes
2. document
    - */*.cabal for affected package(s) (descriptions, tested-with, files..)
    - */CHANGES for affected package(s)
    - doc/site/release-notes.md
    - doc/manual.md (commands, options, --help, ledger compatibility..)
    - doc/site/step-by-step.md
    - doc/site/how-to-*
3. test
    - make unittest
    - make functest
    - make haddocktest
4. branch
    - switch to release branch (git checkout X.Y)
5. version
    - edit .version (don't make setversion)
    - manually bump version for affected package(s): cabal files, manual..
6. package
    - set Makefile's PACKAGES to affected package(s)
    - make cabalsdist
7. test
    - install from tarball(s) into a clean directory
8. tag
    - make tagrelease
9. push
    - git push --tags
10. upload
    - make cabalupload
11. announce
    - [email hledger]
    - [tweet]
