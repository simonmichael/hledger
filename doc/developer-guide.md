<!-- hledger repo and http://hledger.org versions of this document are periodically bidirectionally synced -->

# Developer guide

## Quick links

**Released version:**\\
[[Release notes]],
[[download|Downloads]]\\
Hackage:
[hledger-lib](http://hackage.haskell.org/package/hledger-lib),
[hledger](http://hackage.haskell.org/package/hledger),
[hledger-web](http://hackage.haskell.org/package/hledger-web),
[hledger-interest](http://hackage.haskell.org/package/hledger-interest),
[hledger-irr](http://hackage.haskell.org/package/hledger-irr),
[hledger-vty](http://hackage.haskell.org/package/hledger-vty),
[hledger-chart](http://hackage.haskell.org/package/hledger-chart)\\
Debian: [haskell-hledger](http://packages.qa.debian.org/h/haskell-hledger.html)\\
Ubuntu: [haskell-hledger](https://launchpad.net/ubuntu/+source/haskell-hledger)\\
RedHat/Fedora: [hledger](http://fr2.rpmfind.net/linux/rpm2html/search.php?query=hledger&submit=Search+...)\\
Gentoo: [hledger](http://gpo.zugaina.org/Search?search=hledger)\\

<!--
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

**Development version:**\\
[Commits](http://github.com/simonmichael/hledger/commits),
[COMMITS!](http://starlogs.net/#simonmichael/hledger) (turn up your volume),
[Code](http://github.com/simonmichael/hledger),
[[developer-guide#set-up-for-development|How to clone it]]
<!-- [hledger-web dev demo](http://demo.hledger.org:5001) -->
\\
Reports:
[build](http://hydra.cryp.to:8080/project/hledger),
[dependencies](http://packdeps.haskellers.com/feed/?needle=hledger)
<!-- [haddock coverage](http://hledger.org/profs/haddock-coverage), -->
<!-- [unit test coverage](http://hledger.org/profs/coverage/hpc_index_fun.html), -->
<!-- [benchmark](http://hledger.org/profs/latest.bench) -->
<!-- [profile](http://hledger.org/profs/latest.prof), -->
<!-- [heap](http://hledger.org/profs/latest.ps) -->
<!-- [developer notes](http://github.com/simonmichael/hledger/NOTES.org)\ -->
<!-- [browse dev API docs](http://hledger.org/api/frames.html) -->

---

*2014: needs an update*

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

Project documentation appears in many places:

- website
- user manual
- developer guide
- code documentation: haddock
- various developer reports
- developer notes outline
- blurbs: in cabal files, module headers, google project, repo message of the day..

For most of hledger's history, documentation was primarily (all capitals) markdown files in the source code repository. This was processed by pandoc, hakyll or yst to generate a static website.

As of 2014/2, hledger.org is a wiki, and most docs are maintained as wiki pages.
Some, particularly the user manual and release notes, still exist in the main source repo as well.
These should probably not be updated directly, instead they are copied from the wiki before each release.

### Quality control

Relevant tools include:

- unit tests (HUnit, make unittest)
- functional tests (shelltestrunner, make functest)
- performance tests (simplebench, make bench)
- documentation tests (make haddocktest + manual)
- ui tests (manual)
- installation tests (manual)

### Code

- the hledger repo is hosted on github.com: http://github.com/simonmichael/hledger.
  (You can also jump there via [hledger.org/code](http://hledger.org/code) or code.hledger.org).

<!-- ### release process -->

<!-- ### roadmap -->

<!-- ### communication and collaboration -->

<!-- ### web presence and hosting setup -->

<!-- ### finances and other resources -->

<!-- ### licensing and legal issues -->

<!-- ### contributors and credits -->

### Data model

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



## How to..

New contributors of all levels are most welcome.
Here are some tips to help you get productive on the hledger project.

### Suggest enhancements

Suggestions and feature requests are easy to make. They are welcome feedback, 
but we don't want them to pile up and
obscure bugs and other developer priorities, so we try manage them a. with discussion
and b. optionally as cards on a trello board.
The current recommendation is

1. **discuss/research first**\\
   Is your wish already on the [trello wishlist/planning board](http://hledger.org/trello)
   or [bug tracker](http://hledger.org/bugs) ?\\
   In any case, perhaps discuss it on [irc](irc://irc.freenode.net/#ledger) or the [mail list](http://hledger.org/list) first ?

2. **wishes are best stored on trello**\\
   Is it a problem with the current released product ? report in the [bug tracker](http://hledger.org/bugs)\\
   Is it a feature idea or wish ? add a card on the [trello board](http://hledger.org/trello), if needed

But do what you think best. When a wish does land in the bug tracker, it gets the WISH label.

### Report problems

- check for related issues in the [bug tracker](http://hledger.org/bugs) or in the [mail list archive](http://hledger.org/list)
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

- get to know the [bug tracker](http://hledger.org/bugs) and its contents
- research and update issues
- some convenient url shortcuts:\\
  [`hledger.org/bugs`](http://hledger.org/bugs)\\
  [`hledger.org/bugs/new`](http://hledger.org/bugs/new)\\
  `hledger.org/bugs/N`

### Set up for development

1. get an up-to-date [ghc](http://haskell.org/ghc), at least 7.0 and preferably 7.6
2. there's probably no need to install the [haskell platform](http://haskell.org/platform) now, but you could
3. it's probably worth getting the latest and best cabal: `cabal update; cabal install cabal-install`
4. ensure you have [git](http://git-scm.com) installed
5. the hledger Makefile assumes GNU Make, so on some platforms you may need to spell "make" as "gmake"
6. get the hledger repo:

        git clone git@github.com:simonmichael/hledger.git
        cd hledger

7. install packages required to build hledger and add-ons, or as many of them as possible:

        cabal update
        make install

    This will also try to cabal install development builds of the hledger
    executables, so ghc-pkg unregister those afterwards if you don't want
    that.

8. try building with make:

        make bin/hledgerdev

    This is usually quicker and simpler than fiddling with multiple cabal packages during development.
    Note this executable will not be as optimised as the normal cabal build, and has the "dev" suffix
    as a reminder of this.

9. try auto-building with sp:

        make auto   # or autoweb

    You'll need to follow the instructions to install `sp`.
    This is how I do most hledger development. It will recompile whenever you save changes to source files.

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

- after getting one or more patches committed, read and sign the [contributor list & agreement](CONTRIBUTORS.html)
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

