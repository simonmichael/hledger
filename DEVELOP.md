---
title: hledger Developer Guide
---

# Developer guide

**Quick links:**

**hledger released version**\
[release notes](NEWS.html)\
[hledger](http://hackage.haskell.org/package/hledger),
[hledger-web](http://hackage.haskell.org/package/hledger-web),
[hledger-interest](http://hackage.haskell.org/package/hledger-interest),
[hledger-irr](http://hackage.haskell.org/package/hledger-irr),
[hledger-vty](http://hackage.haskell.org/package/hledger-vty),
[hledger-chart](http://hackage.haskell.org/package/hledger-chart),
[hledger-lib](http://hackage.haskell.org/package/hledger-lib)
packages\
<!-- [hledger-web demo](http://demo.hledger.org) -->
<!-- <script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/jquery-1.4.2.js"></script> -->
<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/hoogle.js"></script>
<form action="http://haskell.org/hoogle/" method="get" style="display:inline; margin:0; padding:0;">
<input type="hidden" name="prefix" value="+hledger +hledger-lib +hledger-web +hledger-vty +hledger-chart" />
<span style="white-space:nowrap;"
><input type="text" name="hoogle" id="hoogle" accesskey="1" size="30"
/><input type="submit" value="search API with hoogle"
/></span>
</form>

**hledger development version**<br>
[Get it](#how-to-set-up-for-development),
[browse it](http://github.com/simonmichael/hledger),
[changes](http://github.com/simonmichael/hledger/commits),
[CHANGES](http://starlogs.net/#simonmichael/hledger) (turn up your volume!)
<!-- [hledger-web dev demo](http://demo.hledger.org:5001) -->
<br>
[build](http://hydra.cryp.to:8080/project/hledger),
[dependencies](http://packdeps.haskellers.com/feed/?needle=hledger)
<!-- [haddock coverage](http://hledger.org/profs/haddock-coverage), -->
<!-- [unit test coverage](http://hledger.org/profs/coverage/hpc_index_fun.html), -->
<!-- [benchmark](http://hledger.org/profs/latest.bench) -->
<!-- [profile](http://hledger.org/profs/latest.prof), -->
<!-- [heap](http://hledger.org/profs/latest.ps) -->
reports\
<!-- [developer notes](http://github.com/simonmichael/hledger/NOTES.org)\ -->
<!-- [browse dev API docs](http://hledger.org/api/frames.html) -->

\


## How to..

New contributors of all levels are most welcome.
Here are some tips to help you get productive on the hledger project.

### how to suggest enhancements

Suggestions and feature requests are easy to make, less easy to
research, and hard to implement.  And, alas! We don't have a team of
sleek, well-fed, idle coders standing by to implement everybody's
wishes. (Please
[help change that](#how-to-set-up-for-hledger-development)).

Wishes *are* welcome feedback, but we don't want them to pile up and
obscure bugs and other developer priorities, so we manage them with discussion
and optionally as cards on a trello board. 
The current recommendation is

1. **discuss/research first**\
   Is your wish already on the [trello wishlist/planning board](http://hledger.org/trello)
   or [bug tracker](http://hledger.org/bugs) ?\
   Perhaps discuss it on [irc](irc://irc.freenode.net/#ledger), the [mail list](http://hledger.org/list) or trello ?

2. **report wishes on trello, bugs in the bug tracker**\
   Is it a problem with the released and documented functionality ? report in the [bug tracker](http://hledger.org/bugs)\
   Is it a wish ? add a card on the [trello board](http://hledger.org/trello), if needed

3. **don't sweat it**\
   Or, do what you think best. If a wish does land in the bug tracker, it gets the WISH label.

### how to report problems

- check for related issues in the [bug tracker](http://hledger.org/bugs) or in the [mail list archive](http://hledger.org/list)
- discuss/confirm the issue on irc or list
- report new issues in the bug tracker
<!-- - test and share problem journal snippets at paste . hledger.org -->

### how to help with testing

- review and test our documentation and web presence
- download and test the binaries on your platform
- test installing via cabal
- use the tools and test functionality, usability, browser compatibility, ui layout etc.
- check that `hledger test` reports no failures
- [run the developer tests](#how-to-run-the-tests)
- discuss/report problems via irc/mail list/bug tracker

### how to help with bug tracking

- get to know the [bug tracker](http://hledger.org/bugs) and its contents
- research and update issues
- some convenient url shortcuts:  
  [`hledger.org/bugs`](http://hledger.org/bugs)  
  [`hledger.org/bugs/new`](http://hledger.org/bugs/new)  
  `hledger.org/bugs/N`

### how to set up for development

1. get an up-to-date [ghc](http://haskell.org/ghc), at least 7.0 and preferably 7.6
2. there's probably no need to install the [haskell platform](http://haskell.org/platform) now, but you could
3. it's probably worth getting the latest and best cabal: `cabal update; cabal install cabal-install`
4. ensure you have [git](http://git-scm.com) installed
5. the hledger Makefile assumes GNU Make, so on some platforms you may need to spell "make" as "gmake"

- get the hledger repo:
  
        git clone git@github.com:simonmichael/hledger.git
        cd hledger
        
- install packages required to build hledger and add-ons, or as many of them as possible:
  
        cabal update
        make install

    This will also try to cabal install development builds of the hledger
    executables, so ghc-pkg unregister those afterwards if you don't want
    that.

- try building with make:

        make bin/hledgerdev

    This is usually quicker and simpler than fiddling with multiple cabal packages during development.
    Note this executable will not be as optimised as the normal cabal build, and has the "dev" suffix
    as a reminder of this.

- try auto-building with sp:

        make auto   # or autoweb

    You'll need to follow the instructions to install `sp`.
    This is how I do most hledger development. It will recompile whenever you save changes to source files.
        
### how to get your changes accepted

Follow the usual github workflow:

- fork the main hledger repo on github,
- git clone it to your local machine,
- git commit, after (?) pulling and merging the latest upstream changes
- git push back to github,
- open a pull request on github,
- follow up on any discussion there.
  
If you're new to this process, [help.github.com](http://help.github.com) may be useful.

### how to improve the documentation

- get familiar with the website and documentation online, review and test
- get familiar with the site/doc source files (see Makefile)
- set up for hledger development
- send patches with names prefixed with "doc: " (or "site: ")

### how to run the tests

- set up for hledger development
- cabal install shelltestrunner
- make test

### how to add a test

- identify what to test
- choose the test type: unit ? functional ? benchmark ?
- currently expected to pass or fail ?
- figure out where it goes
- write test, verify expected result
- get it committed

### how to fix a bug or add a feature

- research, discuss, validate the issue/feature on irc/list/bug tracker
- look for related tests, run the tests and check they are passing
- add a test ?
- develop a patch
- include any related issue numbers in the patch name, eg: "fix for blah blah (#NNN)"
- get it committed

### how to become a contributor

- after getting one or more patches committed, read and sign the [contributor list & agreement](CONTRIBUTORS.html)
- or, [ask](#how-to-get-help) to be added

### how to do code review

- review and discuss new pull requests and commits on github
- set up for development and test the latest changes in your own repo
- read the existing [code docs and source](#quick-links)
- send feedback or discuss via irc or list

### how to help with packaging

- package hledger for linux distros, macports, etc.
- develop mac/windows installers
- find and assist distro packagers/installer developers

### how to help with project management

- clarify/update goals and principles
- monitor, report on project progress and performance
- research, compare and report on successful projects, related projects
- identify collaboration opportunities
- marketing, communication, outreach
- release management, roadmap planning

## Project overview

A rough overview/blueprint for the hledger project.

### mission, principles, goals

The hledger project aims to produce:

- a practical, accessible, dependable tool for end users
- a useful library and toolbox for finance-minded haskell programmers
- a successful, time-and-money-solvent project within a thriving ecosystem of financial software projects.

### roles and activities

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

### documentation

- website
- user manual
- developer guide
- code documentation: haddock
- various developer reports
- developer notes outline
- blurbs: in cabal files, module headers, google project, repo message of the day..

### quality assurance

- unit tests (HUnit, make unittest)
- functional tests (shelltestrunner, make functest)
- performance tests (simplebench, make bench)
- documentation tests (make haddocktest + manual)
- ui tests (manual)
- installation tests (manual)

### code

- the hledger repo is hosted on github.com:

  [http://github.com/simonmichael/hledger](http://github.com/simonmichael/hledger)
  
  You can also jump there by entering hledger.org/code or code.hledger.org .

<!-- ### release process -->

<!-- ### roadmap -->

<!-- ### communication and collaboration -->

<!-- ### web presence and hosting setup -->

<!-- ### finances and other resources -->

<!-- ### licensing and legal issues -->

<!-- ### contributors and credits -->

## Related projects

- I have a few older bits and pieces [here](http://joyful.com/Ledger)
- John Wiegley's [ledger](http://wiki.github.com/jwiegley/ledger) inspired hledger.
- Tim Docker's [ledger-reports](http://dockerz.net/repos/ledger-reports) builds on hledger to generate
  [html reports](http://dockerz.net/software/hledger_report_sample/report.html)
- [beancount](https://furius.ca/beancount/) is another ledger clone, in python
- h/ledger inspired Uwe Hollerbach's [umm](http://www.korgwal.com/umm/)
- [http://darcsden.com/alex/bill](http://darcsden.com/alex/bill), [http://darcsden.com/alex/bill-atomo](http://darcsden.com/alex/bill-atomo), [http://darcsden.com/alex/bill-scheme](http://darcsden.com/alex/bill-scheme) - a time-tracking and billing app
- [http://darcsden.com/ozamosi/debts](http://darcsden.com/ozamosi/debts) - Silly debt tracking webapp
- [http://darcsden.com/housetab-multi](http://darcsden.com/dbp/housetab-multi), [housetab.org](http://housetab.org) - a webapp to manage expenses between a group of friends.

<!-- <a href="https://www.google.com/analytics/reporting/?reset=1&id=15489822" accesskey="a"></a> -->
