---
title: hledger developers' guide
---

# Developers' guide

Quick links for everyone, plus a rough guide mostly for hledger
contributors and developers.

<a name="support" />

## Quick links

**Support**<br>
  Users and developers: [how to get help](#how-to-get-help)
  <br>IRC channel: [irc.freenode.net/#ledger](irc://irc.freenode.net/#ledger)
  <br>Mail list: [list.hledger.org](http://list.hledger.org)
  <br>Bug tracker: [bugs.hledger.org](http://bugs.hledger.org)
  (also `bugs.hledger.org/NNN`, `bugs.hledger.org/new`, `bugs.hledger.org/grid`)

**Released version**<br>
  [Binaries](DOWNLOADS.html), [release notes](NEWS.html), [hledger-web demo](http://demo.hledger.org)<br>
  Hackage:
  [hledger](http://hackage.haskell.org/package/hledger),
  [hledger-web](http://hackage.haskell.org/package/hledger-web),
  [hledger-vty](http://hackage.haskell.org/package/hledger-vty),
  [hledger-chart](http://hackage.haskell.org/package/hledger-chart),
  [hledger-lib](http://hackage.haskell.org/package/hledger-lib)

<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/jquery-1.4.2.js"></script>
<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/hoogle.js"></script>
<form action="http://haskell.org/hoogle/" method="get" style="display:inline; margin:0; padding:0;">
<input type="hidden" name="prefix" value="+hledger +hledger-lib +hledger-web +hledger-vty +hledger-chart" />
**Latest development version**<br>
  [Get it](#how-to-set-up-for-hledger-development),
  [browse it](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger),
  [hledger-web dev demo](http://demo.hledger.org:5001)
  <br>[API haddocks](http://hledger.org/api)
  <span style="white-space:nowrap;"
  ><input type="text" name="hoogle" id="hoogle" accesskey="1"
  /><input type="submit" value="hoogle search"
  /></span>
  <br>[Developer notes](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=plainblob;f=/NOTES),
  [haddock coverage](http://hledger.org/profs/haddock-coverage),
  [unit test coverage](http://hledger.org/profs/coverage/hpc_index_fun.html),
  [benchmark](http://hledger.org/profs/latest.bench),
  [profile](http://hledger.org/profs/latest.prof),
  [heap](http://hledger.org/profs/latest.ps)
<!--   [hledger dependencies](http://packdeps.haskellers.com/feed/?needle=hledger), -->
<!--   [hledger-lib dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-lib), -->
<!--   [hledger-chart dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-chart), -->
<!--   [hledger-vty dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-vty), -->
<!--   [hledger-web dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-web) -->
reports
</form>

## How to..

### how to get help

- <form action="http://groups.google.com/group/hledger/boxsubscribe" >
  join and use the [hledger mail list](http://list.hledger.org). Your email:
  <input type=text name=email><input type=submit name="sub" value="Subscribe">
  </form>
- chat Simon (sm) on the
  [\#ledger](irc://irc.freenode.net/#ledger) irc channel which we
  share, or [send email](mailto:simon@joyful.com?subject=hledger:)
- for issues relevant to the wider *ledger community, you can also use or cc [ledger's mail list](http://list.ledger-cli.org)
- don't forget the [installation tips](MANUAL.html#installing), [Troubleshooting](MANUAL.html#troubleshooting) section, and [bug tracker](http://bugs.hledger.org)

### how to report problems

- check for related issues at [bugs.hledger.org](http://bugs.hledger.org) ([view all](http://bugs.hledger.org/grid)) or in the [list archive](http://groups.google.com/group/hledger/topics)
- discuss/confirm the issue on irc or list
- [report](http://code.google.com/p/hledger/issues/entry) new issues in the bug tracker
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

- get to know the bug tracker and its contents
- join its google project for more access
- research and update issues

### how to set up for hledger development

1. get an up-to-date [ghc](http://haskell.org/ghc), at least 6.12.3 but preferably 7
2. there's probably no need to install the [haskell platform](http://haskell.org/platform) now, but you could
3. it's probably worth getting the latest and best cabal: `cabal update; cabal install cabal-install`
4. get an up-to-date [darcs](http://darcs.net), at least 2.x and preferably newer: use a binary package or `cabal install darcs`

- get the hledger repo:
  
        darcs get --lazy http://joyful.com/repos/hledger
        cd hledger
        
- install packages required to build hledger and add-ons, or as many of them as possible:
  
        cabal update
        make install

    This will also try to cabal install development builds of the hledger
    executables, so ghc-pkg unregister those afterwards if you don't want
    that.

- try building with make:

        make bin/hledger

    This is usually quicker and simpler than fiddling with multiple cabal packages during development.

- try auto-building with sp:

        darcs get http://joyful.com/repos/searchpath
        searchpath$ make, add sp to your path
        hledger$ make autotest

    This is how I do most hledger development. It will recompile whenever you save changes to source files.
    You'll need to install `sp` as shown.
        
- test patch sending. Make a dummy change:

        echo >>README.markdown; darcs record README.markdown -a -m 'my test patch'
        
    send it to yourself:
    
        darcs send --to me@my.address
        
    and make sure you receive it. If not, your system may not be
    configured to send email from the command line. Try to fix that. As a
    last resort, you can `darcs send -O` and mail the resulting patch file
    to the list. Finally, clean up:
    
        darcs obliterate -p 'my test patch'

### how to get your patch committed

- [send it](#how-to-set-up-for-hledger-development)
- you should receive a reply shortly. If in doubt, [follow up](#how-to-get-help) at any time.
- respond to any code review feedback, submitting new patches if needed, until you receive a "patch applied" acknowledgement
- to verify the patch is in the main repo: listen for it on irc or look for it in darcsweb

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

- watch for and read new patches on the mail list, irc, darcsweb patch feed, or [darcsweb patch log](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=shortlog)
- set up for development and test new patches in your own repo
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

## Project notes

This will grow into an overall description/blueprint for the hledger project.

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

- main darcs repo is http://joyful.com/repos/hledger, browse with [darcsweb](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger)

<a href="http://joyful.com/darcsweb/darcsweb.cgi?r=hledger"><img src=http://joyful.com/repos/hledger/commits.png border=0></a>

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
