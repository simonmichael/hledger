---
title: hledger developers' guide
---

# Developers' guide

A rough guide for hledger contributors of all kinds, plus some quick links for everyone.

<a name="support" />

## Quick links

- User and developer support: [how to get help](#how-to-get-help)  
- IRC channel: [irc://irc.freenode.net/#ledger](irc://irc.freenode.net/#ledger)  
- Mail list: [http://list.hledger.org](http://list.hledger.org)  
- Bug tracker: [http://bugs.hledger.org](http://bugs.hledger.org)  
- [Release notes](NEWS.html)
- Code:
  [get it](#how-to-set-up-for-hledger-development),
  [browse it](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger),
  [latest api docs](http://hledger.org/api-doc), 
  [latest internal code docs](http://hledger.org/code-doc) for all packages
- Packages:
  [hledger](http://hackage.haskell.org/package/hledger),
  [hledger-lib](http://hackage.haskell.org/package/hledger-lib),
  [hledger-chart](http://hackage.haskell.org/package/hledger-chart),
  [hledger-vty](http://hackage.haskell.org/package/hledger-vty),
  [hledger-web](http://hackage.haskell.org/package/hledger-web)
- Developer reports:
  [unit test coverage](http://hledger.org/profs/coverage/hpc_index_fun.html),
  [benchmark](http://hledger.org/profs/latest.bench),
  [profile](http://hledger.org/profs/latest.prof),
  [heap](http://hledger.org/profs/latest.ps)
- [Developer notes](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger;a=plainblob;f=/NOTES)
<!--   [hledger dependencies](http://packdeps.haskellers.com/feed/?needle=hledger), -->
<!--   [hledger-lib dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-lib), -->
<!--   [hledger-chart dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-chart), -->
<!--   [hledger-vty dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-vty), -->
<!--   [hledger-web dependencies](http://packdeps.haskellers.com/feed/?needle=hledger-web) -->

<script type="text/javascript" src="http://haskell.org/hoogle/jquery-1.4.2.js"></script>
<script type="text/javascript" src="http://haskell.org/hoogle/hoogle.js"></script>
<form action="http://haskell.org/hoogle/" method="get">
 <input type="text"   name="hoogle" id="hoogle" accesskey="1" size="30" />
 <input type="hidden" name="prefix" value="+hledger +hledger-lib" />
 <input type="submit" value="Search hledger, hledger-lib API" />
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
- for issues relevant to the wider *ledger community, you can also use or cc ledger's mail list

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

- install [darcs](http://darcs.net) if you don't have it (version 2)
- get the hledger repo:
  
        darcs get --lazy http://joyful.com/repos/hledger
        cd hledger
        
- install packages required to build hledger and add-ons, or as many of them as possible:
  
        cabal update
        make install

    This will also try to cabal install the development version of the
    hledger packages, so ghc-pkg unregister those afterwards if you don't
    want that.
        
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

- John Wiegley's [ledger](http://wiki.github.com/jwiegley/ledger) inspired hledger.
- [beancount](https://furius.ca/beancount/) is another ledger clone, in python
- h/ledger inspired Uwe Hollerbach's [umm](http://www.korgwal.com/umm/)
- Tim Docker's [ledger-reports](http://dockerz.net/repos/ledger-reports) builds on hledger to generate
  [html reports](http://dockerz.net/software/hledger_report_sample/report.html)
- I have a few older bits and pieces [here](http://joyful.com/Ledger)

<!-- <a href="https://www.google.com/analytics/reporting/?reset=1&id=15489822" accesskey="a"></a> -->
