# Contributor Guide

<!-- toc -->

## Quick Links

| | |
|-------------------------------|----------------------------------------------------------------------------
| IRC                           | [#hledger](https://kiwiirc.com/nextclient/irc.libera.chat:+6697/#hledger). Also: [#hledger-bots](http://kiwiirc.com/nextclient/irc.libera.chat:+6697/#hledger-bots)
| Mail list                     | [list.hledger.org](http://list.hledger.org) 
| Twitter                       | [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime). Also: [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a>, [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime) 
| Reddit                        | [/r/plaintextaccounting](https://www.reddit.com/r/plaintextaccounting/) 
| Stack Exchange                | [money.stackexchange.com?hledger](https://money.stackexchange.com/search?q=hledger&tab=newest) 
| Hacker News                   | [hledger mentions](https://hn.algolia.com/?query=hledger&sort=byDate&prefix&page=0&dateRange=all&type=all) 
| hledger-web demo&nbsp;&nbsp;  | [demo.hledger.org](http://demo.hledger.org) 
| hledger interactive demo      | <https://hledger.alhur.es>  (hledger compiled with GHCJS)
| Trello                        | [old wishlist planning board](http://trello.hledger.org) 
| Github                        | [simonmichael/hledger](https://github.com/simonmichael/hledger) (shortcut: code.hledger.org)<br> [commits](http://github.com/simonmichael/hledger/commits), <!-- [unreleased commits](https://github.com/simonmichael/hledger/compare/0.23...master), [release branch commits](https://github.com/simonmichael/hledger/compare/master...0.23), --> [COMMITS!](http://starlogs.net/#simonmichael/hledger) <br> [open bugs](http://bugs.hledger.org), [open wishes](http://wishes.hledger.org), [open unknowns](https://github.com/simonmichael/hledger/issues?utf8=✓&q=is%3Aissue%20is%3Aopen%20-label%3A%22A%20BUG%22%20-label%3A%22A%20WISH%22%20), [open pull requests](http://prs.hledger.org), [draft open pull requests](http://draftprs.hledger.org), [ready open pull requests](http://readyprs.hledger.org), [all issues](https://github.com/simonmichael/hledger/issues?q=) <br> [issues with bounty tag](https://github.com/simonmichael/hledger/issues?q=label:bounty), [bountysource bounties](https://github.com/simonmichael/hledger/issues?q=%22Add%20to%20the%20bounty%20at%20Bountysource%22%20OR%20%22claim%20the%20bounty%20on%20Bountysource%22%20OR%20%22bounty%20on%20this%20issue%20has%20been%20claimed%20at%20Bountysource%22%20), [codemill bounties](https://github.com/simonmichael/hledger/issues?q=codemill), [codefund bounties](https://github.com/simonmichael/hledger/issues?utf8=✓&q=codefund) <br> [stars.hledger.org](http://stars.hledger.org):  <a class="github-button" href="https://github.com/simonmichael/hledger" data-icon="octicon-star" data-count-href="/simonmichael/hledger/stargazers" data-count-api="/repos/simonmichael/hledger#stargazers_count" data-count-aria-label="# stargazers on GitHub" aria-label="Star simonmichael/hledger on GitHub"></a> our rank among starred haskell projects:<br>2016: #71, 2017: #54, 2018: #53, 2020: #36 <br> [github projects](https://github.com/simonmichael/hledger/projects) <br> [ci.hledger.org](http://ci.hledger.org) [![hledger CI](https://github.com/simonmichael/hledger/workflows/hledger%20CI/badge.svg)](https://github.com/simonmichael/hledger/actions) 
| Hackage                       | <a name=hackage></a>packages: [hledger-lib](http://hackage.haskell.org/package/hledger-lib), [hledger](http://hackage.haskell.org/package/hledger), [hledger-ui](http://hackage.haskell.org/package/hledger-ui), [hledger-web](http://hackage.haskell.org/package/hledger-web), [hledger-diff](http://hackage.haskell.org/package/hledger-diff), [hledger-iadd](http://hackage.haskell.org/package/hledger-iadd), [hledger-interest](http://hackage.haskell.org/package/hledger-interest), [hledger-irr](http://hackage.haskell.org/package/hledger-irr), [\*hledger\*](http://hackage.haskell.org/packages/search?terms=hledger) <!-- [![...](https://img.shields.io/hackage/v/hledger.svg?label=current+release)](http://hackage.haskell.org/package/hledger) --> <br> diffs: [hledger-lib](http://hdiff.luite.com/cgit/hledger-lib/diff), [hledger](http://hdiff.luite.com/cgit/hledger/diff), [hledger-ui](http://hdiff.luite.com/cgit/hledger-ui/diff), [hledger-web](http://hdiff.luite.com/cgit/hledger-web/diff) <br> build status: [hledger-lib](http://matrix.hackage.haskell.org/package/hledger-lib), [hledger](http://matrix.hackage.haskell.org/package/hledger), [hledger-ui](http://matrix.hackage.haskell.org/package/hledger-ui), [hledger-web](http://matrix.hackage.haskell.org/package/hledger-web) <br> reverse deps: [hledger-lib](http://packdeps.haskellers.com/reverse/hledger-lib), [hledger](http://packdeps.haskellers.com/reverse/hledger), [hledger-ui](http://packdeps.haskellers.com/reverse/hledger-ui), [hledger-web](http://packdeps.haskellers.com/reverse/hledger-web) <br> [![on hackage](https://img.shields.io/hackage/v/hledger.svg?label=hackage&colorB=green)](http://hackage.haskell.org/package/hledger) <br> [![...](https://img.shields.io/hackage-deps/v/hledger-lib.svg?label=hledger-lib+bounds)](http://packdeps.haskellers.com/feed?needle=hledger-lib) [![...](https://img.shields.io/hackage-deps/v/hledger.svg?label=hledger+bounds)](http://packdeps.haskellers.com/feed?needle=hledger) <br> [![...](https://img.shields.io/hackage-deps/v/hledger-ui.svg?label=hledger-ui+bounds)](http://packdeps.haskellers.com/feed?needle=hledger-ui) [![...](https://img.shields.io/hackage-deps/v/hledger-web.svg?label=hledger-web+bounds)](http://packdeps.haskellers.com/feed?needle=hledger-web) 
| Stackage                      | [build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) <br> [open hledger-related issues](https://github.com/fpco/stackage/search?q=hledger+is%3Aopen&type=Issues) <br> packages: [hledger-lib](https://www.stackage.org/package/hledger-lib), [hledger](https://www.stackage.org/package/hledger), [hledger-ui](https://www.stackage.org/package/hledger-ui), [hledger-web](https://www.stackage.org/package/hledger-web)<br> versions: [hledger-lib](https://www.stackage.org/package/hledger-lib/snapshots), [hledger](https://www.stackage.org/package/hledger/snapshots), [hledger-ui](https://www.stackage.org/package/hledger-ui/snapshots), [hledger-web](https://www.stackage.org/package/hledger-web/snapshots) <br> [![](https://repology.org/badge/version-for-repo/stackage_lts/hledger.svg)](https://repology.org/metapackage/hledger) [![...](https://repology.org/badge/version-for-repo/stackage_nighly/hledger.svg)](https://repology.org/metapackage/hledger) 
| Repology                      | [quick hledger packaging status](https://repology.org/metapackage/hledger/badges), [detailed \*hledger\* packaging status](https://repology.org/metapackages/?search=hledger) <br> [![...](https://repology.org/badge/tiny-repos/hledger.svg)](https://repology.org/metapackage/hledger)
| Debian                        | source packages: [haskell-hledger-lib](http://tracker.debian.org/pkg/haskell-hledger-lib), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-lib), [haskell-hledger](http://tracker.debian.org/pkg/haskell-hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger), [haskell-hledger-ui](http://tracker.debian.org/pkg/haskell-hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-ui), [haskell-hledger-web](http://tracker.debian.org/pkg/haskell-hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-web) <br>stable: [hledger](https://packages.debian.org/stable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=stable), [hledger-ui](https://packages.debian.org/stable/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=stable), [hledger-web](https://packages.debian.org/stable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=stable) <br>testing: [hledger](https://packages.debian.org/testing/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=testing), [hledger-ui](https://packages.debian.org/testing/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=testing), [hledger-web](https://packages.debian.org/testing/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=testing) <br>unstable: [hledger](https://packages.debian.org/unstable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=unstable), [hledger-ui](https://packages.debian.org/unstable/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=unstable), [hledger-web](https://packages.debian.org/unstable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=unstable) <br>all: [\*hledger\*](https://packages.debian.org/search?searchon=names&keywords=hledger) <br> sampled install stats: [hledger](https://qa.debian.org/popcon.php?packages=hledger), [hledger-ui](https://qa.debian.org/popcon-graph.php?packages=hledger-ui), [hledger-web](https://qa.debian.org/popcon-graph.php?packages=hledger-web)
| Ubuntu                        | source packages: [haskell-hledger-lib](https://launchpad.net/ubuntu/+source/haskell-hledger-lib), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-lib), [haskell-hledger](https://launchpad.net/ubuntu/+source/haskell-hledger), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger), [haskell-hledger-ui](https://launchpad.net/ubuntu/+source/haskell-hledger-ui), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-ui), [haskell-hledger-web](https://launchpad.net/ubuntu/+source/haskell-hledger-web), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-web) <br> binary packages: [\*hledger\*](http://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger) 
| Gentoo                        | [hledger](http://gpo.zugaina.org/dev-haskell/hledger), [hledger-web](http://gpo.zugaina.org/dev-haskell/hledger-web), [\*hledger\*](http://gpo.zugaina.org/Search?search=hledger) 
| Fedora                        | [hledger](https://apps.fedoraproject.org/packages/hledger), [\*hledger\*](https://apps.fedoraproject.org/packages/s/hledger), [hledger (package db)](https://admin.fedoraproject.org/pkgdb/package/hledger/), [Haskell SIG](http://fedoraproject.org/wiki/Haskell_SIG) 
| Void Linux                    | [package search](https://voidlinux.org/packages/) -> hledger 
| Nix                           | [\*hledger\*](http://hydra.nixos.org/search?query=hledger) 
| Homebrew                      | [hledger](https://formulae.brew.sh/formula/hledger) <br> our 1-year homebrew rank: <br>2020: [#1520 of 10000 on mac](https://formulae.brew.sh/analytics/install-on-request/365d), [#762 of 8288 on linux](https://formulae.brew.sh/analytics-linux/install-on-request/365d/)
| Sandstorm                     | [hledger web app & reviews](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90), [issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=label%3A%22platform%3A%20sandstorm%22%20)
| Reference                     | [fosskers GHC compatibility chart](https://www.fosskers.ca/en/blog/base) 

<!-- | hledger-api demo        | [api-demo.hledger.org/api/v1/accounts](http://api-demo.hledger.org/api/v1/accounts), [api-demo.hledger.org/swagger.json](http://api-demo.hledger.org/swagger.json), [in swagger editor](http://editor2.swagger.io/#/?import=api-demo.hledger.org/swagger.json&no-proxy) <br> [unfinished angular sample app](http://api-demo.hledger.org) ([code](https://github.com/simonmichael/hledger/tree/master/hledger-api/examples/angular)) -->
<!-- 
| Travis CI               | [![...](https://api.travis-ci.org/simonmichael/hledger.svg?branch=master)](https://travis-ci.org/simonmichael/hledger/builds)
| Appveyor CI             | [![...](https://ci.appveyor.com/api/projects/status/5vejw0w5n5igdr42?svg=true)](https://ci.appveyor.com/project/simonmichael/hledger/history) 
| Azure CI                | <a href="https://dev.azure.com/simonmic/hledger/_build"><img src="https://dev.azure.com/simonmic/hledger/_apis/build/status/simonmichael.hledger?branchName=master" alt="..."></a>
-->

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

## Open issues

An overview of hledger's [issue tracker](https://github.com/simonmichael/hledger/issues).
A good place to start looking for something to work on.
(Another contribution idea: a github API script to generate this table along with issue counts.)

<!-- 
This table doesn't have to be aligned, but it helps.
Editing it may require editor support, search/replace etc.
Syntax: http://www.pandoc.org/MANUAL.html#tables -> pipe_tables
-->

| COMPONENT [*](#components)                                                                                                 | BUGS                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | WISHES                                                                                                                 | PRS                                                                                           | OTHER
|----------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------
| [all](https://github.com/simonmichael/hledger/issues?q=is:open)                                                            | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?))                                                                                                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22)                         | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr)                         | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22)
| **Tools:**
| [install](https://github.com/simonmichael/hledger/issues?q=is:open+label:install) (hledger-install.sh)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:install) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:install)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:install)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:install))                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:install)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:install)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:install)
| [cli](https://github.com/simonmichael/hledger/issues?q=is:open+label:cli) (hledger)                                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:cli) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:cli)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:cli)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:cli))                                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:cli)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:cli)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:cli)
| [ui](https://github.com/simonmichael/hledger/issues?q=is:open+label:ui) (hledger-ui)                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:ui) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:ui)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:ui)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:ui))                                                             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:ui)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:ui)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:ui)
| [web](https://github.com/simonmichael/hledger/issues?q=is:open+label:web) (hledger-web)                                    | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:web) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:web)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:web)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:web))                                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:web)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:web)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:web)
| **Input/Output Formats:**
| [journal](https://github.com/simonmichael/hledger/issues?q=is:open+label:journal)                                          | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:journal) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:journal)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:journal)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:journal))                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:journal)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:journal)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:journal)
| [timeclock](https://github.com/simonmichael/hledger/issues?q=is:open+label:timeclock)                                      | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:timeclock) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:timeclock)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:timeclock)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:timeclock))                                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:timeclock)         | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:timeclock)         | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:timeclock)
| [timedot](https://github.com/simonmichael/hledger/issues?q=is:open+label:timedot)                                          | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:timedot) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:timedot)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:timedot)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:timedot))                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:timedot)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:timedot)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:timedot)
| [csv](https://github.com/simonmichael/hledger/issues?q=is:open+label:csv)                                                  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:csv) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:csv)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:csv)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:csv))                                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:csv)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:csv)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:csv)
| [json](https://github.com/simonmichael/hledger/issues?q=is:open+label:json)                                                | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:json) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:json)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:json)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:json))                                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:json)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:json)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:json)
| [html](https://github.com/simonmichael/hledger/issues?q=is:open+label:html)                                                | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:html) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:html)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:html)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:html))                                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:html)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:html)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:html)
| **Commands:**
| [accounts](https://github.com/simonmichael/hledger/issues?q=is:open+label:accounts)                                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:accounts) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:accounts)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:accounts)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:accounts))                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:accounts)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:accounts)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:accounts)
| [activity](https://github.com/simonmichael/hledger/issues?q=is:open+label:activity)                                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:activity) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:activity)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:activity)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:activity))                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:activity)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:activity)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:activity)
| [add](https://github.com/simonmichael/hledger/issues?q=is:open+label:add)                                                  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:add) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:add)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:add)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:add))                                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:add)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:add)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:add)
| [balcmds](https://github.com/simonmichael/hledger/issues?q=is:open+label:balcmds) (bal/bs/bse/cf/is/...)                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:balcmds) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:balcmds)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:balcmds)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:balcmds))                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:balcmds)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balcmds)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:balcmds)
| [balance](https://github.com/simonmichael/hledger/issues?q=is:open+label:balance)                                          | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:balance) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:balance)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:balance)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:balance))                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:balance)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balance)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:balance)
| [balancesheet](https://github.com/simonmichael/hledger/issues?q=is:open+label:balancesheet)                                | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:balancesheet) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:balancesheet)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:balancesheet)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:balancesheet))                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:balancesheet)      | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balancesheet)      | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:balancesheet)
| [cashflow](https://github.com/simonmichael/hledger/issues?q=is:open+label:cashflow)                                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:cashflow) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:cashflow)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:cashflow)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:cashflow))                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:cashflow)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:cashflow)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:cashflow)
| [checkdates](https://github.com/simonmichael/hledger/issues?q=is:open+label:checkdates)                                    | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:checkdates) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:checkdates)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:checkdates)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:checkdates))                             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:checkdates)        | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:checkdates)        | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:checkdates)
| [checkdupes](https://github.com/simonmichael/hledger/issues?q=is:open+label:checkdupes)                                    | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:checkdupes) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:checkdupes)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:checkdupes)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:checkdupes))                             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:checkdupes)        | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:checkdupes)        | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:checkdupes)
| [close](https://github.com/simonmichael/hledger/issues?q=is:open+label:close)                                              | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:close) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:close)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:close)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:close))                                                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:close)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:close)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:close)
| [import](https://github.com/simonmichael/hledger/issues?q=is:open+label:import)                                            | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:import) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:import)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:import)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:import))                                             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:import)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:import)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:import)
| [incomestatement](https://github.com/simonmichael/hledger/issues?q=is:open+label:incomestatement)                          | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:incomestatement) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:incomestatement)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:incomestatement)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:incomestatement))         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:incomestatement)   | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:incomestatement)   | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:incomestatement)
| [prices](https://github.com/simonmichael/hledger/issues?q=is:open+label:prices)                                            | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:prices) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:prices)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:prices)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:prices))                                             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:prices)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:prices)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:prices)
| [print](https://github.com/simonmichael/hledger/issues?q=is:open+label:print)                                              | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:print) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:print)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:print)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:print))                                                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:print)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:print)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:print)
| [printunique](https://github.com/simonmichael/hledger/issues?q=is:open+label:printunique)                                  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:printunique) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:printunique)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:printunique)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:printunique))                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:printunique)       | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:printunique)       | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:printunique)
| [register](https://github.com/simonmichael/hledger/issues?q=is:open+label:register)                                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:register) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:register)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:register)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:register))                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:register)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:register)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:register)
| [registermatch](https://github.com/simonmichael/hledger/issues?q=is:open+label:registermatch)                              | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:registermatch) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:registermatch)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:registermatch)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:registermatch))                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:registermatch)     | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:registermatch)     | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:registermatch)
| [rewrite](https://github.com/simonmichael/hledger/issues?q=is:open+label:rewrite)                                          | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:rewrite) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:rewrite)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:rewrite)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:rewrite))                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:rewrite)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:rewrite)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:rewrite)
| [roi](https://github.com/simonmichael/hledger/issues?q=is:open+label:roi)                                                  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:roi) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:roi)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:roi)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:roi))                                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:roi)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:roi)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:roi)
| [stats](https://github.com/simonmichael/hledger/issues?q=is:open+label:stats)                                              | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:stats) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:stats)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:stats)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:stats))                                                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:stats)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:stats)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:stats)
| [tags](https://github.com/simonmichael/hledger/issues?q=is:open+label:tags)                                                | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:tags) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:tags)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:tags)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:tags))                                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:tags)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:tags)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:tags)
| **Miscellaneous:**
| [budget](https://github.com/simonmichael/hledger/issues?q=is:open+label:budget) (budgeting)                                | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:budget) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:budget)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:budget)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:budget))                                             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:budget)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:budget)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:budget)
| [packaging](https://github.com/simonmichael/hledger/issues?q=is:open+label:deps) (packaging, dependencies)                      | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:deps) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:deps)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:deps)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:deps))                                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:deps)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:deps)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:deps)
| [doc](https://github.com/simonmichael/hledger/issues?q=is:open+label:doc) (documentation, help)                            | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:doc) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:doc)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:doc)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:doc))                                                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:doc)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:doc)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:doc)
| [periodexpressions](https://github.com/simonmichael/hledger/issues?q=is:open+label:periodexpressions) (-b, -e, -p, date:)  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:periodexpressions) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:periodexpressions)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:periodexpressions)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:periodexpressions)) | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:periodexpressions) | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:periodexpressions) | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:periodexpressions)
| [site](https://github.com/simonmichael/hledger/issues?q=is:open+label:site) (website, web presence)                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:site) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:site)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:site)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:site))                                                     | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:site)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:site)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:site)
| [tools](https://github.com/simonmichael/hledger/issues?q=is:open+label:tools) (dev tools, infrastructure)                  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:tools) ([first](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:%22good+first+issue%22+label:tools)/[easy](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+label:easy?+label:tools)/[neither](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+-label:%22good+first+issue%22+-label:easy?+label:tools))                                                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:tools)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:tools)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:tools)


## About the project

### Mission

Why was hledger created ?

Mainly:

- to provide a more usable, robust, documented, cross-platform-installable version of Ledger for users
- to provide a more maintainable and hackable version of Ledger for developers 

Also:

- to provide a useful library and toolbox for finance-minded haskell programmers
- to explore the suitability of Haskell for such applications
- to experiment with building a successful time-and-money-solvent project in a thriving ecosystem of financial software projects

What is the hledger project's current mission ?

1. Provide peace of mind: bring clarity, relief, and peace of mind to folks stressed, confused, overwhelmed by finances.
2. Educate and empower: help individuals and communities achieve clarity, accountability and mastery with money and time.

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



## Getting started

New contributors are always welcome in the hledger project. 
Jump in! Or [ask us](index.html#helpfeedback) to help you find a task.

### Funder

Become a financial backer to
sustain and grow this project,
increase your influence,
express gratitude,
build prosperity consciousness,
and help transform world finance!

- Use the donate links on the [home page](https://hledger.org)
- Configure a recurring donation
- Contribute or pledge bounties on issues you care about
- Ask your organization to contribute
- Work on project sustainability, accountability, fundraising

### Tester

- Test installation on platforms you have access to
- Test examples, advice, and links in the docs
- Run the latest release or developer build in daily use
- Run [tests](#run-package-tests)
- Run [benchmarks](#run-package-benchmarks)
- Report packaging, documentation, UX, functional, performance issues
- Report and help analyse problems via irc/mail list/bug tracker

When reporting bugs, don't forget to search the tracker for a similar bug report.
Otherwise, open a new bug by clicking "New issue", or <http://bugs.hledger.org/new>.

Enhancement requests are sometimes added to the tracker,but for these consider using
the IRC channel and mail list (see [Getting help](index.html#getting-help)).
Both are archived and linkable, so the idea won't be lost.
There is also a collection of wishes at the old [trello board](http://trello.hledger.org).

### Technical Writer

- get familiar with the website and documentation online, review and test
- get familiar with the site/doc source files (see [Shake.hs](#shake))
- get the latest hledger source
- send patches with names prefixed with "doc: " (or "site: ")

### Graphics Designer

- more/better logos & graphics
- illustrations and diagrams
- web design mockups for home page, site, hledger-web UI

<!-- ### Product Designer -->
### Communicator

Marketing and market understanding is vital.

- clarify project goals, value proposition, brand, mission, story
- monitor product-market fit
- identify new opportunities
- influence developer priorities
- spread the word!

### Maintainer

#### Help with issue management

- watch tracker activity, report status
- apply/update labels where needed
- follow up on dormant issues
- facilitate a consistently good bug-reporting & PR-contributing experience

#### Help with packaging

- package hledger for linux distros, macports, etc.
- develop mac/windows installers
- find and assist distro packagers/installer developers

#### Help with project management

- clarify/update goals and principles
- monitor, report on project progress and performance
- research, compare and report on successful projects, related projects
- identify collaboration opportunities
- marketing, communication, outreach
- release management, roadmap planning

### Developer

See [Developer workflows](#developer-workflows).





## Make

A Makefile is provided to make common developer tasks easy to remember,
and to insulate us a little from the ever-evolving Haskell tools ecosystem.
Using it is entirely optional, but recommended.
You'll need [GNU Make](http://www.gnu.org/software/make) installed.

The Makefile contains a fair amount of obsolete cruft and needs cleanup. Some tasks (docs, website) are now handled by the [Shake](#shake) file instead.

The Makefile is self-documenting. Run `make` to see a list of the main make rules:

```shell
$ make
Makefile:37: -------------------- hledger make rules --------------------
Makefile:39: make [help] -- list documented rules in this makefile. make -n RULE shows more detail.
Makefile:204: (INSTALLING:)
Makefile:206: make install -- download dependencies and install hledger executables to ~/.local/bin or equivalent (with stack)
Makefile:231: (BUILDING:)
Makefile:235: make build -- download dependencies and build hledger executables (with stack)
Makefile:304: make hledgerdev -- quickly build the hledger executable (with ghc and -DDEVELOPMENT)
...
```

To see what a make rule will do without actually doing it, use the `-n` flag:

```shell
$ make build -n
stack build
```
```shell
$ make test -n
(stack test \
		&& echo pkgtest PASSED) || echo pkgtest FAILED
(stack exec hledger test \
		&& echo builtintest PASSED) || echo builtintest FAILED
(COLUMNS=80 PATH=`pwd`/bin:/home/simon/src/hledger/bin:/home/simon/.local/bin:/home/simon/.cabal/bin:/opt/ghc/7.10.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/var/lib/gems/1.9.1/bin stack exec -- shelltest --execdir -- -j16 --hide-successes tests \
		&& echo functest PASSED) || echo functest FAILED
```


## Shake

`Shake.hs` in the top directory complements the Makefile; it is used for some more complex tasks, such as building documentation and the web site.

Compile it: 

    ./Shake.hs   # or, make Shake

See help:

    ./Shake


## Code

hledger is a suite of applications, tools and libraries.
The main hledger code repository is [github.com/simonmichael/hledger](http://github.com/simonmichael/hledger)
(shortcut url `code.hledger.org`).
There are also various hledger addons maintained as separate projects with their own repos.

### hledger packages

Within the main repo, there are a number of separate cabal packages,
making it easier to pick and choose parts of hledger to install or to package.
They are:

#### hledger-lib

[package](http://hackage.haskell.org/package/hledger-lib),
[code](https://github.com/simonmichael/hledger/tree/master/hledger-lib)

Core data models, parsing, standard reports, and utilities.
Most data types are defined in [Hledger.Data.Types](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html),
while functions that operate on them are defined in Hledger.Data.TYPENAME.
Under [Hledger.Read](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read.hs)
are parsers for the supported input formats.
Data files are parsed into a
[Journal](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Journal),
which contains a list of
[Transactions](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Transaction),
each containing multiple
[Postings](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Posting)
of some
[MixedAmount](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:MixedAmount)
(multiple
single-[CommoditySymbol](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:CommoditySymbol)
[Amounts](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Amount))
to some
[AccountName](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:AccountName).
When needed, the Journal is further processed to derive a
[Ledger](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Ledger),
which contains summed 
[Accounts](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Account).
In [Hledger.Reports](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Reports.html)
there are standard reports, which extract useful data from the Journal or Ledger.

Here's a diagram of the main data model:

<a href="https://hledger.org/images/data-model.png" class="highslide" onclick="return hs.expand(this)">
<img src="https://hledger.org/images/data-model.png" alt="diagram" title="main data types" style="max-width:100%;">
</a>

<!-- generated by plantuml from:
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
Journal *-- "*" MarketPrice
Journal *-- "*" Transaction
MarketPrice -- Date
MarketPrice -- Amount
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
Amount -- CommoditySymbol
Amount -- Quantity
Amount -- AmountPrice
Amount -- AmountStyle
</uml>
-->

#### hledger

[package](http://hackage.haskell.org/package/hledger),
[code](https://github.com/simonmichael/hledger/tree/master/hledger),
[manual](https://hledger.org/hledger.html)

hledger's command line interface, and command line options and utilities for other hledger tools.

Try tracing the execution of a hledger command:

1. [Hledger.Cli.Main:main](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Main.hs#L302)
parses the command line to select a command, then
2. gives it to
[Hledger.Cli.Utils:withJournalDo](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Utils.hs#L73),
which runs it after doing all the initial parsing.
3. Parsing code is under
[hledger-lib:Hledger.Read](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read.hs),
eg [Hledger.Read.JournalReader](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read/JournalReader.hs).
4. Commands extract useful information from the parsed data model using
[hledger-lib:Hledger.Reports](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Reports),
and
5. render in plain text for console output (or another output format, like CSV).
6. Everything uses the data types and utilities from
[hledger-lib:Hledger.Data](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Data)
and [hledger-lib:Hledger.Utils](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Utils.hs).

#### hledger-ui

[package](http://hackage.haskell.org/package/hledger-ui),
[code](https://github.com/simonmichael/hledger/tree/master/hledger-ui),
[manual](https://hledger.org/hledger-ui.html)

A terminal interface.

#### hledger-web

[package](http://hackage.haskell.org/package/hledger-web),
[code](https://github.com/simonmichael/hledger/tree/master/hledger-web),
[manual](https://hledger.org/hledger-web.html)

A web interface.
hledger-web starts a web server built with the yesod framework,
and (by default) opens a web browser view on it.
It reads the journal file(s) at startup and again whenever they change.
It can also write (append) new transactions to the journal file.

There are two main views, which can be filtered with
[queries](https://hledger.org/hledger.html#queries):

- [/journal](http://demo.hledger.org/journal), showing general journal entries (like `hledger print`)

- [/register](http://demo.hledger.org/register?q=inacct:Expenses:Food),
  showing transactions affecting an account (slightly different from
  hledger's [register](https://hledger.org/hledger.html#register) command, which shows postings).

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

Handler module and function names end with R, like the yesod-generated route type they deal with.

Dynamically generated page content is mostly inline hamlet.
Lucius/Julius files and widgets generally are not used, except for the default layout.

Here are some ways to run it during development:

- `yesod devel`: runs in developer mode, rebuilds automatically when config, template, static or haskell files change
(but only files in the hledger-web package):
```shell
$ (cd hledger-web; yesod devel)
```

- [yesod-fast-devel](https://hackage.haskell.org/package/yesod-fast-devel)
  may be a good alternative, also reloads the browser page

- `stack ghci`: runs the server in developer mode from GHCI.
Changes to static files like hledger.js will be visible on page reload;
to see other changes, restart it as shown.
```shell
$ (cd hledger-web; stack ghci hledger-web)
hledger-web> :main --serve   # restart: ctrl-c, :r, enter, ctrl-p, ctrl-p, enter
```

- `make ghci-web`: runs the server in developer mode from GHCI, also
interprets the hledger-lib and hledger packages so that :reload picks
up changes in those packages too:
```shell
$ make ghci-web
ghci> :main --serve
```
(This rule also creates symbolic links to hledger-web's `config`, `messages`, `static` and `templates`
directories, needed in developer mode, so it can run from the top directory. This may not work on Windows.)

### Quality

Relevant tools include:

- unit tests (HUnit, make unittest)
- functional tests (shelltestrunner, make functest)
- performance tests (simplebench, make bench)
- documentation tests (make haddocktest + manual)
- ui tests (manual)
- installation tests (manual)
- code reviews

### Code review

- Code review party 2014/7/21-25:
  [discussion](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1070)<!-- missing ,
  [log](https://hledger.org/static/irc-20140725-code-review.html) -->
- Dev sprint/party 2015/10/10:
  [discussion](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1254)<!-- ircbrowse down ,
  [pre-chat](http://ircbrowse.net/day/hledger/2015/10/10),
  [log](http://ircbrowse.net/day/hledger/2015/10/11) -->


## Pull requests

Most contributed hledger code (and some of the project maintainer's code)
is submitted and reviewed via Github pull requests.
Here are some tips for contributing PRs to hledger.

### Code review is important

We aim to improve and sustain hledger's quality and maintainability over the long term.

Many PRs, especially small ones, and even some big ones, can be merged quickly. 
We love merging good PRs quickly.

Some bigger or more risky PRs can require substantial review, discussion, changes, or re-submission. 
Sometimes this is a bigger task than the coding.
Much valuable design, quality control, and knowledge sharing happens at this time. 
Some PRs get rejected, but their discussion and exploration can still be a useful contribution.
We very much want to avoid wasted work, but it occasionally happens. 
Our process is evolving and imperfect.
All of this is normal.

We hope you'll see it as a golden opportunity to collaborate with experts,
share and receive knowledge, refine your design/documentation/code,
and practice real-world development and communication skills.
Patience and persistence pays.

### The pull request

A PR should have a clear purpose, documented in its description. Mention any #ISSUENOs addressed.

Don't tackle too much at once. 
Smaller/more focussed PRs can be reviewed quicker and accepted (or rejected) quicker.

Consider showing a draft of documentation first (more on this below).

### The commit(s)

Commits should be easy to review.
Ideally each commit is complete, and has a single clear purpose,
which should be documented in the summary (and long description, if needed).
\#ISSUENOs can be mentioned in summary/description too when appropriate.

Within the above constraint, fewer, larger commits are preferred.

Keep in mind that commit messages are valuable documentation 
for future developers and troubleshooters. 
They are also the starting point for package changelogs and hledger release notes.
High-quality commit messages makes the release process quicker, and the resulting docs better. 

User-impacting commits should mention the user-visible changes, 
and be described in user-relevant language.
Library-user-impacting commits, eg API changes, ideally will also
be called out, and can described in more technical language.
Commits affecting hledger internals are less important, 
but you may notice some adhoc conventions if you browse the history.
In particular, you can optionally prefix the summary with short component codes (cf [Issues](#issues))
to facilitate history reading and changelog/release note production.

Rewrite and force-push your commits freely (rebase -i, push -f) to clean them up. 
Unless we decide to squash the PR into one commit, 
your commits will become part of hledger's history "for all time", 
so think about future developers trying to understand them, git bisect, etc.   

Rebase your commits against latest master for easiest review. Especially if they start to conflict.

We like to use some conventions in commit messages when it makes sense. These aren't mandatory, but appreciated:

- prepend a  [component](#components) prefix, eg `cli: ` or `journal: `, for clarity and to help with changelog production
- prepend a semicolon (`;`) to indicate commits that need not be mentioned in changelogs/release notes (as in the Emacs project)
- append a final `[ci skip]` line to indicate commits that need not trigger a CI build, to reduce carbon emissions from Travis.

### The docs

PRs should include appropriate updates to reference documentation, unless otherwise agreed.
Typically this means the manual source files (hledger*/hledger*.m4.md).
It can also involve
command line option names and descriptions,
other --help output,
hledger's commands list,
hledger-ui's help dialog,
hledger-web's help dialog,
etc.
Sometimes it means the developer docs, at least the ones in the main repo (READMEs).

Reviewers can understand your PR more efficiently once proposed doc changes are provided, and may postpone it otherwise.
We are happy to help with the docs if needed - just ask.

Updating rendered manuals (hledger.{1,info,txt,md,html}) is not required, and probably best avoided to reduce conflicts.
Updating other docs such as tutorials, how-tos, examples, or screenshots is not required,
though it's welcome (may be in a different repo).

### Documentation first

hledger follows documentation-driven design.
It is in fact highly effective, and highly recommended,
to write the new docs (help text/user manual/haddocks/developer README..) before writing any code.
You can share a rough draft on IRC, on the mail list, in an issue comment,
or in a "WIP" PR starting with just the proposed docs commit.

This is often the quickest road to getting something merged into hledger.
hledger's many parts interact in surprisingly complex ways.
The documentation-driven working style lets us discuss, clarify and reach a good-enough consensus economically,
after which coding/review/acceptance can go quicker.
<!--
changes can impact past and future users,
ease of contribution,
long-term maintenance costs,
product architecture,
compatibility with the larger plain text accounting ecosystem,
etc.
-->

### Related ideas

http://neilmitchell.blogspot.com/2019/06/the-one-pr-per-day-rule.html


## Tests

About testing in the hledger project, as of 201809.

### Kinds of tests

<div style="margin:1em 2em; font-style:italic;">
"Here, then, is a list of properties of tests. Not all tests need to exhibit all properties. However, no property should be given up without receiving a property of greater value in return.

- Isolated — tests should return the same results regardless of the order in which they are run.
- Composable — if tests are isolated, then I can run 1 or 10 or 100 or 1,000,000 and get the same results.
- Fast — tests should run quickly.
- Inspiring — passing the tests should inspire confidence
- Writable — tests should be cheap to write relative to the cost of the code being tested.
- Readable — tests should be comprehensible for reader, invoking the motivation for writing this particular test.
- Behavioral — tests should be sensitive to changes in the behavior of the code under test. If the behavior changes, the test result should change.
- Structure-insensitive — tests should not change their result if the structure of the code changes.
- Automated — tests should run without human intervention.
- Specific — if a test fails, the cause of the failure should be obvious.
- Deterministic — if nothing changes, the test result shouldn’t change.
- Predictive — if the tests all pass, then the code under test should be suitable for production."
--[Kent Beck](https://medium.com/@kentbeck_7670/test-desiderata-94150638a4b3)
</div>

1.  Unit tests

    Unit tests exercise small chunks of functionality. In hledger, that
    means a function. So, many of our functions have one or more unit
    tests. These are mostly in hledger-lib, with a few in hledger.

    Our unit tests use the
    [tasty](http://hackage.haskell.org/package/tasty) test runner,
    [tasty-hunit](http://hackage.haskell.org/package/tasty-hunit) HUnit-style tests,
    and some helpers from
    [Hledger.Utils.Test](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Utils/Test.hs),
    such as:
    
    - `tests` and `test` aliases for `testGroup` and `testCase`
    - `assert*` helpers for constructing various kinds of assertions

    We would like our unit tests to be:

    -   easy to read (clear, concise)
    -   easy to write (low boilerplate, low cognitive load)
    -   easy to maintain (easy to edit, easy to refactor, robust)
    -   easy to associate with the code under test (easy to view/jump
        between code & test, easy to estimate coverage)
    -   and scalable (usable for all devs, easy to run and select,
        suitable for small/large modules/packages).

    Here\'s the current pattern (let us know if you see a better way):

    ``` haskell
    module Foo (
      ...
      tests_Foo -- export this module's and submodules' tests
    )
    where
    import Hledger  -- provides Hledger.Utils.Test helpers
    import Bar      -- submodules, providing tests_Bar etc.
    import Baz

    functionA = ...
    functionB = ...
    functionC = ...
    functionD = ...

    tests_Foo = tests "Foo" [ -- define tests at the end of each module

       -- a group of several named tests for functionA
       tests "functionA" [
         test "a basic test"           $ assertBool "" SOMEBOOL
        ,test "a pretty equality test" $ SOMEEXPR @?= EXPECTEDVALUE
        ,test "a pretty parsing test"  $ assertParseEq PARSER INPUT EXPECTEDRESULT
        ,test "a multiple assertions test" $ do
          A @?= B
          doSomeIO
          C @?= D
        ]

       -- a single test containing multiple unnamed assertions for functionB
      ,test "functionB" $ do
         assertBool "" BOOL
         EXPR @?= VALUE

      ,tests_Foo            -- aggregate submodule tests
      ,tests_Bar
      ]
    ```

    Here are
    [some](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Data/Posting.hs#L296)
    real-world
    [examples](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Read/JournalReader.hs#L579).

    The unit tests are shipped as part of the hledger executable, and
    can always be run via the [test](https://hledger.org/manual#test)
    command (`hledger test`).

    Here\'s the quick way to run unit tests while developing:\
    `make ghcid-test` or `make ghcid-test-Some.Module`.

2.  Doc tests

    Like unit tests, but defined inside functions\' haddock
    documentation, in the style of a GHCI transcript. These test
    functionality, provide usage examples in the API docs, and test
    those examples, all at once. They are a bit more finicky and slower
    than unit tests. See
    [doctest](http://hackage.haskell.org/package/doctest) for more.

    doctests [do not work on Mac with GHC
    8.4+](https://github.com/sol/doctest/issues/199), out of the box.
    See
    [ghc\#15105](https://ghc.haskell.org/trac/ghc/ticket/15105#comment:10)
    for current status and a workaround.

3.  Functional tests

    Functional tests test the overall functioning of the program. For
    hledger, that means running `hledger` with various inputs and
    options and checking for the expected output. This exercises
    functionality in the hledger and hledger-lib packages. We do this
    with
    [shelltestrunner](http://hackage.haskell.org/package/shelltestrunner).
    Tests are defined in files named `*.test` under
    [hledger/test/](https://github.com/simonmichael/hledger/tree/master/hledger/test),
    grouped by *component* (command or topic name).
    For more about these, see the README there.

4.  Code tests

    We have some tests aimed at testing eg code quality, generally
    defined as make rules, such as:

      --------------------- -------------------------------------------------------------------------------------
      `make haddocktest`    can haddock process all code docs without error
      `make buildtest`      does all code build warning free with the default GHC version & stackage snapshot
      `make buildtestall`   does the code build warning free with all supported GHC versions/stackage snapshots
      --------------------- -------------------------------------------------------------------------------------

    See below for examples.

5.  Package test suites

    Haskell tools like stack and cabal recognise test suites defined in
    a package\'s cabal file (or package.yaml file). These can be run via
    `stack test`, `cabal test` etc., and they are required to build and
    pass by services like Stackage. Here are the currently hledger
    package test suites:

      ------------- ------------ ---------------------------------------------------------------
      package       test suite   what it runs
      hledger-lib   doctests     doctests
      hledger-lib   easytests    unit tests
      hledger       test         builtin test command (hledger\'s + hledger-lib\'s unit tests)
      hledger-ui                 
      hledger-web                
      ------------- ------------ ---------------------------------------------------------------

### Coverage

This means how thoroughly the code is tested - both in breadth (are all
parts of the code tested at least a little ?) and in depth (are all
possible code paths, states, situations tested ?).

Our current test coverage can be summarised like so:

  ------------- ------ ----- ------------
  package       unit   doc   functional
  hledger-lib   X      X     X
  hledger       X            X
  hledger-ui                 
  hledger-web                
  ------------- ------ ----- ------------

There are ways to generate detailed coverage reports for haskell unit
tests, at least. It would be useful to set this up for hledger.

### How to run tests

Run unit tests:

``` example
$ make unittest
```

Run doctests:

``` example
$ make doctest
```

Run functional tests (and unit tests, now):

``` example
$ stack install shelltestrunner
$ make functest
```

Run the package tests (unit tests, maybe doctests, but not functional
tests) of all or selected packages.

``` example
$ stack test [PKG]
```

Run \"default tests: package plus functional tests\":

``` example
$ make test
```

Test generation of haddock docs:

``` example
$ make haddocktest
```

Thorough test for build issues with current GHC:

``` example
$ make buildtest
```

Thorough test for build issues with all supported GHC versions:

``` example
$ make buildtestall
```

Run built-in hledger/hledger-lib unit tests via hledger command:

``` example
$ hledger test  # test installed hledger
$ stack build hledger && stack exec -- hledger test  # test just-built hledger
$ hledger test --help
test [TESTPATTERN] [SEED]
  Run the unit tests built in to hledger-lib and hledger,
  printing results on stdout and exiting with success or failure.
  Tests are run in two batches: easytest-based and hunit-based tests.
  If any test fails or gives an error, the exit code will be non-zero.
  If a pattern argument (case sensitive) is provided, only easytests
  in that scope and only hunit tests whose name contains it are run.
  If a numeric second argument is provided, it will set the randomness
  seed for easytests.
```

Rebuild and rerun hledger/hledger-lib unit tests via ghcid:

``` example
$ make ghcid-test
```

Rebuild and rerun only some tests via ghcid (see hledger test --help):

``` example
$ make ghcid-test-TESTPATTERN
```

See all test-related make rules:

``` example
$ make help-test
```




## Benchmarks

Benchmarks are standard performance measurements,
which we define using `bench` declarations in cabal files.
There is [one in hledger.cabal](https://github.com/simonmichael/hledger/blob/master/hledger/hledger.cabal#L228),
with related code and data files in [hledger/bench/](https://github.com/simonmichael/hledger/tree/master/hledger/bench).

To run the standard hledger benchmark, use `stack bench hledger`.
This installs haskell dependencies (but not system dependencies) and rebuilds as needed,
then runs [hledger/bench/bench.hs](https://github.com/simonmichael/hledger/blob/master/hledger/bench/bench.hs),
which by default shows quick elapsed-time measurements for several operations on a standard data file:

```shell
$ stack bench hledger
NOTE: the bench command is functionally equivalent to 'build --bench'
...
hledger-0.27: benchmarks
Running 1 benchmarks...
Benchmark bench: RUNNING...
Benchmarking hledger in /Users/simon/src/hledger/hledger with timeit
read bench/10000x1000x10.journal        [1.57s]
print                                   [1.29s]
register                                [1.92s]
balance                                 [0.21s]
stats                                   [0.23s]
Total: 5.22s
Benchmark bench: FINISH
```

bench.hs has some other modes, which you can use by compiling and running it directly.
`--criterion` reports more detailed and dependable measurements, but takes longer:

```shell
$ cd hledger; stack exec -- ghc -ibench bench/bench && bench/bench --criterion
...
Linking bench/bench ...
Benchmarking hledger in /Users/simon/src/hledger/hledger with criterion
benchmarking read bench/10000x1000x10.journal
time                 1.414 s    (1.234 s .. 1.674 s)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 1.461 s    (1.422 s .. 1.497 s)
std dev              59.69 ms   (0.0 s .. 62.16 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking print
time                 1.323 s    (1.279 s .. 1.385 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.305 s    (1.285 s .. 1.316 s)
std dev              17.20 ms   (0.0 s .. 19.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking register
time                 1.995 s    (1.883 s .. 2.146 s)
                     0.999 R²   (0.998 R² .. NaN R²)
mean                 1.978 s    (1.951 s .. 1.995 s)
std dev              25.09 ms   (0.0 s .. 28.26 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking balance
time                 251.3 ms   (237.6 ms .. 272.4 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 260.4 ms   (254.3 ms .. 266.5 ms)
std dev              7.609 ms   (3.192 ms .. 9.638 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking stats
time                 325.5 ms   (299.1 ms .. 347.2 ms)
                     0.997 R²   (0.985 R² .. 1.000 R²)
mean                 329.2 ms   (321.5 ms .. 339.6 ms)
std dev              11.08 ms   (2.646 ms .. 14.82 ms)
variance introduced by outliers: 16% (moderately inflated)
```

`--simplebench` shows a table of elapsed-time measurements for the commands defined in [bench/default.bench](https://github.com/simonmichael/hledger/blob/master/hledger/bench/default.bench).
It can also show the results for multiple h/ledger executables side by side, if you tweak the bench.hs code.
Unlike the other modes, it does not link with the hledger code directly, but runs the "hledger" executable found in $PATH (so ensure that's the one you intend to test).

```shell
$ cd hledger; stack exec -- ghc -ibench bench/bench && bench/bench --simplebench
Benchmarking /Users/simon/.local/bin/hledger in /Users/simon/src/hledger/hledger with simplebench and shell
Using bench/default.bench
Running 4 tests 1 times with 1 executables at 2015-08-23 16:58:59.128112 UTC:
1: hledger -f bench/10000x1000x10.journal print	[3.27s]
1: hledger -f bench/10000x1000x10.journal register	[3.65s]
1: hledger -f bench/10000x1000x10.journal balance	[2.06s]
1: hledger -f bench/10000x1000x10.journal stats	[2.13s]

Summary (best iteration):

+-----------------------------------------++---------+
|                                         || hledger |
+=========================================++=========+
| -f bench/10000x1000x10.journal print    ||    3.27 |
| -f bench/10000x1000x10.journal register ||    3.65 |
| -f bench/10000x1000x10.journal balance  ||    2.06 |
| -f bench/10000x1000x10.journal stats    ||    2.13 |
+-----------------------------------------++---------+
```

bench's --simplebench mode is based on a standalone tool, [tools/simplebench.hs](https://github.com/simonmichael/hledger/blob/master/tools/simplebench.hs).
simplebench.hs is a generic benchmarker of one or more executables (specified on the command line) against one or more sets of command-line arguments (specified in a file).
It has a better command-line interface than bench.hs, so you may find it more convenient
for comparing multiple hledger versions, or hledger and ledger. Eg:

```shell
$ stack exec -- ghc tools/simplebench
[1 of 1] Compiling Main             ( tools/simplebench.hs, tools/simplebench.o )
Linking tools/simplebench ...
```
```shell
$ tools/simplebench -h
tools/simplebench -h
simplebench: at least one executable needed
bench [-f testsfile] [-n iterations] [-p precision] executable1 [executable2 ...]

Run some functional tests with each of the specified executables,
where a test is "zero or more arguments supported by all executables",
and report the best execution times.

  -f testsfile   --testsfile=testsfile    file containing tests, one per line, default: bench.tests
  -n iterations  --iterations=iterations  number of test iterations to run, default: 2
  -p precision   --precision=precision    show times with this precision, default: 2
  -v             --verbose                show intermediate results
  -h             --help                   show this help

Tips:
- executables may have arguments if enclosed in quotes
- tests can be commented out with #
- results are saved in benchresults.{html,txt}
```
```shell
cd hledger; $ ../tools/simplebench -f bench/default.bench hledger ledger
Using bench/default.bench
Running 4 tests 2 times with 2 executables at 2015-08-24 04:24:37.257068 UTC:

Summary (best iteration):

+-----------------------------------------++---------+--------+
|                                         || hledger | ledger |
+=========================================++=========+========+
| -f bench/10000x1000x10.journal print    ||    3.24 |   0.43 |
| -f bench/10000x1000x10.journal register ||    3.80 |   3.48 |
| -f bench/10000x1000x10.journal balance  ||    2.05 |   0.18 |
| -f bench/10000x1000x10.journal stats    ||    2.10 |   0.19 |
+-----------------------------------------++---------+--------+
```

Finally, for quick, fine-grained performance measurements when troubleshooting or optimising, I use
[dev.hs](https://github.com/simonmichael/hledger/blob/master/dev.hs).



## Version numbers

Some places version numbers appear:

- --version (and sometimes --help) output of all hledger* executables
- web manuals on hledger.org
- download page
- changelogs
- release notes
- release announcements
- hackage/stackage uris
- cabal tarball filenames
- platform-specific packages

Some old version numbering goals:

1. automation, robustness, simplicity, platform independence
2. cabal versions must be all-numeric
3. release versions can be concise (without extra .0's)
4. releases should have a corresponding VCS tag
5. development builds should have a precise version appearing in --version
6. development builds should generate cabal packages with non-confusing versions
7. there should be a way to mark builds/releases as alpha or beta
8. avoid unnecessary compiling and linking
9. minimise VCS noise and syncing issues (commits, unrecorded changes)

Current version numbering policy:

- We (should) follow <http://haskell.org/haskellwiki/Package_versioning_policy>

- The "full release version" is ma.jor.minor, where minor is 0 for a
  normal release or 1..n for bugfix releases. Each component is a
  natural number (can be >= 10). Eg: 1.13 major release, 1.13.1
  bugfix release.

- The "release version", which we prefer to use when possible, is
  just ma.jor when minor is 0. Ie elide the dot zero.

- The build version is ma.jor.minor+patches, where patches is the number
  of patches applied in the current repo since the last release tag.

- `hledger --version` shows the release version or build version as
  appropriate.

- Release tags in the VCS are like PKG-VERSION. Eg hledger-1.13,
- hledger-ui-1.13.1.

Current process:

- In each hledger package directory there's a `.version` file
  containing its desired version number.
  
- After changing a `.version` file: run `./Shake setversion` to
  propagate the versions to all other places in the packages where
  they should appear. This is not perfect (see Shake.hs) so review and
  manually adjust the proposed changes before committing.  Those
  places include (you can also run these rules individually):

  - `PKG/package.yaml` contains the cabal package version declaration,
    bounds on other hledger packages, and a CPP VERSION macro used in
    `hledger/Hledger/Cli/Version.hs`. Changes in package.yaml will be
    propagated to `PKG/PKG.cabal` on the next stack or Shake build, or
    by `make gencabal`.

  - `PKG/.version.m4` contains the _version_ macro used in  documentation source files (*.m4.md). It is updated by `./Shake setversion`.

  - `PKG/.date.m4` contains the _monthyear_ macro used in  man pages. It is updated by `./Shake manuals`.

- At release time:

  - `./Shake PKG/CHANGES.md-finalise` converts the topmost heading, if
    it is an interim heading (just a commit hash), to a permanent
    heading containing the version and today's date.

  - for each package being released, a PKG-VERSION git tag is created.

- At major release time:

  - A new snapshot of the reference docs is added to the website, by
    `./Shake site/doc/VERSION/.snapshot`, and added to the links in
    `site/js/site.js`.

## Sample journals

Synthetic data files like `examples/100x100x10.journal` are useful for benchmarks and testing.
The numbers describe the number of transactions, number of accounts, and maximum account depth respectively.
They are generated by [`tools/generatejournal.hs`](https://github.com/simonmichael/hledger/blob/master/tools/generatejournal.hs).
They should get built automatically as needed, if not you can use `make samplejournals`:

```shell
$ make samplejournals
ghc tools/generatejournal.hs
[1 of 1] Compiling Main             ( tools/generatejournal.hs, tools/generatejournal.o )
Linking tools/generatejournal ...
tools/generatejournal 100 100 10 >examples/100x100x10.journal
tools/generatejournal 1000 1000 10 >examples/1000x1000x10.journal
tools/generatejournal 1000 10000 10 >examples/1000x10000x10.journal
tools/generatejournal 10000 1000 10 >examples/10000x1000x10.journal
tools/generatejournal 10000 10000 10 >examples/10000x10000x10.journal
tools/generatejournal 100000 1000 10 >examples/100000x1000x10.journal
tools/generatejournal 3 5 5 >examples/ascii.journal
tools/generatejournal 3 5 5 --chinese >examples/chinese.journal
tools/generatejournal 3 5 5 --mixed >examples/mixed.journal
```


## Docs

### Four kinds of documentation

20191209: needs update. See also doc/README.


<div style="margin:1em 2em; font-style:italic;">
"There is a secret that needs to be understood in order to write good
software documentation: there isn’t one thing called documentation,
there are four. They are: tutorials, how-to guides, explanation and
technical reference. They represent four different purposes or
functions, and require four different approaches to their creation."
--[Daniele Procida] (https://news.ycombinator.com/item?id=21289832)
</div>

https://github.com/simonmichael/hledger/tree/master/doc

Project documentation lives in a number of places:

- `site/*.md` is the hledger.org website content, which is generated with hakyll[-std] and pandoc
- haddock documentation in the code appears on Hackage
- short blurbs: cabal files, module headers, HCAR, GSOC project, ..
- `doc/notes.org` has some old developer notes
- developer reports (profiles, benchmarks, coverage..) in doc/profs, sometimes published at hledger.org/profs

How to prepare changelogs & release notes

Changelogs:

- ./Shake changelogs
- edit the new changelog items (identify, filter, move to correct changelog, deduplicate, rewrite, sort/group)

Release notes:

- add a new TOC entry and section in site/release-notes.md
- copy/rewrite/summarise package changelogs 
- note any other items of interest
- list release contributors
- write release summary


## Issues

The hledger project\'s issue tracker is on github. It contains:

-   BUG issues - failures in some part of the hledger project (the main
    hledger packages, docs, website..)
-   WISH issues - feature proposals, enhancement requests
-   uncategorised issues - we don\'t know what these are yet
-   pull requests - proposed changes to code and docs

### Issue Urls

-   <http://bugs.hledger.org> - show open BUG issues
-   <http://wishes.hledger.org> - show open WISH issues
-   <http://issues.hledger.org> - show all issues, open or closed
-   <http://prs.hledger.org> - show open pull requests
-   <http://bugs.hledger.org/new> - create a new issue

### Labels

Labels are used to categorise:

-   the issue\'s type: \"A BUG\" or \"A WISH\", in shades of red (The A
    makes it appear as first label)
-   relevant subsystems/topics, in light blue. More about this below.
-   relevant platforms, in light purple
-   resolution if not fixed:
    \"closed:cant-reproduce/duplicate/invalid/wont-fix\", in dark grey
-   \"bounty\", in bright yellow: issues with bountysource funding
-   \"easy?\", in dim yellow: issues which are probably relatively easy
    to fix
-   \"imported\" etc., in white: miscellaneous information

### Components

Issues and the hledger project generally are organised into components:
mostly non-overlapping topics, one for each user command, add-on tool,
input format, output format, etc. Each component gets a light blue label
in the issue tracker. Component names (sometimes abbreviated) are used
as a prefix to commit messages, and to organise changelogs and release
notes. The current components, and their open issues, can be seen in
the [Open Issues](#open-issues) table above.

### Custodians

If you are interested in helping with a particular component for a
while, please add yourself as a custodian in Open Issues table above.
A custodian\'s job is to help manage the issues, rally the troops, and
drive the open issue count towards zero. The more custodians, the
better! By dividing up the work this way, we can scale and make forward
progress.

### Milestones and Projects

Milestones are used a little bit to plan releases. In 2017 we
experimented with projects, but in 2018 milestones are in favour again..

### Estimates

You might see some experiments in estimate tracking, where some issue
names might have a suffix noting estimated and spent time. Basic format:
\[ESTIMATEDTOTALTASKTIME\|TIMESPENTSOFAR\]. Examples: \`\`\` \[2\] two
hours estimated, no time spent \[..\] half an hour estimated (a dot is
\~a quarter hour, as in timedot format) \[1d\] one day estimated (a day
is \~4 hours) \[1w\] one week estimated (a week is \~5 days or \~20
hours) \[3\|2\] three hours estimated, about two hours spent so far
\[1\|1w\|2d\] first estimate one hour, second estimate one week, about
two days spent so far \`\`\` Estimates are always for the total time
cost (not time remaining). Estimates are not usually changed, a new
estimate is added instead. Numbers are very approximate, but better than
nothing.

### Trello

The [trello board](http://trello.hledger.org) (trello.hledger.org) is an
old collection of wishlist items. This should probably be considered
deprecated.

## Funding

My vision for the hledger project has always been for it to be "accountable" and "self-sustaining", possibly through new forms of incentivisation. 
Classic non-monetary FOSS communities are a beautiful and precious thing.
Adding money can change their dynamic.
Yet, we would enjoy having a lot more issues resolved, and a faster rate of progress.
So we experiment, gently.

Currently we use bounties as a way to encourage resolution of issues.
There are a few ways to do this:

1. You or your organisation can offer a bounty simply by saying so on the issue.

2. You can use Bountysource. A few hledger bounties have been completed there.

3. You can use the new Open Collective process below.

Issues with bounties of any kind are marked with the `bounty` label.
The Bounty Manager is @simonmichael.

### New bounty process

It currently looks like this, and will evolve:

- Issues are marked as bounties by @simonmichael. Feel free to suggest additional issues which should receive the bounty label.

- Bounties are paid from the hledger project's public Open Collective fund.
  By contributing to the fund as an individual or organisation, you enable more bounties.

- These OC bounties (unlike 1 and 2 above) have standard amounts.
  These may be adjusted over time, depending eg on the state of our funds.
  Our current bounty amounts are
  - level 1: 10 USD
  - level 2: 25 USD
  - level 3: 50 USD

- When you complete a bounty, submit an expense to Open Collective,
  for whichever of the above bounty amounts you think appropriate,
  based eg on time or expertise spent, how much you need it,
  how much remains in our fund for other bounties, etc.
  This will be reviewed by OC and (maybe ?) @simonmichael.
  Successful claims, like donations, will appear in our public OC ledger.

Our bounty amounts are small, and nothing like professional rates in most countries,
but they still establish a principle of sustainability,
and help us to experiment.
You are encouraged to claim your bounties,
though you can also choose to transfer them to a new issue of your choice.

## Developer workflows

### Get developer tools

Ensure [`stack`](https://haskell-lang.org/get-started) is installed
(or if you’re a [cabal](https://www.haskell.org/cabal/) expert, feel free to use that.)

Ensure [`git`](http://git-scm.com) is installed. On Windows, it comes with stack.

Here are some useful optional tools:

- [GNU Make](http://www.gnu.org/software/make): to use the convenient [Make rules](#make).
- [`entr`](http://www.entrproject.org/) runs arbitrary commands when files change.
- [`ghcid`](http://hackage.haskell.org/package/ghcid) gives real-time GHC feedback as you make code changes.
- [`shelltestrunner`](http://hackage.haskell.org/package/shelltestrunner) runs hledger's functional tests.
- [`quickbench`](http://hackage.haskell.org/package/quickbench) measures and reports time taken by commands.
- [`hasktags`](http://hackage.haskell.org/package/hasktags) generates tag files for quick code navigation in editors like Emacs and vim.
- For browsing and editing Haskell code, popular tools include: Emacs, Vim, IDEA, VS Code, Atom..

Eg:

    stack install ghcid shelltestrunner quickbench hasktags
    brew install entr

### Get the code

    git clone https://github.com/simonmichael/hledger
    cd hledger

### Review code

- review and discuss new [pull requests](http://prs.hledger.org) and commits on github
- build hledger and test the latest changes in your own repo
- read the existing [code docs and source](#quick-links)
- send feedback or discuss via [IRC or mail list](index.html#helpfeedback)

### Build in place

See also https://hledger.org/download.html#c.-build-the-development-version .

    stack build    # hledger hledger-ui ...

This fetches the required GHC version and haskell dependencies from the default stackage snapshot (configured in `stack.yaml`), 
then builds all hledger packages.
This can take a while! To save time, you can build individual packages, eg just the CLI and TUI.

Note stack does not fetch C libraries such as curses or terminfo, which you might need to install yourself, using your system's package manager.
In case of trouble, see [download](/download.html#link-errors).

If you want to use an older snapshot/GHC for some reason, specify one of the older stack-*.yaml files:

    stack --stack-yaml stack8.2.yaml build
    
### Run in place

    stack exec -- hledger     # ARGS...
    stack exec -- hledger-ui  # ARGS...
    stack exec -- which hledger

### Build and install

This builds and also copies the hledger executables to `~/.local/bin` or the Windows equivalent
(which you should  [add to your `$PATH`](/download.html#b)).

    stack install    # hledger hledger-ui ...

### Run package tests

Runs any HUnit/doctest/easytest tests defined by each hledger package.

    stack test    # hledger ...

### Run package benchmarks

Runs any performance reports defined by each hledger package.

    stack bench    # hledger ...

### Run quickbench benchmarks

Times the end-user commands in `bench.sh` using quickbench.

    make bench

### Run functional tests

Runs the shelltestrunner tests defined in hledger/test/, which test the hledger CLI.

    make functest

### Run haddock tests

Checks for anything that would break haddock doc generation.

    make haddocktest

Checks for the unit-tests embedded in documentation.

    make doctest

### Simulate Travis tests

Locally runs tests similar to what we run on Travis CI.

    make travistest

### Test with all supported GHC versions/stackage snapshots

    make allsnapshotstest

### Use GHCI

GHCI is GHC's [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), 
useful for exploring and calling code interactively.

If you try to run GHCI (or things based on it, like ghcid)
right after cloning the hledger repo, you might see an error about CPP macros, eg like
[on #961](https://github.com/simonmichael/hledger/issues/961#issuecomment-459283412).
To fix this, build the hledger packages once, eg `stack build`.
 (Or `stack build hledger` might be enough, depending what you are doing.)

#### Get a GHCI prompt for hledger-lib:

    cd hledger-lib; stack ghci hledger-lib

Changing into the package directory isn't actually needed, but it
enables a custom .ghci script which sets a more useful short prompt.

#### Get a GHCI prompt for hledger:

    cd hledger; stack ghci hledger

#### Get a GHCI prompt for hledger-ui:

    cd hledger-ui; stack ghci hledger-ui

#### Get a GHCI prompt for hledger-web:

    cd hledger-web; stack ghci hledger-web

hledger-web also needs to find some things in its current directory (like the static/ directory).
This normally just works, if not please [send details](https://github.com/simonmichael/hledger/issues/274).

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

### Get your changes accepted

Follow the usual github workflow:

- fork the main hledger repo on github,
- git clone it to your local machine,
- git commit, after (?) pulling and merging the latest upstream changes
- git push back to github,
- open a pull request on github,
- follow up on any discussion there.

If you're new to this process, [help.github.com](http://help.github.com) may be useful.

### Add yourself to the contributor list

- after getting something into the master branch, read and sign the [contributor list & agreement](https://hledger.org/contributors.html). Or, [ask](index.html#help-feedback) to be added.
- give yourself a high five!

### Work on docs

Most docs tasks are handled by [Shake](#shake). 

#### List Shake rules:

    ./Shake

#### Generate man/info/txt manuals (in hledger*/) and embed in hledger executables:

    ./Shake manuals
    stack build

#### Generate html manuals and the hledger website (in site/_site/):

    ./Shake website

#### To remove all files generated by Shake:

    ./Shake Clean

### Use ghcid for watching GHC/GHCI

[ghcid](http://hackage.haskell.org/package/ghcid) is the most reliable and fastest way to see GHC's feedback, and optionally run tests or a GHCI command, as you edit. We run it via make, for convenience and to watch multiple packages rather than just one. Run `make help-ghcid` to list related rules.

#### Watch for compile errors in hledger-lib and hledger:

    make ghcid

#### Watch compile errors and the output of some hledger command:

    ghcid -c 'make ghci' -T ':main -f a.j bal --budget -N'

### Use --file-watch for watching stack

stack's --file-watch flag will re-run build/test/bench when source files or package.yaml/cabal files change. Eg:

    stack test hledger --file-watch

If you find that adding --fast makes this any faster, please update this.

### Use entr for watching arbitrary commands

[entr](http://entrproject.org/) is the most robust cross-platform tool for watching files and running a command when they change. Note its first argument must be an executable program, to run a shell command or multiple commands use `bash -c "..."`.

#### Rerun a single functional test as you change it:

    ls hledger/test/budget/budget.test | entr bash -c 'clear; COLUMNS=80 stack exec -- shelltest --execdir hledger/test/budget/budget.test -i12'
