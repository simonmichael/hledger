# Developer docs

Contributor, developer, and maintainer docs.
These aim to describe and communicate the structure, processes and
workflows of the hledger project - "the machine that makes the machine".

These mostly are kept in doc/ in the main hledger repo,
and then symlinked into the hledger_site repo for rendering on hledger.org.

- [README](dev-README.md)
- [ROADMAP](ROADMAP.md)
<!-- - [TODO](TODO.md) -->
- [ACHIEVEMENTS](ACHIEVEMENTS.md)
- [BENCHMARKS](BENCHMARKS.md)
- [CHANGELOGS](CHANGELOGS.md)
- [CODE](CODE.md)
- [COMMITS](COMMITS.md)
- [CONTRIBUTING](CONTRIBUTING.md)
- [CREDITS](CREDITS.md)
- [DECISIONS](DECISIONS.md)
- [DEVWORKFLOWS](DEVWORKFLOWS.md)
- [DOCS](DOCS.md)
- [ERRORS](ERRORS.md)
- [EXAMPLES](EXAMPLES.md)
- [FILES](FILES.md)
- [FINANCE](FINANCE.md)
- [Investment Accounting Feature Ladder](investment-accounting-features.md)
- [ISSUES](ISSUES.md)
- [LINKS](LINKS.md)
- [MAKE](MAKE.md)
- [MOCKUPS](MOCKUPS.md)
- [PULLREQUESTS](PULLREQUESTS.md)
- [REGRESSIONS](REGRESSIONS.md)
- [RELEASING](RELEASING.md)
- [REPOS](REPOS.md)
- [SHAKE](SHAKE.md)
- [TESTS](TESTS.md)
- [VERSIONNUMBERS](VERSIONNUMBERS.md)

## Dev links

**Discussion:**
<https://hledger.org/support>\
\
**Github:**\
<http://code.hledger.org>,
<http://site.hledger.org>,
<http://finance.hledger.org>\
[commits](https://github.com/simonmichael/hledger/commits),
[COMMITS!](https://starlogs.net/#simonmichael/hledger)\
<http://ci.hledger.org>\
<http://bugs.hledger.org>,
<http://wishes.hledger.org>,
[unknown issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=is%3Aissue%20is%3Aopen%20-label%3A%22A%20BUG%22%20-label%3A%22A%20WISH%22%20),
<http://prs.hledger.org>,
<http://draftprs.hledger.org>,
<http://readyprs.hledger.org>,
[all issues](https://github.com/simonmichael/hledger/issues?q=)\
[issues with bounty tag](https://github.com/simonmichael/hledger/issues?q=label:bounty),
[bountysource bounties](https://github.com/simonmichael/hledger/issues?q=%22Add%20to%20the%20bounty%20at%20Bountysource%22%20OR%20%22claim%20the%20bounty%20on%20Bountysource%22%20OR%20%22bounty%20on%20this%20issue%20has%20been%20claimed%20at%20Bountysource%22%20),
[codemill bounties](https://github.com/simonmichael/hledger/issues?q=codemill),
[codefund bounties](https://github.com/simonmichael/hledger/issues?utf8=✓&q=codefund)\
[projects.hledger.org](http://projects.hledger.org)\
[stars.hledger.org](http://stars.hledger.org) our rank among starred haskell projects:\
2024:#36\
2023:#32\
2022:#34\
2020:#36\
2018:#53\
2017:#54\
2016:#71\
\
**Hackage:**\
packages:
[hledger-lib](https://hackage.haskell.org/package/hledger-lib),
[hledger](https://hackage.haskell.org/package/hledger),
[hledger-ui](https://hackage.haskell.org/package/hledger-ui),
[hledger-web](https://hackage.haskell.org/package/hledger-web),
[hledger-diff](https://hackage.haskell.org/package/hledger-diff),
[hledger-iadd](https://hackage.haskell.org/package/hledger-iadd),
[hledger-interest](https://hackage.haskell.org/package/hledger-interest),
[hledger-irr](https://hackage.haskell.org/package/hledger-irr),
[\*hledger\*](https://hackage.haskell.org/packages/search?terms=hledger)\
diffs:
[hledger-lib](https://hdiff.luite.com/cgit/hledger-lib/diff),
[hledger](https://hdiff.luite.com/cgit/hledger/diff),
[hledger-ui](https://hdiff.luite.com/cgit/hledger-ui/diff),
[hledger-web](https://hdiff.luite.com/cgit/hledger-web/diff)\
build status:
[hledger-lib](https://matrix.hackage.haskell.org/package/hledger-lib),
[hledger](https://matrix.hackage.haskell.org/package/hledger),
[hledger-ui](https://matrix.hackage.haskell.org/package/hledger-ui),
[hledger-web](https://matrix.hackage.haskell.org/package/hledger-web)\
reverse deps:
[hledger-lib](https://packdeps.haskellers.com/reverse/hledger-lib),
[hledger](https://packdeps.haskellers.com/reverse/hledger),
[hledger-ui](https://packdeps.haskellers.com/reverse/hledger-ui),
[hledger-web](https://packdeps.haskellers.com/reverse/hledger-web)\
[![on hackage](https://img.shields.io/hackage/v/hledger.svg?label=hackage&colorB=green)](https://hackage.haskell.org/package/hledger)\
[![...](https://img.shields.io/hackage-deps/v/hledger-lib.svg?label=hledger-lib+bounds)](https://packdeps.haskellers.com/feed?needle=hledger-lib)[![...](https://img.shields.io/hackage-deps/v/hledger.svg?label=hledger+bounds)](https://packdeps.haskellers.com/feed?needle=hledger)\
[![...](https://img.shields.io/hackage-deps/v/hledger-ui.svg?label=hledger-ui+bounds)](https://packdeps.haskellers.com/feed?needle=hledger-ui)[![...](https://img.shields.io/hackage-deps/v/hledger-web.svg?label=hledger-web+bounds)](https://packdeps.haskellers.com/feed?needle=hledger-web)\
\
**Stackage:**\
[build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml)\
[open hledger-related issues](https://github.com/fpco/stackage/search?q=hledger+is%3Aopen&type=Issues)\
packages:
[hledger-lib](https://www.stackage.org/package/hledger-lib),
[hledger](https://www.stackage.org/package/hledger),
[hledger-ui](https://www.stackage.org/package/hledger-ui),
[hledger-web](https://www.stackage.org/package/hledger-web)\
versions:
[hledger-lib](https://www.stackage.org/package/hledger-lib/snapshots),
[hledger](https://www.stackage.org/package/hledger/snapshots),
[hledger-ui](https://www.stackage.org/package/hledger-ui/snapshots),
[hledger-web](https://www.stackage.org/package/hledger-web/snapshots)\
[![](https://repology.org/badge/version-for-repo/stackage_lts/hledger.svg)](https://repology.org/metapackage/hledger)[![...](https://repology.org/badge/version-for-repo/stackage_nighly/hledger.svg)](https://repology.org/metapackage/hledger)\
\
**Repology:**\
[quick hledger packaging status](https://repology.org/metapackage/hledger/badges),
[detailed hledger packaging status](https://repology.org/project/hledger/versions),
[all \*hledger\* packages](https://repology.org/metapackages/?search=hledger)\
\
**Homebrew:**\
[hledger](https://formulae.brew.sh/formula/hledger)\
our 1-year rank among homebrew installs:\
2023: [#1821 of 24k](https://formulae.brew.sh/analytics/install-on-request/365d)(top8%)\
2020: [#1520 of 10k](https://formulae.brew.sh/analytics/install-on-request/365d)(top15%)\
\
**Debian:**\
source packages:
[haskell-hledger-lib](https://tracker.debian.org/pkg/haskell-hledger-lib),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-lib),
[haskell-hledger](https://tracker.debian.org/pkg/haskell-hledger),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger),
[haskell-hledger-ui](https://tracker.debian.org/pkg/haskell-hledger-ui),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-ui),
[haskell-hledger-web](https://tracker.debian.org/pkg/haskell-hledger-web),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-web)\
stable:
[hledger](https://packages.debian.org/stable/hledger),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=stable),
[hledger-ui](https://packages.debian.org/stable/hledger-ui),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=stable),
[hledger-web](https://packages.debian.org/stable/hledger-web),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=stable)\
testing:
[hledger](https://packages.debian.org/testing/hledger),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=testing),
[hledger-ui](https://packages.debian.org/testing/hledger-ui),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=testing),
[hledger-web](https://packages.debian.org/testing/hledger-web),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=testing)\
unstable:
[hledger](https://packages.debian.org/unstable/hledger),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=unstable),
[hledger-ui](https://packages.debian.org/unstable/hledger-ui),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=unstable),
[hledger-web](https://packages.debian.org/unstable/hledger-web),
[bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=unstable)\
all:
[\*hledger\*](https://packages.debian.org/search?searchon=names&keywords=hledger)\
popcon sampled install stats:
[haskell-hledger](https://qa.debian.org/popcon.php?packages=haskell-hledger),
[hledger](https://qa.debian.org/popcon.php?packages=hledger),
[hledger-ui](https://qa.debian.org/popcon-graph.php?packages=hledger-ui),
[hledger-web](https://qa.debian.org/popcon-graph.php?packages=hledger-web)\
\
**Ubuntu:**\
source packages:
[haskell-hledger-lib](https://launchpad.net/ubuntu/+source/haskell-hledger-lib),
[bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-lib),
[haskell-hledger](https://launchpad.net/ubuntu/+source/haskell-hledger),
[bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger),
[haskell-hledger-ui](https://launchpad.net/ubuntu/+source/haskell-hledger-ui),
[bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-ui),
[haskell-hledger-web](https://launchpad.net/ubuntu/+source/haskell-hledger-web),
[bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-web)\
binary packages:
[\*hledger\*](https://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger)\
\
**Gentoo:**
[hledger](https://gpo.zugaina.org/dev-haskell/hledger),
[hledger-web](https://gpo.zugaina.org/dev-haskell/hledger-web),
[\*hledger\*](https://gpo.zugaina.org/Search?search=hledger)\
\
**Fedora:**
[hledger](https://apps.fedoraproject.org/packages/hledger),
[\*hledger\*](https://apps.fedoraproject.org/packages/s/hledger),
[hledger (package db)](https://admin.fedoraproject.org/pkgdb/package/hledger/),
[Haskell SIG](https://fedoraproject.org/wiki/Haskell_SIG)\
\
**Void Linux:** [package search](https://voidlinux.org/packages/)->hledger\
\
**Nix:** [\*hledger\*](https://hydra.nixos.org/search?query=hledger)\
\
**Sandstorm:**
[hledger web app & reviews](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90),
[issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=label%3A%22platform%3A%20sandstorm%22%20)\
\
**Reference:** [fosskers GHC compatibility chart](https://www.fosskers.ca/en/blog/base)\
\
**Old trello planning board:** <https://trello.hledger.org>\
\
**hledger-web demo:** <https://demo.hledger.org>\


<!-- hledger GHCJS demo      <https://hledger.alhur.es> -->

<!--
hledger-api demo
[api-demo.hledger.org/api/v1/accounts](https://api-demo.hledger.org/api/v1/accounts),
[api-demo.hledger.org/swagger.json](https://api-demo.hledger.org/swagger.json),
[in swagger editor](https://editor2.swagger.io/#/?import=api-demo.hledger.org/swagger.json&no-proxy)\
[unfinished angular sample app](https://api-demo.hledger.org) ([code](https://github.com/simonmichael/hledger/tree/master/hledger-api/examples/angular))
-->

<!-- The Debian packages:
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
