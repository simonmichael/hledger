# hledger project README

## lightweight, portable, dependable accounting tools

hledger is a computer program for easily tracking money, time, or other commodities,
on unix, mac and windows (and web-capable mobile devices, to some extent).

It is first a command-line tool, but there are also terminal and
web interfaces, and a Haskell library for building your own
programs and scripts (hledger is written in Haskell).  hledger was
inspired by and is largely compatible with Ledger.  hledger is free
software available under the GNU General Public License v3+.

hledger aims to help both computer experts and regular folks
to gain clarity and control in their finances and time management,
but currently it is a bit more suited to techies.
I use it every day to:

-   track spending and income
-   see time reports by day/week/month/project
-   get accurate numbers for client billing and tax filing
-   track invoices

Though limited in features, hledger is lightweight, usable and reliable.
For some, it is a simpler, less distracting, more future-proof alternative to Quicken or GnuCash.

For more, see the home page: **<https://hledger.org>**

## Sponsorship

Your support, large or small, helps keep this project strong!\
See also **<https://hledger.org#sponsorship>**.

<!-- keep synced with index.md: -->

Sponsor Simon (project leader):
[![github](https://img.shields.io/badge/Sponsor_on-Github-limegreen "Sponsor the project leader via Github")](https://github.com/sponsors/simonmichael)
[![liberapay](https://img.shields.io/badge/Sponsor_on-Liberapay-limegreen "Sponsor the project leader via Liberapay")](https://liberapay.com/simonmichael)
[![paypal](https://www.paypal.com/en_US/i/btn/x-click-but04.gif "Give one time or recurringly via Paypal")](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5J33NLXYXCYAY)

Sponsor the hledger project as an organisation:
[![OpenCollective](https://opencollective.com/hledger/sponsors/badge.svg)][oc contributors] <!-- wrong count --> \
[![](https://opencollective.com/hledger/sponsor/0/avatar.svg)](https://opencollective.com/hledger/sponsor/0/website)
[![](https://opencollective.com/hledger/sponsor/1/avatar.svg)](https://opencollective.com/hledger/sponsor/1/website)
[![](https://opencollective.com/hledger/sponsor/2/avatar.svg)](https://opencollective.com/hledger/sponsor/2/website)
[![](https://opencollective.com/hledger/sponsor/3/avatar.svg)](https://opencollective.com/hledger/sponsor/3/website)
[![](https://opencollective.com/hledger/sponsor/4/avatar.svg)](https://opencollective.com/hledger/sponsor/4/website)
[![](https://opencollective.com/hledger/sponsor/5/avatar.svg)](https://opencollective.com/hledger/sponsor/5/website)
[![](https://opencollective.com/hledger/sponsor/6/avatar.svg)](https://opencollective.com/hledger/sponsor/6/website)
[![](https://opencollective.com/hledger/sponsor/7/avatar.svg)](https://opencollective.com/hledger/sponsor/7/website)
[![](https://opencollective.com/hledger/sponsor/8/avatar.svg)](https://opencollective.com/hledger/sponsor/8/website)
[![](https://opencollective.com/hledger/sponsor/9/avatar.svg)](https://opencollective.com/hledger/sponsor/9/website)

Sponsor the hledger project as an individual:
[![OpenCollective](https://opencollective.com/hledger/backers/badge.svg)][oc contributors] <!-- wrong count --> \
[![](https://opencollective.com/hledger/backer/0/avatar.svg)](https://opencollective.com/hledger/backer/0/website)
[![](https://opencollective.com/hledger/backer/1/avatar.svg)](https://opencollective.com/hledger/backer/1/website)
[![](https://opencollective.com/hledger/backer/2/avatar.svg)](https://opencollective.com/hledger/backer/2/website)
[![](https://opencollective.com/hledger/backer/3/avatar.svg)](https://opencollective.com/hledger/backer/3/website)
[![](https://opencollective.com/hledger/backer/4/avatar.svg)](https://opencollective.com/hledger/backer/4/website)
[![](https://opencollective.com/hledger/backer/5/avatar.svg)](https://opencollective.com/hledger/backer/5/website)
[![](https://opencollective.com/hledger/backer/6/avatar.svg)](https://opencollective.com/hledger/backer/6/website)
[![](https://opencollective.com/hledger/backer/7/avatar.svg)](https://opencollective.com/hledger/backer/7/website)
[![](https://opencollective.com/hledger/backer/8/avatar.svg)](https://opencollective.com/hledger/backer/8/website)
[![](https://opencollective.com/hledger/backer/9/avatar.svg)](https://opencollective.com/hledger/backer/9/website)
[![](https://opencollective.com/hledger/backer/10/avatar.svg)](https://opencollective.com/hledger/backer/10/website)
[![](https://opencollective.com/hledger/backer/11/avatar.svg)](https://opencollective.com/hledger/backer/11/website)
[![](https://opencollective.com/hledger/backer/12/avatar.svg)](https://opencollective.com/hledger/backer/12/website)
[![](https://opencollective.com/hledger/backer/13/avatar.svg)](https://opencollective.com/hledger/backer/13/website)
[![](https://opencollective.com/hledger/backer/14/avatar.svg)](https://opencollective.com/hledger/backer/14/website)
[![](https://opencollective.com/hledger/backer/15/avatar.svg)](https://opencollective.com/hledger/backer/15/website)
[![](https://opencollective.com/hledger/backer/16/avatar.svg)](https://opencollective.com/hledger/backer/16/website)
[![](https://opencollective.com/hledger/backer/17/avatar.svg)](https://opencollective.com/hledger/backer/17/website)
[![](https://opencollective.com/hledger/backer/18/avatar.svg)](https://opencollective.com/hledger/backer/18/website)
[![](https://opencollective.com/hledger/backer/19/avatar.svg)](https://opencollective.com/hledger/backer/19/website)
[![](https://opencollective.com/hledger/backer/20/avatar.svg)](https://opencollective.com/hledger/backer/20/website)

Bounties for specific tasks:
[![all bounties](https://img.shields.io/badge/github-All_bountied_issues-30bae8 "all bountied issues, bountysource and otherwise")](https://github.com/simonmichael/hledger/issues?q=label:bounty)
[![bountysource bounties](https://api.bountysource.com/badge/team?team_id=75979&style=bounties_received "issues bountied via bountysource")](https://www.bountysource.com/teams/hledger)


<br>
<br>
<br>

[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](http://www.gnu.org/licenses/gpl.html)
[![hledger CI](https://github.com/simonmichael/hledger/workflows/hledger%20CI/badge.svg)](https://github.com/simonmichael/hledger/actions)
[![on hackage](https://img.shields.io/hackage/v/hledger.svg?label=hackage&colorB=green)](http://hackage.haskell.org/package/hledger)
[![](https://img.shields.io/hackage-deps/v/hledger-lib.svg?label=hledger-lib+bounds)](http://packdeps.haskellers.com/feed?needle=hledger-lib)
[![](https://img.shields.io/hackage-deps/v/hledger.svg?label=hledger+bounds)](http://packdeps.haskellers.com/feed?needle=hledger)
[![](https://img.shields.io/hackage-deps/v/hledger-ui.svg?label=hledger-ui+bounds)](http://packdeps.haskellers.com/feed?needle=hledger-ui)
[![](https://img.shields.io/hackage-deps/v/hledger-web.svg?label=hledger-web+bounds)](http://packdeps.haskellers.com/feed?needle=hledger-web)
[![](https://repology.org/badge/version-for-repo/stackage_lts/hledger.svg)](https://repology.org/metapackage/hledger)
[![](https://repology.org/badge/version-for-repo/stackage_nighly/hledger.svg)](https://repology.org/metapackage/hledger)
[![github issues](https://img.shields.io/github/issues/simonmichael/hledger.svg)](http://bugs.hledger.org)


[oc contributors]: https://opencollective.com/hledger#section-contributors
