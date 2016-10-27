---
title:  The hledger project
author: Simon Michael
date:   November 2016
status: stable, actively developed
...

hledger is a set of cross-platform tools (and Haskell libraries) for
tracking money, time, or any other commodity using double-entry
accounting and a simple plain text file format.
hledger aims to be reliable and practical for daily use, and provides
command-line, curses-style and web interfaces.  It is a largely
compatible Haskell reimplementation of John Wiegley's Ledger program.
hledger is released under GNU GPLv3+.

In November 2015, the immediate plans were to
improve docs and help,
improve parser speed and memory efficiency, 
integrate a separate parser for Ledger files built by John Wiegley,
hledger-ui improvements,
and work towards the 1.0 release.

All but one of these goals have been achieved:

- docs have been reorganized, with more focussed manuals available in
  multiple versions, formats and as built-in help
  
- hledger has migrated from parsec to megaparsec and from String to Text,
  parsers have been simplified, memory usage is ~30% less on large files,
  speed is slightly improved all around
  
- the ledger4 parser is not yet integrated

- hledger-ui has acquired many new features making it more useful
  (file editing, filtering, historical/period modes, quick period browsing..)

- 1.0 has been released!

Also,

- hledger-web is more robust and more mobile-friendly
- hledger-api, a simple web API server, has been added
- a new "timedot" file format allows retroactive/approximate time logging
- we now support GHC 8 and GHC 7.10, dropping GHC 7.8 and 7.6 support.
  (GHC 7.8 support requires a maintainer).
- hpack is now used for maintaining cabal files
- our benchmarking tool has been spun off as the quickbench package
- the hledger.org website is simpler, clearer, and more mobile-friendly
- a call for help was sent out last month, and contributor activity
  has increased.

Future plans include:

- support the 1.0 release
- improve the website and docs
- grow the user & developer community
- clean up, automate, improve and scale our processes
- improve quality, reduce waste
- add the ledger4 parser
- add budget/goal-tracking features
- improve hledger-ui usability and features; live reloading

hledger is available from the hledger.org website, from Github,
Hackage, and Stackage, and is packaged for a number of systems
including Homebrew, Debian, Ubuntu, Gentoo, Fedora, and NixOS.

For more, see <http://hledger.org>.
