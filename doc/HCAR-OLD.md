## 2011/11/1 HCAR

hledger is a library and end-user tool (with command-line, curses and web
interfaces) for converting, recording, and analyzing financial
transactions, using a simple human-editable plain text file format. It is
a haskell port and friendly fork of John Wiegley's Ledger, licensed under
GNU GPLv3+.

hledger aims to be a reliable, practical tool for daily use. It reports
charts of accounts or account balances, filters transactions by type,
helps you record new transactions, converts CSV data from your bank,
publishes your text journal with a rich web interface, generates simple
charts, and provides an API for use in your own financial scripts and
apps.

In the last six months there have been two major releases. 0.15 focussed
on features and 0.16 focussed on quality. Changes include:

- new modal command-line interface, extensible with hledger-* executables in the path
- more useful web interface, with real account registers and basic charts
- hledger-web no longer needs to create support files, and uses latest yesod & warp
- more ledger compatibility
- misc command enhancements, API improvements, bug fixes, documentation updates
- lines of code increased by 3k to 8k
- project committers increased by 6 to 21

Current plans include:

- Continue the release rhythm of odd-numbered = features, even-numbered =
  quality/stability/polish, and releasing on the first of a month

- In 0.17, clean up the storage layer, allow rcs integration via
  filestore, and read (or convert) more formats

- Keep working towards wider usefulness, improving the web interface and
  providing standard financial reports

## 2011/05 HCAR

hledger is a haskell port and friendly fork of John Wiegley's ledger.  It
is a robust command-line accounting tool with a simple human-editable data
format. Given a plain text file describing transactions, of money or any
other commodity, hledger will print the chart of accounts, account
balances, or transactions you're interested in.  It can also help you
record transactions, or convert CSV data from your bank. There are also
curses and web interfaces. The project aims to provide a reliable,
practical day-to-day financial reporting tool, and also a useful library
for building financial apps in haskell.

Since hledger's last HCAR entry in 2009, hledger became cabalised, had 10
non-bugfix releases on hackage, split into multiple packages, acquired a
public mailing list, bug tracker, fairly comprehensive manual,
cross-platform binaries, and has grown to 5k lines of code and 15
committers. 0.14 has just been released, with 5 code committers.

The project is available under the GNU GPLv3 or later, at http://hledger.org .

Current plans are to continue development at a steady pace, to attract
more developers, and to become more useful to a wider range of users, eg
by building in more awareness of standard accounting procedures and by
improving the web and other interfaces.

## 2009/05 HCAR

hledger is a (primarily) command-line accounting tool similar to John
Wiegley's "ledger".  It reads a plain text journal file describing money
or commodity transactions, or timelog entries, and generates precise
activity and balance reports.

Since the last report, hledger has reached release 0.4 on Hackage. It has
60 test cases, new features such as basic curses and web-based interfaces,
and has had some performance tuning. It is now quite useful for day to day
reporting of money and time. Also, the project has a new web address
(hledger.org), and has attracted two new committers.

## 2008/11 HCAR

hledger is a command-line accounting tool similar to John Wiegley’s ledger tool.

The first release has been published on Hackage, and has attracted some
interest. It can be used for generating simple balance and transaction
reports from a plain-text general ledger. A home page and mail list has
also been created.

Immediate plans are to add some more of the most useful features from 
ledger, so that hledger can be used for day-to-day finances, and to grow
the community of contributors.

