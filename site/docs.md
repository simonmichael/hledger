# hledger docs

<style>
h2 { font-size:x-large; margin-top:0.5em; }
h3 { font-size:large; margin-bottom:0.2em; }
tr { border-top:thin solid #bbb; border-bottom:thin solid #bbb; vertical-align:top; }
td:nth-child(1) { padding-right:1em; white-space:nowrap; }
</style>

<div class="container">

hledger-specific docs are collected here. 
For more docs relevant to all ledger-likes, see [plaintextaccounting.org](http://plaintextaccounting.org).

## Help/feedback
|
|---------------|----------------------------------------------------------------------------|
| IRC           | [#hledger](http://irc.hledger.org) on Freenode (<!-- [chat log](http://ircbrowse.net/browse/hledger); --> see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) <!-- *Quick help and background chat.* --> <!-- *If you don't get an answer promptly, you can type `sm` to alert me, or leave the window open and check back later.* -->
| Twitter       | [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime) hashtag (see also [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a>) <!-- *Social!* -->
| Mail list     | [mail.hledger.org](http://mail.hledger.org) <!-- *Slightly less quick, more eyeballs.* -->
| Issue tracker | [bugs.hledger.org](http://bugs.hledger.org) (just the bugs), [issues.hledger.org](http://issues.hledger.org) (all issues). <!-- *Always check here.* --> <!-- *Bug reports are welcome.* -->
<!-- | hledger-web demo&nbsp;&nbsp; | [demo.hledger.org](http://demo.hledger.org) -->
<!-- | hledger-web on Sandstorm&nbsp;&nbsp; | [hledger-web app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90), [issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=label%3A%22platform%3A%20sandstorm%22%20) -->
<!-- | hledger-api demo        | <\!-- [demo.hledger.org/api](http://demo.hledger.org/api/swagger.json), -\-> [in swagger editor](http://editor.swagger.io/#/?import=demo.hledger.org/api/swagger.json&no-proxy) -->

<div class="row">

<div class="col-sm-3">

## Introduction

### What is hledger?

[hledger.org](http://hledger.org)\
[Misc. links](more-docs.html)\
<!-- Some learning resources not yet moved to the above. -->
[FAQ](faq.html)\


### What is plain text accounting?

[plaintextaccounting.org](http://plaintextaccounting.org)\


### Installing 

[Download](download.html)\
[Release notes](release-notes.html)\


### Getting started

[hledger Step by Step](step-by-step.html) tutorial\


</div>



<div class="col-sm-3">

## [Cookbook](cookbook.html){title="all on one page"}

### Journal management

[Start a journal](start-journal.html)\
[Track changes with version control](version-control.html)\


### Account names

[PTA: choosing accounts](http://plaintextaccounting.org/#choosing-accounts)\
[Rewrite account names](account-aliases.html)\
[Use another account separator character](account-separator.html)\


### Journal entries

[Example journal entries](entries.html)\


### Data entry/import

[Convert CSV files](csv-import.html)\


### Investing

[Track investments](investments.html)\


### More..
[PTA: Common tasks](http://plaintextaccounting.org/#common-tasks)

</div>



<div class="col-sm-3">

## [Reference](manual.html){title="all on one page"}

### File formats

[hledger_journal(5)](journal.html)
is hledger's native data format, representing an accounting journal

[hledger_csv(5)](csv.html)
allows CSV import/export

[hledger_timeclock(5)](timeclock.html)
is for clock-in/clock-out time logging

[hledger_timedot(5)](timedot.html)
is for human-friendly time logging

### Tools

[hledger(1)](hledger.html)
is the command-line UI

[hledger-ui(1)](hledger-ui.html)
is a curses-style UI

[hledger-web(1)](hledger-web.html)
is a web UI

[hledger-api(1)](hledger-api.html)
is a HTTP-JSON API

### Addon tools

Available [on Hackage/Stackage](/download.html#d):

[hledger-diff](http://hackage.haskell.org/package/hledger-diff)
shows differences in an account's transactions between one journal file and another.

[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd)
is a curses-style replacement for hledger's add command. 

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
generates interest transactions for an account according to various schemes. 

[hledger-irr](http://hackage.haskell.org/package/hledger-irr)
calculates the internal rate of return of an investment account.

### Experimental tools

Available [in the hledger repo](/download.html#d):

[hledger-autosync](https://github.com/simonmichael/hledger/blob/master/bin/hledger-autosync) 
is a symbolic link for running 
[ledger-autosync](https://pypi.python.org/pypi/ledger-autosync),
which can download or convert OFX data.

[hledger-budget.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-budget.hs#L10)
adds Ledger-style periodic transactions and budget reports.

[hledger-chart.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-chart.hs#L47)
is an old pie chart generator.

[hledger-check.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check.hs)
checks more powerful account balance assertions.

[hledger-check-dates.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-dates.hs#L15)
checks that journal entries are ordered by date.

[hledger-check-dupes.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-dupes.hs#L21)
checks for account names sharing the same leaf name.

[hledger-equity.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-equity.hs#L17)
prints balance-clearing/restoring transactions, useful for bringing account balances across file boundaries. 

[hledger-prices.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-prices.hs)
prints all prices from the journal.

[hledger-print-unique.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-print-unique.hs#L15)
prints transactions with unique descriptions.

[hledger-register-match.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-register-match.hs#L23)
helps ledger-autosync ignore previously imported transactions.

[hledger-rewrite.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-rewrite.hs#L28)
adds Ledger-style automatic postings to existing transactions.

[hledger-tags.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-tags.hs)
lists tag names in use.


</div> <!-- col -->

</div> <!-- row -->
</div> <!-- container -->

<!-- For more docs relevant to all ledger-likes, see also [plaintextaccounting.org](http://plaintextaccounting.org)  -->

