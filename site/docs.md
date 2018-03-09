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
| IRC:           | [#hledger](http://irc.hledger.org) on Freenode (<!-- [chat log](http://ircbrowse.net/browse/hledger); --> see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) <!-- *Quick help and background chat.* --> <!-- *If you don't get an answer promptly, you can type `sm` to alert me, or leave the window open and check back later.* -->
| Twitter:       | [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime) hashtag (see also [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime)) <!-- <a href="https://twitter.com/ledgertips">@LedgerTips</a> --> <!-- *Social!* -->
| Mail list:     | [list.hledger.org](http://list.hledger.org), posting address: hledger@googlegroups.com <!-- *Slightly less quick, more eyeballs.* -->
| Issue tracker: | [bugs.hledger.org](http://bugs.hledger.org) (just the bugs), [issues.hledger.org](http://issues.hledger.org) (all issues). <!-- *Always check here.* --> <!-- *Bug reports are welcome.* -->
| Private/security issues: | [simon@joyful.com](mailto:simon@joyful.com)
<!-- | hledger-web demo&nbsp;&nbsp; | [demo.hledger.org](http://demo.hledger.org) -->
<!-- | hledger-web on Sandstorm&nbsp;&nbsp; | [hledger-web app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90), [issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=label%3A%22platform%3A%20sandstorm%22%20) -->
<!-- | hledger-api demo        | <\!-- [demo.hledger.org/api](http://demo.hledger.org/api/swagger.json), -\-> [in swagger editor](http://editor.swagger.io/#/?import=demo.hledger.org/api/swagger.json&no-proxy) -->

<div class="row">

<div class="col-sm-3">

## Introduction

### What is hledger?
An enhanced, well-documented rewrite of the original plain text accounting tool.\
[hledger intro](http://hledger.org)\
[Download](download.html)\
[Release notes](release-notes.html)\


### What is Plain Text Accounting?
Using plain text data formats and modular free software tools for robust, efficient accounting.\
[plaintextaccounting.org](http://plaintextaccounting.org)\


### What is Accounting?
Tracking your use of valuable commodities, such as money or time, for increased awareness and effectiveness.\
[Selected accounting links](http://github.com/simonmichael/hledger/wiki/more-docs)\

</div>

<div class="col-sm-3">

## Reference

[Manual](manual.html) - all the main manuals on one page

### Core tools

[hledger](hledger.html)
is the command-line UI

[hledger-ui](hledger-ui.html)
is a curses-style UI

[hledger-web](hledger-web.html)
is a web UI

[hledger-api](hledger-api.html)
is a HTTP JSON server

### File formats

[journal format](journal.html)
is hledger's native data format, representing an accounting journal

[csv format](csv.html)
allows hledger to read and write CSV, a common download format

[timeclock format](timeclock.html)
is for precise time logging with clock-in/clock-out

[timedot format](timedot.html)
is for human-friendly approximate time logging

</div>

<div class="col-sm-3">

## Wiki / Cookbook

Tutorials, how-tos and discussion of general topics.

[github.com/simonmichael/hledger/wiki](https://github.com/simonmichael/hledger/wiki){style="white-space:nowrap;"}

</div>

</div> <!-- row -->
</div> <!-- container -->

<!-- For more docs relevant to all ledger-likes, see also [plaintextaccounting.org](http://plaintextaccounting.org)  -->

