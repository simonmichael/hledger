---
title: home
...

<style>
 .highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
 .highslide-caption {color:white; background-color:black;}
 a {white-space:nowrap;}
</style>

<!-- INTRO -->

<img src="images/coins2-248.png" width="" height="200" style="float:right; margin:1em 1em 0 1em;" />
<h1 style="/*font-size:4em;*/ ">hledger</h1>
<span style="font-size:xx-large;">
Friendly, robust, plain text accounting.
<!-- Robust, powerful, plain text accounting. -->
<!-- Robust, precise, plain text accounting. -->
<!-- Plain text accounting for everyone. -->
<!-- Making accounting fun for techies. -->
</span>

<span style="font-size:x-large;">hledger</span> is an accounting program, for tracking money, time, or
other commodities. It is cross platform and released under GNU GPLv3.
<!-- [![...](https://api.travis-ci.org/simonmichael/hledger.svg?branch=master)](https://travis-ci.org/simonmichael/hledger/builds) -->
<!-- [![...](https://ci.appveyor.com/api/projects/status/5vejw0w5n5igdr42?svg=true)](https://ci.appveyor.com/project/simonmichael/hledger/history) -->
[![...](https://img.shields.io/github/stars/simonmichael/hledger.svg?style=social&label=Github){style="min-width:102;" title="Star us!"}](https://github.com/simonmichael/hledger/)

With simple yet powerful functionality accessed from command line, terminal or web browser, 
hledger is a fast, secure, dependable alternative to spreadsheets, Quickbooks, GnuCash, Xero etc.

The hledger project is led by Simon Michael.
I've been building and using hledger since 2007, because I needed it.
Welcome, and I hope you find it helpful in mastering your time and money.
And if you feel like helping this project grow, please join us!

<!-- FUNDING -->

<div style="text-align:center;">
<a href="https://opencollective.com/hledger#support"><img border="0" src="https://opencollective.com/hledger/backers/badge.svg" title="Back us with a monthly donation at Open Collective" alt="open collective backers"></a>
<a href="https://opencollective.com/hledger#support"><img border="0" src="https://opencollective.com/hledger/sponsors/badge.svg" title="Sponsor us with a $100+ monthly donation at Open Collective and get your organization's logo on our README" alt="open collective sponsors"></a>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img width=62 height=31 border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" title="Give one time or recurringly with Paypal" alt="paypal"></a>
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" title="Contribute or claim issue bounties via Bountysource" alt="bountysource"></a>
<!-- <a href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img border="0" src="//api.flattr.com/button/flattr-badge-large.png" alt="" title="Give monthly with Flattr" alt="flattr"></a> -->
</div>

<!-- DOCS -->

<style>
h2 { font-size:x-large; margin-top:.5em; }
h3 { font-size:large; margin-bottom:.2em; }
tr { border-top:thin solid #bbb; border-bottom:thin solid #bbb; vertical-align:top; }
td:nth-child(1) { padding-right:1em; white-space:nowrap; }
.col-sm-3, .col-sm-4 {
  padding:0;
}
.contentbox {
  margin:1em .5em .5em 0;
  padding:.1em .5em;
  border-radius:1em;
}
#introduction {
/*  background-color:#ffb; */
}
#help {
  background-color:#fdd;
}
#reference {
  background-color:#eef;
  text-align:left;
}
#reference .subcontent {
  margin-left:1em;
}
#wiki {
  background-color:#efe;
  text-align:center;
}
#wiki .subcontent {
}
</style>

<div class="container">
<div class="row">
<div class="col-sm-3">

<div id=introduction class=contentbox>
## Introduction

### What is hledger?

An enhanced, well-documented reimplementation of plain text accounting.\
**[What is hledger ?](intro.html)**

### What is plain text accounting?

Using plain text data formats and version control for robust, transparent accounting.\
**[plaintextaccounting.org](http://plaintextaccounting.org)**

### What is accounting?

Tracking your use of valuable commodities, such as money or time, for increased awareness and effectiveness.\
**[Selected accounting links](http://github.com/simonmichael/hledger/wiki/more-docs#accounting)**
</div>

<div id=help class=contentbox>
## Help/Feedback

|
|-------------------|----------------------------------------------------------------------------|
| IRC:              | [#hledger](http://irc.hledger.org) on Freenode <!-- [chat log](http://ircbrowse.net/browse/hledger); --> <!-- see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1) --> <!-- *Quick help and background chat.* --> <!-- *If you don't get an answer promptly, you can type `sm` to alert me, or leave the window open and check back later.* -->
| Twitter:          | [#hledger,<br>#plaintextaccounting](#twitter) <!-- *Social!* -->
| Reddit:           | [/r/plaintextaccounting](https://www.reddit.com/r/plaintextaccounting/)
| Mail list:        | [list.hledger.org](http://list.hledger.org), [hledger@googlegroups.com](mailto:hledger@googlegroups.com) <!-- *Slightly less quick, more eyeballs.* -->
| Issues:           | [bugs.hledger.org](http://bugs.hledger.org)&nbsp;(bugs), [issues.hledger.org](http://issues.hledger.org)&nbsp;(all) <!-- *Always check here.* --> <!-- *Bug reports are welcome.* -->
| Other:            | [simon@joyful.com](mailto:simon@joyful.com)
<!-- | hledger-web demo&nbsp;&nbsp; | [demo.hledger.org](http://demo.hledger.org) -->
<!-- | hledger-web on Sandstorm&nbsp;&nbsp; | [hledger-web app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90), [issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=label%3A%22platform%3A%20sandstorm%22%20) -->
<!-- | hledger-api demo        | <\!-- [demo.hledger.org/api](http://demo.hledger.org/api/swagger.json), -\-> [in swagger editor](http://editor.swagger.io/#/?import=demo.hledger.org/api/swagger.json&no-proxy) -->
</div> <!-- column -->

</div>
<div class="col-sm-3">

<div id=reference class=contentbox>
## Reference

**[Release notes](release-notes.html)**\
What's new in each hledger version.

**[User manual](manual.html)**\
The main hledger manuals combined on one page for easy searching.
Includes:

<div class=subcontent>
[hledger](hledger.html)\
the command-line UI

[hledger-ui](hledger-ui.html)\
a curses-style UI

[hledger-web](hledger-web.html)\
a web UI

[hledger-api](hledger-api.html)\
a HTTP JSON server

[journal format](journal.html)\
hledger's native file format

[csv format](csv.html)\
hledger's CSV import system

[timeclock format](timeclock.html)\
a file format for precise time logging

[timedot format](timedot.html)\
a file format for human-friendly approximate time logging
</div>
</div>

</div> <!-- column -->
<div class="col-sm-4">

<div id=wiki class=contentbox>
<div class=subcontent>
<!-- WIKICONTENT -->
## [Cookbook](https://github.com/simonmichael/hledger/wiki/Cookbook)

### Getting started

[hledger basics tutorial](https://github.com/simonmichael/hledger/wiki/hledger basics tutorial)  
[hledger terminology](https://github.com/simonmichael/hledger/wiki/hledger terminology)  
[Create a journal](https://github.com/simonmichael/hledger/wiki/Create a journal)  
[Common journal entries](https://github.com/simonmichael/hledger/wiki/Common journal entries)  
[hledger accounting concepts tutorial](https://github.com/simonmichael/hledger/wiki/hledger accounting concepts tutorial)   
[hledger multicurrency tutorial](https://github.com/simonmichael/hledger/wiki/hledger multicurrency tutorial)       
[FAQ](https://github.com/simonmichael/hledger/wiki/FAQ)

### Data entry, import, management

[Convert CSV files](https://github.com/simonmichael/hledger/wiki/Convert CSV files)  
[Customize default CSV accounts](https://github.com/simonmichael/hledger/wiki/Customize default CSV accounts)  
[Track changes with version control](https://github.com/simonmichael/hledger/wiki/Track changes with version control)  
[Use another account separator character](https://github.com/simonmichael/hledger/wiki/Use another account separator character)  
["Full-fledged Hledger" setup](https://github.com/adept/full-fledged-hledger)  
["hledger: Make It So" setup](https://github.com/apauley/hledger-makeitso)

### Reporting

[hledger tags tutorial](https://github.com/simonmichael/hledger/wiki/hledger tags tutorial)  
[Queries](https://github.com/simonmichael/hledger/wiki/Queries)  
[Rewrite account names](https://github.com/simonmichael/hledger/wiki/Rewrite account names)  

### Accounting tasks

[Foreign trip expenses](https://github.com/simonmichael/hledger/wiki/Foreign trip expenses)  
[Budgeting and forecasting](https://github.com/simonmichael/hledger/wiki/Budgeting and forecasting)  
[Project accounting](https://github.com/simonmichael/hledger/wiki/Project accounting)  
[Track investments](https://github.com/simonmichael/hledger/wiki/Track investments)  
[Time planning](https://github.com/simonmichael/hledger/wiki/Time planning)

### Usage tips

[Addons](https://github.com/simonmichael/hledger/wiki/Addons)  
[Mobile apps](https://github.com/simonmichael/hledger/wiki/Mobile apps)  
[Save frequently used options](https://github.com/simonmichael/hledger/wiki/Save frequently used options)  

### More..

[More docs](https://github.com/simonmichael/hledger/wiki/More docs)  
[plaintextaccounting.org](http://plaintextaccounting.org) -
[ledger‑likes](http://plaintextaccounting.org/#ledger-likes) -
[tools](http://plaintextaccounting.org/#related-tools) -
[docs](http://plaintextaccounting.org/#docs) -
[common&nbsp;tasks](http://plaintextaccounting.org/#common-tasks) -
[discussion](http://plaintextaccounting.org/#discussion)  
[ledger-cli.org](http://ledger-cli.org) - [docs](https://www.ledger-cli.org/docs.html) - [wiki](https://github.com/ledger/ledger/wiki)  
[Beancount's docs](http://furius.ca/beancount/doc/index)  
<!-- ENDWIKICONTENT -->
</div>
</div>

</div> <!-- column -->
</div> <!-- row -->
</div> <!-- container -->

