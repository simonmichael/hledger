---
title: home
...

<style>
 .highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
 .highslide-caption {color:white; background-color:black;}
 a {white-space:nowrap;}
</style>

<!-- INTRO -->

<img src="/images/coins2-248.png" width="" height="200" style="float:right; margin:1em 1em 0 1em;" />
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

With simple yet powerful functionality accessed from command line, terminal or web browser, 
it is a fast, secure and dependable alternative to spreadsheets, Quickbooks, GnuCash, Xero etc.

The hledger project is led by Simon Michael.
I've been building and using hledger since 2007, because I needed it.
I hope you will find it helpful in mastering your time and money.
And if you feel like helping, please join us!

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
## [User Cookbook](https://github.com/simonmichael/hledger/wiki/Home)

### Getting started

[hledger basics tutorial](https://github.com/simonmichael/hledger/wiki/hledger basics tutorial)  
[Start a journal](https://github.com/simonmichael/hledger/wiki/Start a journal)  
[Common journal entries](https://github.com/simonmichael/hledger/wiki/Common journal entries)  
[hledger accounting concepts tutorial](https://github.com/simonmichael/hledger/wiki/hledger accounting concepts tutorial)  
[FAQ](https://github.com/simonmichael/hledger/wiki/FAQ)

### Data entry, import, management

[Convert CSV files](https://github.com/simonmichael/hledger/wiki/Convert CSV files)  
[Track changes with version control](https://github.com/simonmichael/hledger/wiki/Track changes with version control)  
[Use another account separator character](https://github.com/simonmichael/hledger/wiki/Use another account separator character)  
[Full-fledged hledger](https://github.com/adept/full-fledged-hledger)  

### Reporting

[hledger tags tutorial](https://github.com/simonmichael/hledger/wiki/hledger tags tutorial)  
[Queries](https://github.com/simonmichael/hledger/wiki/Queries)  
[Rewrite account names](https://github.com/simonmichael/hledger/wiki/Rewrite account names)  

### Accounting tasks

[Budgeting and forecasting](https://github.com/simonmichael/hledger/wiki/Budgeting and forecasting)  
[Project accounting](https://github.com/simonmichael/hledger/wiki/Project accounting)  
[Track investments](https://github.com/simonmichael/hledger/wiki/Track investments)  

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

## [Dev Zone](https://github.com/simonmichael/hledger/wiki/Dev Zone)
<!-- ENDWIKICONTENT -->
</div>
</div>

</div> <!-- column -->
</div> <!-- row -->
</div> <!-- container -->


<!-- TWITTER -->

<a name="twitter"></a>
<div style="margin-top:4em; text-align:center; ">

## [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime), [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime) on Twitter:

<!-- <a href="https://twitter.com/ledgertips">@LedgerTips</a> -->
<style>
 .twitter-timeline {
   height: 1500px !important;
   /* width: 70% !important; */
 }
</style>
<a class="twitter-timeline" data-chrome="noheader" data-dnt="true" href="https://twitter.com/search?q=%23hledger%20OR%20%23plaintextaccounting%20-RT%20-%23TheJoker" data-widget-id="707934052225945600">Tweets about #hledger OR #plaintextaccounting OR #ledgercli -RT -#TheJoker</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>

</div>
