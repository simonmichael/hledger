# hledger docs

<style>
h2 { font-size:x-large; margin-top:.5em; }
h3 { font-size:large; margin-bottom:.2em; }
tr { border-top:thin solid #bbb; border-bottom:thin solid #bbb; vertical-align:top; }
td:nth-child(1) { padding-right:1em; white-space:nowrap; }
.col-sm-3, .col-sm-4 {
  padding:0;
}
.contentbox {
  padding:.1em .5em;
  margin:0 .5em .5em 0;
  border-radius:1em;
}
#introduction {
  background-color:#ffd;
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
**[hledger intro](http://hledger.org)**

### What is plain text accounting?

Using plain text data formats and version control for robust, transparent accounting.\
**[plaintextaccounting.org](http://plaintextaccounting.org)**

### What is accounting?

Tracking your use of valuable commodities, such as money or time, for increased awareness and effectiveness.\
**[Selected accounting links](http://github.com/simonmichael/hledger/wiki/more-docs)**
</div>

<div id=help class=contentbox>
## Help/Feedback

|
|-------------------|----------------------------------------------------------------------------|
| IRC:              | [#hledger](http://irc.hledger.org) on Freenode <!-- [chat log](http://ircbrowse.net/browse/hledger); --> <!-- see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1) --> <!-- *Quick help and background chat.* --> <!-- *If you don't get an answer promptly, you can type `sm` to alert me, or leave the window open and check back later.* -->
| Twitter:          | [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime), [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime) <!-- <a href="https://twitter.com/ledgertips">@LedgerTips</a> --> <!-- *Social!* -->
| Reddit:           | [/r/plaintextaccounting](https://www.reddit.com/r/plaintextaccounting/)
| Mail list:     | [list.hledger.org](http://list.hledger.org), [hledger@googlegroups.com](mailto:hledger@googlegroups.com) <!-- *Slightly less quick, more eyeballs.* -->
| Issue<br>tracker: | [bugs.hledger.org](http://bugs.hledger.org)&nbsp;(bugs), [issues.hledger.org](http://issues.hledger.org)&nbsp;(all) <!-- *Always check here.* --> <!-- *Bug reports are welcome.* -->
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

## [Dev Zone](https://github.com/simonmichael/hledger/wiki/dev/Dev Zone)
<!-- ENDWIKICONTENT -->
</div>
</div>

</div> <!-- column -->
</div> <!-- row -->
</div> <!-- container -->
