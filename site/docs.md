# User guide

<style>
h2 { font-size:x-large; margin-top:0.5em; }
h3 { font-size:large; margin-bottom:0.2em; }
tr { border-top:thin solid #bbb; border-bottom:thin solid #bbb; vertical-align:top; }
td:nth-child(1) { padding-right:1em; white-space:nowrap; }
</style>

<div class="container">

## Get help
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

## Get started

### [Home](index.html)
What is hledger ?

### [Download](download.html)
How to get it, and [release notes](release-notes.html).

### [Step by Step Tutorial](step-by-step.html)
Small, guided exercises introducing data entry, reporting, and accounting.

### [FAQ](faq.html)
Some frequently asked questions.

### [plaintextaccounting.org](http://plaintextaccounting.org)
More background, tips and tools from the plaintext accounting community.

### [Misc. links](more-docs.html)
Some learning resources not yet moved to the above.

</div>
<div class="col-sm-3">

## Get things done

### [hledger Cookbook](cookbook.html)
Practical recipes and examples, including:

<div style="padding-left:0em;">

[Start a journal](start-journal.html)\
[Track changes with version control](version-control.html)\
[Example journal entries](entries.html)\
[Convert CSV files](csv-import.html)\
[Rewrite account names](account-aliases.html)\
[Use another account separator character](account-separator.html)\
[Track investments](investments.html)\

</div>

</div>
<div class="col-sm-3">

## Get the details

### [Manuals](manual.html)
The full reference guide. Includes:

<div style="padding-left:0em;">

**Tools**

**[`hledger`](hledger.html)**\
The command-line UI
, for flexibility and automation.

**[`hledger-ui`](hledger-ui.html)**\
A curses-style UI
, for quick review and monitoring.

**[`hledger-web`](hledger-web.html)**\
A web UI
, for browsing, sharing, and collaboration.

**[`hledger-api`](hledger-api.html)**\
A basic web API
, for building client-side apps. 

**File formats**

**[Journal](journal.html)**\
hledger's native data format, representing an accounting journal.

**[CSV](csv.html)**\
Comma Separated Values, used for import/export.

**[Timeclock](timeclock.html)**\
For time logging, with clock-in/clock-out records.

**[Timedot](timedot.html)**\
A more human-friendly time logging format.

</div>

</div> <!-- col -->

</div> <!-- row -->
</div> <!-- container -->


