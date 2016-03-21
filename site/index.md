---
title: home
...

<style>
.highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
.highslide-caption {color:white; background-color:black;}
.indent0 { margin:0 15em 0  0em; }
.indent1 { margin:0 10em 0  5em; }
.indent2 { margin:0  5em 0 10em; }
.indent3 { margin:0    0 0 15em; }
a {white-space:nowrap;}
/*div.asciicast { float:right; height:200px; }*/
</style>


<!-- <div style="float:right; max-width:200px; margin-left:1em;"> <\!-- top-right area -\-> -->

<!-- get started box 
<div class="panel panel-primary">
<div class="panel-heading">
<h2 class="panel-title">Get started</h2>
</div>
<div class="panel-body">
**[Download](download.html)**,
read the
**[release notes](release-notes.html)**,
**[tutorial](step-by-step.html)**,
or **[manual](docs.html)**,
try the **[web UI](http://demo.hledger.org)**,
introduce yourself and tell us what's good/bad..

<style>
tr {
    /*vertical-align:top;*/
    border-top:thin solid #bbb;
}
</style>
|
|---------------------------|-------------------------------------------------------------------------
| IRC                       | [#hledger](http://irc.hledger.org) 
| Mail list&nbsp;&nbsp;     | via [google](http://list.hledger.org) or [gmane](http://news.gmane.org/gmane.comp.finance.ledger.hledger)
| Twitter                   | [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a>
| Github                    | [code](http://github.com/simonmichael/hledger), [bugs](http://bugs.hledger.org), [issues](http://issues.hledger.org)
-->
<!-- (see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) -->

<!-- | More...&nbsp;&nbsp;       | [quick links](http://hledger.org/developer-guide.html#quick-links) -->

<!-- | web UI demo             | [demo.hledger.org](http://demo.hledger.org) | -->
<!-- [web ui demo](http://demo.hledger.org/register?q=inacct%3Aassets%3Abank%3Achecking+sym%3A\%24) -->

<!-- </div> -->
<!-- </div> <\!-- end of get started box -\-> -->

<!-- </div> <\!-- end of top-right area -\-> -->

<div style="float:right; text-align:right; white-space:nowrap; ">
<a style="margin-left:3px;" href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="" title="Flattr this" border="0"></a> 
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img width=62 height=31 border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt=""></a> 
<div style="display:inline-block; position:relative; top:5px; width:62px; height:31px;">
<script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script> 
</div>
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" alt=""></a> &nbsp;
</div> <!-- end of donate buttons -->

<!-- main content -->

<h1 style="font-size:6em;">hledger</h1>

<img src="/images/coins2-248.png" width="" height="200" style="float:right; margin:1em 1em 0 1em;" />

<h2 style="margin-top:0; margin-bottom:.5em;">
<!-- Making accounting fun for techies. -->
Simple, precise, plain text accounting.
<!-- Plain text accounting for everyone. -->
</h2>

<div style="font-size:medium;">

hledger is an accounting program, for tracking money, time, or
other commodities, on unix, mac and windows. With simple yet powerful
functionality accessed from command line, terminal or web browser, it is a
robust, cross-platform alternative to Quicken, GnuCash, spreadsheets etc.

<div class="indent0">
### plain text ? How does that work ?

**Step 1:**
Record your transactions in a plain text file.
(Use hledger's interactive assistant.. the web interface.. any text editor.. a shell alias.. CSV/OFX import..)
<!-- using a simple format. -->
<!-- Do it daily, or all at once. -->
<!-- Record what you know; you'll get better at it. -->

**Step 2:**
Ask hledger about your accounts.. transactions.. balances.. currencies.. monthly averages.. budgets.. market values..
You can start very simply, and get more sophisticated as you learn more about double-entry accounting.

There is an enthusiastic and growing community practising this way of accounting.
which can be quite educational and enjoyable.
If you'd like more background,
we have collected many useful resources at **[plaintextaccounting.org](http://plaintextaccounting.org)**.

And.. welcome back. Read on - or if you're eager to make a start, 
**[download](download.html)** and start the **[tutorial](step-by-step.html)** now!

<!-- I use it for: -->

<!-- - tracking spending and income -->
<!-- - seeing time reports by day/week/month/project -->
<!-- - getting accurate numbers for client billing and tax filing -->
<!-- - tracking invoices -->
<!-- - building financial and time clarity and serenity -->
</div>

<div class="indent1">
### hledger is Free software

<a href="http://www.gnu.org/licenses/gpl.html" style="float:right; margin:0 0 1em 0;"><img width="104" height="20" src="https://img.shields.io/badge/license-GPLv3+-brightgreen.svg" /></a>
hledger is Free software, created by [Simon Michael](http://joyful.com)
and released under GNU GPLv3+.

I have been actively developing and using hledger since 2007,
together with 30+ other committers, and an unknown number of usually happy-sounding users.
</div>

<div class="indent2">
### inspired by Ledger

hledger is a Haskell reimplementation of the excellent [Ledger](faq.html#hledger-and-ledger).
It remains substantially compatible with Ledger, and if you wish you can keep your data compatible with both.
Read more about the differences in the [FAQ](faq.html#hledger-and-ledger).



</div>

<div class="indent0">
### a command-line tool, that respects your data

<a href="images/balance-q-inc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/balance-q-inc.png" title="Balance report showing income/expense by quarter" /></a>

hledger is first a command-line tool. 
Your data lives in a plain text journal file which you can edit
any way you wish; hledger reads that file and produces reports of
various kinds, without changing your data. (It can help you add new
transactions, but does not change existing ones.)
</div>

<a name="and-a-console-ui"></a>
<div class="indent1">
### a console UI

<a href="images/hledger-ui/hledger-ui-bcexample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-bcexample-acc.png" title="hledger-ui accounts screen" /></a>
hledger also provides a curses-style [console&nbsp;interface](manual#ui)
that lets you review account balances and transactions quickly and without fuss.
([screencast](https://asciinema.org/a/29665))
</div>

<div class="indent2">
### a web UI

<a href="images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>

And, a zero-setup [web&nbsp;interface](manual.html#web)
([demo](http://demo.hledger.org); press `s` to open the sidebar),
for assisted data entry and a more point-and-click experience.
You can set up your own private or shared web instance with a few
clicks at [Sandstorm](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90).
</div>

<div class="indent3">
### a Haskell application and library

<a href="images/hledger-lib-api.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-lib-api.png" title="Part of hledger-lib's haddock api documentation" /></a>

hledger is written in Haskell, a modern, highly-regarded
programming language which contributes to hledger's robustness,
performance and long-term maintainability.  Most functionality is
exposed as
[reusable](http://hackage.haskell.org/package/hledger-lib)
[Haskell](http://hackage.haskell.org/package/hledger)
[libraries](http://hackage.haskell.org/package/hledger-web), making it
easy to write your own hledger-compatible
[scripts](more-docs.html#scripting-examples), [add-ons](manual.html#add-ons) and
applications.
&nbsp;&nbsp;[![build status (travis)](https://travis-ci.org/simonmichael/hledger.svg?branch=master)](https://travis-ci.org/simonmichael/hledger)
</div>

<!-- <div class="indent1"> -->
<!-- ### comfortable for techies, usable by all -->

<!-- hledger aims to be useful to both computer experts and regular folks. -->
<!-- Currently it is a bit more suited to power users, who appreciate the -->
<!-- power of text files, revision control, scriptability and double entry -->
<!-- accounting. The web interface helps make it accessible to GUI-only -->
<!-- folk as well. -->
<!-- </div> -->

<div class="indent0">
### focussed on serving users

hledger strives to be usable, practical and to provide real-world value.
Intuitive features, bug-free operation and complete, accurate documentation are top goals.
Currently it is particularly suited to techies, ie users who appreciate the
power of text files, revision control, scriptability and double entry
accounting. 

I've been using hledger daily and improving it since 2007.
I needed a tool like this badly, and I hope it will keep growing and
helping folks to transform their financial lives.
If you feel the same way, join us!

</div>

<div class="indent1">
### fully documented

Time to check out those **[docs](docs.html)**,
or maybe **[look deeper](developer-guide.html)**.

</div>

</div>
<!-- end of main content -->

<div style="margin-top:4em; text-align:center; ">
<!-- twitter widget -->
<div>
### Recent related tweets:
<a class="twitter-timeline" data-dnt="true" href="https://twitter.com/search?q=%23hledger%20OR%20%23plaintextaccounting%20OR%20%23ledgercli" data-widget-id="711933503055667200">Tweets about #hledger OR #plaintextaccounting OR #ledgercli</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
</div>
<!-- end of twitter widget -->
</div>
