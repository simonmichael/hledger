---
title: home
...

<style>
.highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
.highslide-caption {color:white; background-color:black;}
</style>

<div class="row"> <!-- main row -->
<!-- <div class="col-md-9"> <\!-- main column -\-> -->
<div class=""> <!-- main column -->

<div style="float:right; max-width:200px; margin-left:1em;"> <!-- top-right area -->

<!-- get started box -->
<div class="panel panel-primary">
<div class="panel-heading">
<h2 class="panel-title">Get started</h2>
</div>
<div class="panel-body">
**[Install](download.html)**,
read the
**[release notes](release-notes.html)**,
**[tutorial](step-by-step.html)**,
or **[manual](manual.html)**,
<!-- or **[blog posts](more-docs.html#blog-posts-articles)**, -->
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
| IRC                       | [#hledger](http://irc.hledger.org) <!-- (see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) -->
| Mail list&nbsp;&nbsp;     | via [google](http://list.hledger.org) or [gmane](http://news.gmane.org/gmane.comp.finance.ledger.hledger)
| Twitter                   | [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a>
| Github                    | [code](http://github.com/simonmichael/hledger), [bugs](http://bugs.hledger.org), [issues](http://issues.hledger.org)

<!-- | More...&nbsp;&nbsp;       | [quick links](http://hledger.org/developer-guide.html#quick-links) -->

<!-- | web UI demo             | [demo.hledger.org](http://demo.hledger.org) | -->
<!-- [web ui demo](http://demo.hledger.org/register?q=inacct%3Aassets%3Abank%3Achecking+sym%3A\%24) -->

</div>
</div> <!-- end of get started box -->

</div> <!-- end of top-right area -->

<div style="float:right; max-width:270px; margin-left:1em;"> <!-- second top-right area -->
<!-- donate buttons -->
<div style="margin-top:1em; text-align:center;">
 <!-- Has hledger saved you or your employer money, time or peace of mind ?
<!-- Would you like to support our mission ? (and what should it be ?)
<!-- Donations: -->
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt="paypal"></a> &nbsp;\
<!-- <div style="display:inline; position:relative; top:6px;"><script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script></div> &nbsp; -->
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" alt="bountysource"></a> &nbsp;
<a href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0"></a> &nbsp;
<!-- Also testimonials, examples, blogging, packaging, and patches. -->
</div> <!-- end of donate buttons -->
</div> <!-- end of second top-right area -->

<!-- main content -->

<style>
.indent0 { margin:0 15em 0  0em; }
.indent1 { margin:0 10em 0  5em; }
.indent2 { margin:0  5em 0 10em; }
.indent3 { margin:0    0 0 15em; }
/*div.asciicast { float:right; height:200px; }*/
</style>

<h1 style="font-size:6em;">hledger</h1>

<img src="/images/coins2-248.png" width="" height="200" style="float:right; margin:1em 1em 0 1em;" />

<h2 style="margin-top:0; margin-bottom:.5em;">
<!-- Making accounting fun for techies. -->
Simple, precise, future-proof accounting for techies.
</h2>

<div style="font-size:large;"> <!-- large text -->

hledger is a lightweight accounting program for tracking money, time, or
other commodities, on unix, mac and windows. With simple yet powerful
functionality accessed from command line, terminal or web browser, it is a
reliable, cross-platform alternative to Quicken, GnuCash, spreadsheets etc.

**Step 1:** Record your transactions in a plain text file (using any text editor,
hledger's add command, the web interface, CSV or OFX import, custom scripts..)

**Step 2:** Ask hledger about your accounts, currencies, balances,
monthly averages, market values and more.

You can start out very simple, and get more sophisticated as you learn
more about double-entry accounting.  You'll feel closer to your
finances, and accounting becomes fun.  [Try it!](step-by-step.html)

<!-- I use it for: -->

<!-- - tracking spending and income -->
<!-- - seeing time reports by day/week/month/project -->
<!-- - getting accurate numbers for client billing and tax filing -->
<!-- - tracking invoices -->
<!-- - building financial and time clarity and serenity -->

<div class="indent1">
### Free&nbsp;software

hledger is Free software released under GNU GPLv3+, which helps ensure
its longevity.  It has been developed by
[Simon Michael](http://joyful.com) and contributors since 2007.
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](http://www.gnu.org/licenses/gpl.html)
</div>

<div class="indent2">
### inspired by Ledger

hledger is a Haskell reimplementation of the excellent [Ledger](faq.html#hledger-and-ledger).
It remains substantially compatible with Ledger - if you wish you can keep your data compatible with both -
and it is part of the enthusiastic and growing *ledger community.
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
that lets you review account balances and transactions with fewer
keystrokes and less effort.
<script type="text/javascript" data-t="9" data-autoplay=0 src="https://asciinema.org/a/29665.js" id="asciicast-29665" async></script>
</div>

<div class="indent2">
### a web UI

<a href="images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>

hledger comes with a built-in web server providing a
[web&nbsp;interface](manual.html#web)
([demo](http://demo.hledger.org)), for assisted data entry and point
and click reporting. The web and command-line/curses interfaces can be used
simultaneously.
</div>

<div class="indent3">
### a Haskell application and library

<a href="images/hledger-lib-api.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-lib-api.png" title="Part of hledger-lib's haddock api documentation" /></a>

hledger is written in Haskell, a modern, highly-regarded
programming language which contributes to hledger's robustness,
performance and long-term maintainability.  Most functionality is
exposed as reusable Haskell
[libraries](http://hackage.haskell.org/package/hledger-lib), making it
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
I needed a tool like this badly, and I want to keep it growing and
helping others to transform their financial lives.
If you feel the same way, join us!

</div>

</div> <!-- end of large text -->

<!-- end of main content -->

<div style="margin-top:3em; text-align:center; ">

<!-- twitter widget -->
<!-- <div class="col-md-3"> -->
<div style="margin-top:2em;">
### Latest tweets about hledger and Ledger:
<a class="twitter-timeline" width="" height="600px" data-dnt="true" href="https://twitter.com/hashtag/ledgercli" data-widget-id="539507319734677504"></a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
</div> <!-- end of twitter widget -->

</div>

</div> <!-- end of main row -->
