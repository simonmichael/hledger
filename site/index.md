---
title: home
...

<style>
.highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
.highslide-caption {color:white; background-color:black;}
</style>

<div class="row">
<div class="col-md-9">

<div class="panel panel-primary" style="float:right; max-width:200px; margin-left:1em;">
<div class="panel-heading">
<h2 class="panel-title">Get started</h2>
</div>
<div class="panel-body">
**[Install](download.html)**,
read the **[tutorial](step-by-step.html)**
or **[manual](manual.html)**,
<!-- or **[blog posts](more-docs.html#blog-posts-articles)**, -->
join us on IRC/mail list.

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
</div>

<style>
.indent0 { margin:0 15em 0  0em; }
.indent1 { margin:0 10em 0  5em; }
.indent2 { margin:0  5em 0 10em; }
</style>

<h1 style="font-size:6em;">hledger</h1>

## Making accounting fun for techies.

<img src="/images/coins2-248.png" width="" height="200" style="float:right; margin:0 0 1em 1em;" />
hledger is a computer program for easily tracking money, time, or
other commodities, on unix, mac and windows. With simple yet powerful
functionality accessed from command line or web browser, it is a
reliable, lightweight, cross-platform alternative to Quicken or GnuCash.

<!-- I use it for: -->

<!-- - tracking spending and income -->
<!-- - seeing time reports by day/week/month/project -->
<!-- - getting accurate numbers for client billing and tax filing -->
<!-- - tracking invoices -->
<!-- - building financial and time clarity and serenity -->

<div class="indent1">

### Free&nbsp;software

hledger is available under the GNU GPLv3+, which helps ensure that it
will remain free and available for as long as you need it.  It has
been developed as a community project by
[Simon Michael](http://joyful.com) and
[contributors](contributors.html) since 2007.
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](http://www.gnu.org/licenses/gpl.html)
</div>

<div class="indent2">
### inspired by Ledger

hledger was inspired by and maintains substantial compatibility with [Ledger](faq.html#hledger-and-ledger),
and is part of the enthusiastic and supportive "*ledger" community.
</div>

<div class="indent1">
### documented

<!-- <a href="images/manual.png" class="highslide" onclick="return hs.expand(this)"><img src="images/manual.png" title="hledger's manual" /></a> -->
Complete, accurate documentation is always a top priority.
</div>

<div class="indent0">
### a command-line tool, that respects your data

<a href="images/balance-q-inc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/balance-q-inc.png" title="Balance report showing income/expense by quarter" /></a>

hledger is first a command-line tool. 
Your data lives in a simple, safe, plain text file which you can edit
any way you wish; hledger reads that file and produces reports of
various kinds, without changing your data. (It can help you add new
transactions, but does not change existing ones.)
</div>

<div class="indent1">
### and a curses UI

<a href="images/hledger-ui/hledger-ui-bcexample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-bcexample-acc.png" title="hledger-ui accounts screen" /></a>

hledger also provides a curses-style [console interface](manual#ui)
that lets you review account balances and transactions with fewer
keystrokes and less effort.
</div>

<div class="indent2">
### and a web UI

<a href="images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>

hledger comes with a built-in web server providing an alternate
[browser-based interface](manual.html#web)
([demo](http://demo.hledger.org)), for assisted data entry and point
and click reporting. The web and command-line/curses interfaces can be used
simultaneously.
</div>

<div class="indent1">
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
### focussed on serving the user

hledger strives to be usable, practical and to provide real-world value.
Currently it is particularly suited to power users who appreciate the
power of text files, revision control, scriptability and double entry
accounting. 
I've been using it daily and improving it since 2007.
I needed a tool like this badly, and I want to keep it growing and helping others gain financial clarity and control.
If you feel the same way, join us!
</div>


<div style="margin-top:3em; text-align:center; ">
<!-- Has hledger saved you or your employer money, time or peace of mind ?
<!-- Donations: -->
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" alt="bountysource"></a> &nbsp;
<a href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0"></a> &nbsp;
<div style="display:inline; position:relative; top:6px;"><script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script></div> &nbsp;
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt="paypal"></a> &nbsp;
<!-- Also testimonials, examples, blogging, packaging, and patches. -->
</div>

</div>
<div class="col-md-3">

<a class="twitter-timeline" width="" height="600px" data-dnt="true" href="https://twitter.com/hashtag/ledgercli" data-widget-id="539507319734677504"></a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>

</div>
</div>
