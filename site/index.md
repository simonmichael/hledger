---
title: home
...

<style>
 .highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
 .highslide-caption {color:white; background-color:black;}
 a {white-space:nowrap;}
</style>
<style media="screen and (min-width:1020px) and (orientation: landscape)">
 .indent0 { margin:0 15em 0  0em; }
 .indent1 { margin:0 10em 0  5em; }
 .indent2 { margin:0  5em 0 10em; }
 .indent3 { margin:0    0 0 15em; }
</style>

<!-- funding buttons -->
<div style="text-align:right; width:100%;">
[![github](https://img.shields.io/github/stars/simonmichael/hledger.svg?style=social&label=Github){style="min-width:102;" title="Star us!"}](https://github.com/simonmichael/hledger/)
<a href="https://opencollective.com/hledger#support"><img border="0" src="https://opencollective.com/hledger/backers/badge.svg" title="Back us with a monthly donation at Open Collective" alt="open collective backers"></a>
<a href="https://opencollective.com/hledger#support"><img border="0" src="https://opencollective.com/hledger/sponsors/badge.svg" title="Sponsor us with a $100+ monthly donation at Open Collective and get your organization's logo on our README" alt="open collective sponsors"></a>
<a href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img border="0" src="//api.flattr.com/button/flattr-badge-large.png" alt="" title="Give monthly with Flattr" alt="flattr"></a>
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" title="Contribute or claim issue bounties via Bountysource" alt="bountysource"></a>
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img width=62 height=31 border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" title="Give one time or recurringly with Paypal" alt="paypal"></a>
</div>

<!-- main content -->

<h1 style="font-size:4em;">hledger</h1>

<img src="/images/coins2-248.png" width="" height="200" style="float:right; margin:1em 1em 0 1em;" />

<h2 style="margin-top:0; margin-bottom:.5em;">
<!-- Making accounting fun for techies. -->
<!-- Plain text accounting for everyone. -->
<!-- Robust, precise, plain text accounting. -->
Robust, powerful, plain text accounting.
</h2>

<div style="font-size:medium;">

hledger is an accounting program, for tracking money, time, or
other commodities, on unix, mac and windows. With simple yet powerful
functionality accessed from command line, terminal or web browser, it is a
dependable, cross-platform alternative to Quicken, GnuCash, spreadsheets etc.

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

Read on for more about hledger, or if you're keen to get going,
**[download](download.html)** it and start the **[tutorial](step-by-step.html)** now!

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

And, a zero-setup
[web&nbsp;interface](manual.html#web) (
[demo](http://demo.hledger.org)) for a more point-and-click experience.
You can also set up your own public/private web instance in a few clicks
at
[Sandstorm](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90).
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
or maybe **[look deeper](contributing.html)**.

</div>

</div>
<!-- end of main content -->

<div style="margin-top:4em; text-align:center; ">
<div>
#### Tweets (#hledger OR #plaintextaccounting OR #ledgercli):
<a class="twitter-timeline" data-chrome="noheader" data-dnt="true" href="https://twitter.com/search?q=%23hledger%20OR%20%23plaintextaccounting%20OR%20%23ledgercli%20-RT%20-%23TheJoker" data-widget-id="707934052225945600">Tweets about #hledger OR #plaintextaccounting OR #ledgercli -RT -#TheJoker</a> <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");</script>
</div>

<!-- something to do with the github/stars button, any benefit ? -->
<!-- <script async defer src="https://buttons.github.io/buttons.js"></script> -->
