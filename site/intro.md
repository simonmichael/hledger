---
title: intro
...

<style>
 .highslide img {max-width:200px; float:right; margin:0 0 1em 1em;}
 .highslide-caption {color:white; background-color:black;}
 a {white-space:nowrap;}
</style>
<style media="screen and (min-width:800px) and (orientation: landscape)">
 .indent0 { margin:0 15em 0  0em; }
 .indent1 { margin:0 10em 0  5em; }
 .indent2 { margin:0  5em 0 10em; }
 .indent3 { margin:0    0 0 15em; }
</style>

<!-- main content -->

## What is hledger ?

It's a [plain text accounting](http://plaintextaccounting.org) program, 
for tracking money, time, or other commodities, 
on unix, mac and windows. 

With simple yet powerful functionality accessed from command line, terminal or web browser, 
it is a dependable, cross-platform alternative to Quickbooks, GnuCash, spreadsheets etc.

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
together with 80+ other committers and an unknown number of usually happy-sounding users.
</div>

<div class="indent2">
### inspired by Ledger

hledger is a Haskell [reimplementation](https://github.com/simonmichael/hledger/wiki/FAQ#hledger--ledger)
of the excellent [Ledger](http://ledger-cli.org).
It remains substantially compatible with Ledger, and if you wish you can keep your data compatible with both.
Read more about the [differences](https://github.com/simonmichael/hledger/wiki/FAQ#features) in the FAQ.


</div>

<div class="indent1">
### a command-line tool, that respects your data

<a href="images/balance-q-inc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/balance-q-inc.png" title="Balance report showing income/expense by quarter" /></a>

hledger is first a command-line tool.
Your data lives in a plain text journal file which you can edit
any way you wish; hledger reads that file and produces reports of
various kinds, without changing your data. (It can help you add new
transactions, but does not change existing ones.)
</div>

<a name="and-a-console-ui"></a>
<div class="indent0">
### a console UI

<a href="images/hledger-ui/hledger-ui-bcexample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-bcexample-acc.png" title="hledger-ui accounts screen" /></a>
hledger also provides a curses-style [console&nbsp;interface](manual#ui)
that lets you review account balances and transactions quickly and without fuss.
([screencast](https://asciinema.org/a/29665))
</div>

<div class="indent1">
### a web UI

<a href="images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>

And, a zero-setup
[web&nbsp;interface](manual.html#web) (
[demo](http://demo.hledger.org)) for a more point-and-click experience.
You can also set up your own public/private web instance in a few clicks
at
[Sandstorm](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90).
</div>

<div class="indent2">
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

<div class="indent1">
### fully documented

We practice documentation-driven development. 
Every feature must be **[well documented](index.html#reference)**, 
and getting started must be easy.

</div>

<div class="indent0">
### focussed on serving users

hledger strives to be usable, practical and to provide real-world value.
Intuitive features, bug-free operation and complete, accurate documentation are top goals.
Currently it is particularly suited to techies, ie users who appreciate the
power of text files, revision control, scriptability and double entry
accounting.

</div>

