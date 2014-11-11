# hledger is...

<!-- ## hledger is... -->

### a lightweight, dependable, cross-platform accounting tool

hledger is a computer program for easily tracking money, time, or
other commodities, on unix, mac and windows. Despite (because of ?)
limited GUI features, it is usable, capable, and reliable.  For some,
it is a simple, flexible, future-proof alternative to Quicken or
GnuCash.

### based on Ledger

hledger was inspired by and maintains substantial compatibility with [Ledger](faq.html#hledger-and-ledger),
and is part of the enthusiastic and supportive "*ledger" community.

### a command-line tool, that preserves your data

hledger is first a command-line tool. It follows Ledger's philosophy:
your data lives in a simple, safe, plain text file which you can edit
any way you wish; hledger reads that file and produces reports of
various kinds, without changing your data. (It can help you add new
transactions if you want, but does not change existing ones.)

### ...with a web UI

hledger comes with a built-in web server providing an alternate
[browser-based interface](manual.html#web), which offers assisted data
entry and point and click reporting. The web and command-line clients
can be used simultaneously.

### a Haskell application and library

hledger is written in Haskell, a modern and highly-regarded
programming language which contributes to hledger's robustness,
performance and long-term maintainability.  Most functionality is
exposed as reusable Haskell
[libraries](http://hackage.haskell.org/package/hledger-lib), making it
easy to write your own hledger-compatible
[scripts](more-docs.html#scripting-examples), [add-ons](manual.html#add-on) and
applications.

### Free software

hledger is available under the GNU GPLv3+, which helps ensure that it
will remain free and available for as long as you need it.  It has
been developed as a community project by
[Simon Michael](http://joyful.com) and
[contributors](contributors.html) since 2007.

### documented

Complete and accurate documentation is a top priority.
Undocumented functionality or stale docs are treated as serious bugs.

### comfortable for techies, usable by all

hledger aims to be useful to both computer experts and regular folks.
Currently it is a bit more suited to power users, who appreciate the
power of text files, revision control, scriptability and double entry
accounting. The web interface helps make it accessible to GUI-only
folk as well.

### focussed on serving the user

hledger strives to be accessible, practical and to provide real-world value.
I've been depending on and improving it continuously since 2007, for:

- tracking spending and income
- seeing time reports by day/week/month/project
- getting accurate numbers for client billing and tax filing
- tracking invoices
- building financial and time clarity and serenity

I needed a tool like this badly, and I want to keep it growing and helping others who can also benefit.


## how do I start ?

You could:
**[download](download.html)**,
read the **[tutorial](step-by-step.html)**,
or **[join the IRC channel](http://hledger.org/irc)**
or **[mail list](http://hledger.org/list)** and ask questions.

<!-- <style> -->
<!-- tr { -->
<!--     /*vertical-align:top;*/ -->
<!--     border-top:thin solid #bbb; -->
<!-- } -->
<!-- </style> -->
|---------------------------|-------------------------------------------------------------------------
| IRC                       | [#hledger](http://hledger.org/irc) (see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1))
| Mail list&nbsp;&nbsp;     | [list.hledger.org](http://hledger.org/list)
| Twitter                   | [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a>
| Github                    | [code.hledger.org](http://github.com/simonmichael/hledger)
| More...&nbsp;&nbsp;       | [Quick links](http://hledger.org/developer-guide.html#quick-links)

<!-- | web UI demo             | [demo.hledger.org](http://demo.hledger.org) | -->
<!-- [web ui demo](http://demo.hledger.org/register?q=inacct%3Aassets%3Abank%3Achecking+sym%3A\%24) -->

<div style="margin-top:2em; text-align:right; float:right;">
<!-- Has hledger saved you or your employer money, time or peace of mind ? -->
<!-- Donations: -->
**[Gittip](https://www.gittip.com/simonmichael/)**,
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5J33NLXYXCYAY"><img border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt="paypal"></a>
<!-- Also testimonials, examples, blogging, packaging, and patches. -->
</div>
