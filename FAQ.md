---
title: hledger frequently asked questions
---

# Frequently asked questions

## How does hledger relate to ledger ?

hledger was inspired by and is partly a clone of John Wiegley's
[ledger](http://ledger-cli.org) (also called "c++ ledger" here.) 

I was a happy ledger user and contributor for some time; I still use it
occasionally. I wrote hledger because I wanted to develop financial tools
in the Haskell programming language and ecosystem, whose advantages I
believe are compelling. I have also tried to make hledger a little more
simple, usable, installable, documented, appealing to collaborators, and
to provide alternate user interfaces to make it more widely useful.

ledger has more advanced power-user features on the command-line
(periodic and modifier transactions, budgets, capital gains tracking,
value expressions, custom output formats, etc.) and it remains faster
and more memory efficient (for now!)...

hledger builds faster and has an up-to-date manual and an optional web
interface (which often works on ledger files too)...

The two projects collaborate freely.  We share the
[#ledger](irc://irc.freenode.net/#ledger) IRC channel but have
separate mail lists
([hledger list](http://groups.google.com/group/hledger/),
[ledger-cli list](http://groups.google.com/group/ledger-cli/)).  I try
to give back by providing infrastructure
([ledger-cli.org](http://ledger-cli.org)) and IRC support.
hledger stays compatible with ledger wherever possible, so that you
can often use both tools on the same data file. Here is
[more about compatibility](#compatibility-with-c-ledger).

Summary: hledger is a friendly, co-evolving, compatible rewrite of ledger
in Haskell, lacking some of ledger's power features and raw performance,
and focussing on robustness, usability, ease of development, and
experimental add-ons such as the [web interface](#web).
