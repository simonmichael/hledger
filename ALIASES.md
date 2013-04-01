---
title: hledger How to use account aliases
---

# How to use account aliases

Here's an example of using [account aliases](MANUAL.html#account-aliases).

Say a sole proprietor has a personal.journal:

    1/1
        expenses:food  $1
        assets:cash

and a business.journal:

    1/1
        expenses:office supplies  $1
        assets:business checking

Here each entity has a simple journal with its own simple chart of
accounts.  But at tax reporting time, we need to view these as a single
entity.  So in unified.journal we adjust the personal account names to fit
within the business chart of accounts:

    alias expenses    = equity:draw:personal
    alias assets:cash = assets:personal cash
    include personal.journal
    end aliases
    include business.journal

giving:

    $ hledger -f unified.journal print
    2011/01/01
        equity:draw:personal:food            $1
        assets:personal cash                $-1
    
    2011/01/01
        expenses:office supplies            $1
        assets:business checking           $-1

You can also specify aliases on the command line. This could be useful to
rewrite account names when sharing a report with someone else, such as
your accountant:

    $ hledger --alias 'my earning=income:business'

Command-line alias options are applied after any alias directives in the
journal.  At most one alias directive and one alias option will be applied
to each account name.

