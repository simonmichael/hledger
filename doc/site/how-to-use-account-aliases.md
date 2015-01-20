# How to use account aliases

Here's an example of using [account aliases](manual.html#account-aliases).

Say a sole proprietor has a `personal.journal`:

    2014/1/2
        expenses:food  $1
        assets:cash

and a `business.journal`:

    2014/1/1
        expenses:office supplies  $1
        assets:business checking

So each entity (the business owner, and the business) has their own file with its own simple chart of accounts.
However, at tax reporting time we need to view these as a single entity (at least in the US).
In `unified.journal`, we include both files, and rewrite the personal
account names to fit into the business chart of accounts,

    alias ^expenses    = equity:draw:personal
    alias ^assets:cash = assets:personal cash
    include personal.journal
    end aliases

    include business.journal

Now we can see the data from both files at once, and the personal account names have changed:

    $ hledger -f unified.journal print
    2014/01/01                                    # from business.journal - no aliases applied
        expenses:office supplies            $1
        assets:business checking           $-1

    2014/01/02                                    # from personal.journal
        equity:draw:personal:food            $1   # <- was expenses:food
        assets:personal cash                $-1   # <- was assets:cash

You can also specify aliases on the command line. This could be useful to
quickly rewrite account names when sharing a report with someone else, such as
your accountant:

    $ hledger --alias 'my earning=income:business' ...

