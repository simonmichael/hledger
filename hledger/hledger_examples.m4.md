m4_dnl Quick examples included early in the manual
m4_dnl TODO make these and all the command examples pasteable/doctestable ?
m4_dnl   eg include -f FILE or $LEDGER_FILE details

# COMMON TASKS

Here are some quick examples of how to do some basic tasks with hledger.
For more details, see the reference section below, the hledger_journal(5) manual,
or the more extensive docs at <https://hledger.org>.

## Getting help

```shell
$ hledger                 # show available commands
$ hledger --help          # show common options
$ hledger CMD --help      # show common and command options, and command help
$ hledger help            # show available manuals/topics
$ hledger help hledger    # show hledger manual as info/man/text (auto-chosen)
$ hledger help journal --man  # show the journal manual as a man page
$ hledger help --help     # show more detailed help for the help command
```

Find more docs, chat, mail list, reddit, issue tracker:
<https://hledger.org#help-feedback>

## Constructing a command line

hledger has an extensive and powerful command line interface. We
strive to keep it simple and ergonomic, but you may run into one of
the confusing real world details described in OPTIONS, below. 
If that happens, here are some tips that may help:

- command-specific options must go after the command (it's fine to put all options there) (`hledger CMD OPTS ARGS`)
- running add-on executables directly simplifies command line parsing (`hledger-ui OPTS ARGS`)
- enclose "problematic" args in single quotes
- if needed, also add a backslash to hide regular expression metacharacters from the shell
- to see how a misbehaving command is being parsed, add `--debug=2`.

## Starting a Journal

hledger looks for your accounting data in a journal file, `$HOME/.hledger.journal` by default:
```shell
$ hledger stats
The hledger journal file "/Users/simon/.hledger.journal" was not found.
Please create it first, eg with "hledger add" or a text editor.
Or, specify an existing journal file with -f or LEDGER_FILE.
```

You can override this by setting the `LEDGER_FILE` environment variable.
It's a good practice to keep this important file under version control,
and to start a new file each year. So you could do something like this:
```shell
$ mkdir ~/finance
$ cd ~/finance
$ git init
Initialized empty Git repository in /Users/simon/finance/.git/
$ touch 2020.journal
$ echo "export LEDGER_FILE=$HOME/finance/2020.journal" >> ~/.bashrc
$ source ~/.bashrc
$ hledger stats
Main file                : /Users/simon/finance/2020.journal
Included files           : 
Transactions span        :  to  (0 days)
Last transaction         : none
Transactions             : 0 (0.0 per day)
Transactions last 30 days: 0 (0.0 per day)
Transactions last 7 days : 0 (0.0 per day)
Payees/descriptions      : 0
Accounts                 : 0 (depth 0)
Commodities              : 0 ()
Market prices            : 0 ()
```

## Setting Opening Balances

Pick a starting date for which you can look up the balances of some
real-world assets (bank accounts, wallet..) and liabilities (credit cards..).

To avoid a lot of data entry, you may want to start with just one or
two accounts, like your checking account or cash wallet; and pick a
recent starting date, like today or the start of the week. You can
always come back later and add more accounts and older transactions,
eg going back to january 1st.

Add an opening balances transaction to the journal, declaring the
balances on this date. Here are two ways to do it:

- The first way: open the journal in any text editor and save an entry like this:
  ```journal
  2020-01-01 * opening balances
      assets:bank:checking                $1000   = $1000
      assets:bank:savings                 $2000   = $2000
      assets:cash                          $100   = $100
      liabilities:creditcard               $-50   = $-$50
      equity:opening/closing balances
  ```
  These are start-of-day balances, ie whatever was in the account at the
  end of the previous day.

  The * after the date is an optional status flag.
  Here it means "cleared & confirmed".

  The currency symbols are optional, but usually a good idea as you'll
  be dealing with multiple currencies sooner or later.

  The = amounts are optional balance assertions, providing extra error checking.

- The second way: run `hledger add` and follow the prompts to record a similar transaction:
  ```shell
  $ hledger add
  Adding transactions to journal file /Users/simon/finance/2020.journal
  Any command line arguments will be used as defaults.
  Use tab key to complete, readline keys to edit, enter to accept defaults.
  An optional (CODE) may follow transaction dates.
  An optional ; COMMENT may follow descriptions or amounts.
  If you make a mistake, enter < at any prompt to go one step backward.
  To end a transaction, enter . when prompted.
  To quit, enter . at a date prompt or press control-d or control-c.
  Date [2020-02-07]: 2020-01-01
  Description: * opening balances
  Account 1: assets:bank:checking
  Amount  1: $1000
  Account 2: assets:bank:savings
  Amount  2 [$-1000]: $2000
  Account 3: assets:cash
  Amount  3 [$-3000]: $100
  Account 4: liabilities:creditcard
  Amount  4 [$-3100]: $-50
  Account 5: equity:opening/closing balances
  Amount  5 [$-3050]: 
  Account 6 (or . or enter to finish this transaction): .
  2020-01-01 * opening balances
      assets:bank:checking                      $1000
      assets:bank:savings                       $2000
      assets:cash                                $100
      liabilities:creditcard                     $-50
      equity:opening/closing balances          $-3050
  
  Save this transaction to the journal ? [y]: 
  Saved.
  Starting the next transaction (. or ctrl-D/ctrl-C to quit)
  Date [2020-01-01]: .
  ```

If you're using version control, this could be a good time to commit the journal. Eg:
```shell
$ git commit -m 'initial balances' 2020.journal
```

## Recording Transactions

As you spend or receive money, you can record these transactions
using one of the methods above (text editor, hledger add)
or by using the [hledger-iadd](#iadd) or [hledger-web](#web) add-ons,
or by using the [import command](#import) to convert CSV data downloaded from your bank.

Here are some simple transactions, see the hledger_journal(5) manual
and hledger.org for more ideas:

```journal
2020/1/10 * gift received
  assets:cash   $20
  income:gifts

2020.1.12 * farmers market
  expenses:food    $13
  assets:cash

2020-01-15 paycheck
  income:salary
  assets:bank:checking    $1000
```

## Reconciling

Periodically you should reconcile - compare your hledger-reported balances
against external sources of truth, like bank statements or your bank's website -
to be sure that your ledger accurately represents the real-world balances
(and, that the real-world institutions have not made a mistake!).
This gets easy and fast with (1) practice and (2) frequency.
If you do it daily, it can take 2-10 minutes.
If you let it pile up, expect it to take longer as you hunt down errors and discrepancies.

A typical workflow:

1. Reconcile cash. 
   Count what's in your wallet. 
   Compare with what hledger reports (`hledger bal cash`).
   If they are different, try to remember the missing transaction, 
   or look for the error in the already-recorded transactions.
   A register report can be helpful (`hledger reg cash`).
   If you can't find the error, add an adjustment transaction.
   Eg if you have $105 after the above, and can't explain the missing $2, it could be:
   ```journal
   2020-01-16 * adjust cash
       assets:cash    $-2 = $105
       expenses:misc
   ```

2. Reconcile checking.
   Log in to your bank's website.
   Compare today's (cleared) balance with hledger's cleared balance (`hledger bal checking -C`).
   If they are different, track down the error or record the missing transaction(s)
   or add an adjustment transaction, similar to the above.
   Unlike the cash case, you can usually compare the transaction history and running balance from your bank
   with the one reported by `hledger reg checking -C`.
   This will be easier if you generally record transaction dates 
   quite similar to your bank's clearing dates.

3. Repeat for other asset/liability accounts.

Tip: instead of the register command, use hledger-ui to see a
live-updating register while you edit the journal:
`hledger-ui --watch --register checking -C`

After reconciling, it could be a good time to mark the reconciled
transactions' status as "cleared and confirmed", if you want to track
that, by adding the `*` marker.
Eg in the paycheck transaction above, insert `*` between `2020-01-15` and `paycheck`

If you're using version control, this can be another good time to commit:
```shell
$ git commit -m 'txns' 2020.journal
```

## Reporting

Here are some basic reports.

Show all transactions:
```shell
$ hledger print
2020-01-01 * opening balances
    assets:bank:checking                      $1000
    assets:bank:savings                       $2000
    assets:cash                                $100
    liabilities:creditcard                     $-50
    equity:opening/closing balances          $-3050

2020-01-10 * gift received
    assets:cash              $20
    income:gifts

2020-01-12 * farmers market
    expenses:food             $13
    assets:cash

2020-01-15 * paycheck
    income:salary
    assets:bank:checking           $1000

2020-01-16 * adjust cash
    assets:cash               $-2 = $105
    expenses:misc

```

Show account names, and their hierarchy:
```shell
$ hledger accounts --tree
assets
  bank
    checking
    savings
  cash
equity
  opening/closing balances
expenses
  food
  misc
income
  gifts
  salary
liabilities
  creditcard
```

Show all account totals:
```shell
$ hledger balance
               $4105  assets
               $4000    bank
               $2000      checking
               $2000      savings
                $105    cash
              $-3050  equity:opening/closing balances
                 $15  expenses
                 $13    food
                  $2    misc
              $-1020  income
                $-20    gifts
              $-1000    salary
                $-50  liabilities:creditcard
--------------------
                   0
```

Show only asset and liability balances, as a flat list, limited to depth 2:
```shell
$ hledger bal assets liabilities --flat -2
               $4000  assets:bank
                $105  assets:cash
                $-50  liabilities:creditcard
--------------------
               $4055
```

Show the same thing without negative numbers, formatted as a simple balance sheet:
```shell
$ hledger bs --flat -2
Balance Sheet 2020-01-16

                        || 2020-01-16 
========================++============
 Assets                 ||            
------------------------++------------
 assets:bank            ||      $4000 
 assets:cash            ||       $105 
------------------------++------------
                        ||      $4105 
========================++============
 Liabilities            ||            
------------------------++------------
 liabilities:creditcard ||        $50 
------------------------++------------
                        ||        $50 
========================++============
 Net:                   ||      $4055 
```
The final total is your "net worth" on the end date.
(Or use `bse` for a full balance sheet with equity.)

Show income and expense totals, formatted as an income statement:
```shell
hledger is 
Income Statement 2020-01-01-2020-01-16

               || 2020-01-01-2020-01-16 
===============++=======================
 Revenues      ||                       
---------------++-----------------------
 income:gifts  ||                   $20 
 income:salary ||                 $1000 
---------------++-----------------------
               ||                 $1020 
===============++=======================
 Expenses      ||                       
---------------++-----------------------
 expenses:food ||                   $13 
 expenses:misc ||                    $2 
---------------++-----------------------
               ||                   $15 
===============++=======================
 Net:          ||                 $1005 
```
The final total is your net income during this period.

Show transactions affecting your wallet, with running total:
```shell
$ hledger register cash
2020-01-01 opening balances     assets:cash                   $100          $100
2020-01-10 gift received        assets:cash                    $20          $120
2020-01-12 farmers market       assets:cash                   $-13          $107
2020-01-16 adjust cash          assets:cash                    $-2          $105
```

Show weekly posting counts as a bar chart:
```shell
$ hledger activity -W
2019-12-30 *****
2020-01-06 ****
2020-01-13 ****
```
## Starting a New File

At the end of the year, you may want to continue your journal in a new file,
so that old transactions don't slow down or clutter your reports,
and to help ensure the integrity of your accounting history.
See the [close command](#close).
