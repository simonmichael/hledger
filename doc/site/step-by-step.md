* toc

# hledger step by step

Here you can learn hledger (and a little double-entry accounting) by
practicing, one hands-on exercise at a time (similar to the
"Learn X the Hard Way" books.)

You'll learn the most if you work through each small step in order.
If a step specifies no particular task, your task is to run the examples and understand it.

If you get stuck, or have any other feedback, report it on IRC or the mail list, or send a pull request for this page.

You'll need:

1. A basic understanding of the command line, text file editing, and regular expressions. Or, the ability to ask for help on the IRC channel.

2. hledger (see [Download](download.html)). These exercises were last tested with: hledger 0.23dev-20140212.



## SETUP

### Check your hledger

Get a command prompt, and run hledger to check the version. It should be reasonably [up to date](release-notes.html):

```
$ hledger --version
hledger 0.23
```

## BASIC DATA ENTRY & REPORTING

### Locate your journal file with "hledger stats"

hledger reads financial transactions from a "journal file" (so named because it represents a [General Journal](http://en.wikipedia.org/wiki/General_Journal)).
The default journal file is in your home directory; check its path using the [stats](manual.html#stats) command.
You should see something like:
```
$ hledger stats
The hledger journal file "/home/YOU/.hledger.journal" was not found.
Please create it first, eg with "hledger add" or a text editor.
Or, specify an existing journal file with -f or LEDGER_FILE.
```

Most hledger commands read this file but can not change it; the `add` and `web` commands can also write it.

(If `stats` reports that the file exists, eg because you previously created it, move it out of the way temporarily for these exercises.)

### Record a transaction with "hledger add"

Follow the help and use the [add](manual.html#add) command to record your first transaction,
an imaginary purchase at the supermarket.
We'll go through this in detail. Later you'll learn other ways to enter data.

```
$ hledger add
Adding transactions to journal file /home/YOU/.hledger.journal
Provide field values at the prompts, or press enter to accept defaults.
Use readline keys to edit, use tab key to complete account names.
A code (in parentheses) may be entered following transaction dates.
A comment may be entered following descriptions or amounts.
If you make a mistake, enter < at any prompt to restart the transaction.
To complete a transaction, enter . when prompted.
To quit, press control-d or control-c.

Starting a new transaction.
date ? [2014/02/12]: 
```

`add` prompts for each transaction field. The first is the date.
The value in square brackets is the suggested default (today's date). Press enter to accept it.

```
description ? : trip to the supermarket
```

Transactions have an optional description (a single line of text) to help you understand them.
You can describe the transaction here, or put a payee name, or leave it blank. 
Type `trip to the supermarket` and press enter.

```
account 1 ? : expenses
```

Transactions have two or more accounts. Keep it simple; just enter `expenses` for the first one.

If you're thinking "expenses sounds more like a category": it is, but double entry accounting calls those "accounts", too.
A purchase is a transfer of money from an asset account to an expense account.
An asset is something you own, like some money in a bank account or in your pocket.
Once the money has been "moved" to an expense, you no longer own it, but the increasing balance in the expense account reminds you where it went.

```
amount  1 ? : $10
```

The amount being "moved" to `expenses`. In this case 10 US dollars.

```
account 2 ? : assets
```

Next, specify which account the money comes from. Just say `assets`.

```
amount  2 ? [$-10.0]: 
```

Now you're asked for the amount to "move" to or from the `assets` account.
As the default, hledger offers the amount required to "balance" the postings entered so far.
The minus sign indicates the money is moving from this account.
(hledger uses positive and negative sign instead of accounting's traditional "debit" and "credit" terminology.)
In a balanced transaction, the sum of posted amounts is zero, in other words no money disappears into thin air.
hledger does not allow unbalanced transactions.
Press enter to accept the default. It has an extra decimal place, but never mind.

```
account 3 (or . to complete this transaction) ? : .
```

Type `.` (period) and press enter.

```
Transaction entered:
2014/02/12 trip to the supermarket
    expenses           $10
    assets          $-10.0

Accept this transaction ? [y]:
```

You are given a chance to review the transaction just entered.
Here you see hledger's plain text data format for journal entries:
a non-indented YYYY/MM/DD date, space, and description,
followed by two or more indented posting lines, each containing an account name,
two or more spaces, and an amount. 
(Account names can contain spaces, so at least two spaces are needed to separate them from the amount.)
Press enter.

```
Added to the journal.

Starting a new transaction.
date ? [2014/02/12]: <CTRL-D>
$
```

hledger has saved it to the journal file and is ready for the next
entry.  Press control-d (on Windows, control-c) once to exit.

`stats` should now report that your journal exists and contains one transaction:

```
$ hledger stats
Main journal file        : /home/YOU/.hledger.journal
Included journal files   : 
Transactions span        : 2014-02-12 to 2014-02-13 (1 days)
Last transaction         : 2014-02-12 (0 days ago)
Transactions             : 1 (1.0 per day)
Transactions last 30 days: 1 (0.0 per day)
Transactions last 7 days : 1 (0.1 per day)
Payees/descriptions      : 1
Accounts                 : 2 (depth 1)
Commodities              : 1 ($)
```

### Show transactions with "hledger print"

The [print](manual.html#print) command shows a tidied-up view of the transaction entries in your journal.
Since there's just one so far, you should see:

```
$ hledger print
2014/02/12 trip to the supermarket
    expenses           $10
    assets            $-10
```

### Examine your journal file

List and print the journal file (on Windows, use `dir` and `type` and the file path from `hledger stats`):

```
$ ls -l ~/.hledger.journal
-rw-rw-r-- 1 YOU YOU 74 Feb 12 08:10 /home/YOU/.hledger.journal
$ cat ~/.hledger.journal

2014/02/12 trip to the supermarket
    expenses           $10
    assets
```

### A convenience: inferred amounts

Why is the amount missing from the assets posting above ?
As a convenience to make manual data entry easier, if one amount is missing
hledger infers it so as to balance the transaction ($-10 in this case).
For consistency, `add` uses the same convention when it writes an entry.
(But `print` shows the inferred amount, for clarity.)
Only one missing amount is allowed in each transaction.

### Edit the journal file

Since the journal file is plain text, you can edit it directly with any text editor.
Edit the file and change it to test whether two missing amounts is reported as an error. Eg:

```
$ emacs ~/.hledger.journal
```

Remove the expenses amount and save the file. It now looks like this:

```
2014/02/12 trip to the supermarket
    expenses           
    assets
```

Running `print` again, you should see:

```
$ hledger print
hledger: could not balance this transaction (can't have more than one missing amount; remember to put 2 or more spaces before amounts)
2014/02/12 trip to the supermarket
    expenses
    assets

```

All hledger commands expect the journal to be well-formed, and will report an error and exit otherwise.

### Two spaces

Notice the last part of that error message: "`... remember to put 2 or more spaces before amounts)`".
Another cause of this error is forgetting to put two spaces before the
amount, like this:

```
2014/02/12 trip to the supermarket
    expenses $10  ; <- only one space between expenses and $10 - need at least two
    assets
```

Since account names may contain spaces, hledger thinks the first
posting is to an account named "`expenses $10`", with a missing
amount.  So remember: two or more spaces.

### Unbalanced transactions

Edit the file to look like this:

```
2014/02/12 trip to the supermarket
    expenses        $10
    assets          $10
```

Here, we wrote both posting amounts but got the sign wrong on one of them, so they don't add up to zero.
hledger should detect this mistake. Verify it by running some command, eg `print`. You should see:

```
$ hledger print
hledger: could not balance this transaction (real postings are off by $20)
2014/02/12 trip to the supermarket
    expenses           $10
    assets             $10
```

That makes sense. (It calls them "real" postings because there are some other kinds of posting you haven't learned about yet; they aren't important.)

Correct the mistake by adding the minus sign, or just removing the assets amount entirely, and verify
that `print` works again.

### Record a transaction by editing

Edit the file again and manually add a second purchase transaction.
It's often quickest to copy & paste a similar entry, then change it.
Make the file look like this:

```
2014/02/12 trip to the supermarket
    expenses           $10
    assets

2014/02/13 forgot the bread
    expenses           $5
    assets
```

The blank line between transactions is customary, though not required.
Test your work with `print`. You should see:

```
$ hledger print
2014/02/12 trip to the supermarket
    expenses           $10
    assets            $-10

2014/02/13 forgot the bread
    expenses            $5
    assets             $-5

```

### Show postings and a running total with "hledger register"

The [register](manual.html#register) command shows transactions in a different format. More precisely, it shows postings.
Remember, a posting is an increase or decrease of some account by some amount, and a transaction contains two or more of them.
Run `register` and compare with the output of `print` above. You should see:

```
$ hledger register
2014/02/12 trip to the super..  expenses                       $10           $10
                                assets                        $-10             0
2014/02/13 forgot the bread     expenses                        $5            $5
                                assets                         $-5             0
```

Postings are displayed one per line.
The transaction's date and description is displayed only for the first posting in each transaction.
Next we see the posted account's name and the amount posted.
The final column is a running total of the posted amounts.

### Show a per-account register report

Notice how the running total above keeps resetting to 0.
This makes sense (since we know each transaction's postings add up to zero) but isn't very useful.
The register report is more useful when we restrict it to a subset of postings -
say, only the postings within a single account.
You can do this by specifying the account name as a command line argument.

Run a register report for the `expenses` account. You should see:

```
$ hledger register expenses
2014/02/12 trip to the super..  expenses                       $10           $10
2014/02/13 forgot the bread     expenses                        $5           $15
```

Now it's clear that your `expenses` balance - ie, the total amount spent - has increased to $15.

Your `assets` balance should have dropped accordingly. Check it:

```
$ hledger register assets
2014/02/12 trip to the super..  assets                        $-10          $-10
2014/02/13 forgot the bread     assets                         $-5          $-15
```

### Query expressions

The account name argument above is an example of a
[query expression](manual.html#queries), a search pattern which restricts a report to a subset of the data.
In this way  you can make very precise queries.

Note that it is a case-insensitive regular expression which matches anywhere inside the account name.
So "`e`" would match both `expenses` and `assets`.

And if you had an account named `other assets`, "`assets`" would also match that, so to match only the `assets`
account you'd need a more precise pattern like "`^assets$`".
If this doesn't make sense, read a little about regular expressions.
In a regular expression `^` means "match at the beginning" and `$` means "match at the end".

Multiple query arguments are ANDed and ORed together in a fixed way - follow the link for details.
Basically queries on the same field are ORed, and queries on different fields are ANDed.

Run the following examples and make sure they make sense, consulting the manual as needed.

Show only transactions whose description contains the regular expression "`bread`":

```
$ hledger print desc:bread
2014/02/13 forgot the bread
    expenses            $5
    assets             $-5
```

Show only postings on or after a certain date to an account whose name ends with "es":
```
$ hledger register date:2014/2/13- 'es$'
2014/02/13 forgot the bread     expenses                        $5            $5
```

Note how the account-matching pattern `es$` needs to be quoted here,
because it contains the regular expression metacharacter `$` which would otherwise be interpreted by the unix shell.

### Show accounts and their balances with "hledger balance"

The third of hledger's three core reporting commands is [balance](manual.html#balance).
Use it to list all the accounts posted to, and their ending balance.
You should see account balances agreeing with the final running total in the register reports above:

```
                $-15  assets
                 $15  expenses
--------------------
                   0
```

The overall total of these balances is also shown. As with other reports, you can use a query expression to select a subset of the data to report on.
Eg:

```
$ hledger balance assets
                $-15  assets
--------------------
                $-15
```

### balance shows the sum of matched posting amounts

Here's a balance report based only on the postings dated 2013/2/13:
```
$ hledger balance date:2014/2/13
                 $-5  assets
                  $5  expenses
--------------------
                   0
```

As you can see from this, `balance` does not always report the current
real-world account balance, rather it shows the sum of the postings
you have selected.
If you're not sure what those are, run a `register` report with the same arguments to see them:

```
$ hledger register date:2014/2/13
2014/02/13 forgot the bread     expenses                        $5            $5
                                assets                         $-5             0
```

### Review

You have learned:

- a simple plain text notation for recording financial transactions, used by hledger, Ledger and others

- what is the journal file, where it is, and how to get statistics on it with `hledger stats`

- how to record new transactions using `hledger add`

- how to record transactions by editing the journal file

- what the journal entry for a purchase looks like

- how to detect some common errors, by eye or with hledger

- how hledger selects data to report on, and how to select by account, description, or date

- how to list transactions with `hledger print`

- how to list postings and see an account's balance over time with `hledger register`

- how to list accounts and their current balance, or the sum of their postings in some period, with `hledger balance`

<!--

### Test yourself

Start a journal tracking the cash in your pocket or wallet. Every day for one week,

1. record each and every outflow and inflow of this cash, to the penny
2. run reports showing the transactions, per-account running balance, and current account balances

After seven days, do an audit:

- Count the cash. Does it exactly match the final balance in your hledger reports ?
  If not, try to identify how and when things went wrong.
  If you're confident you found the mistake, have corrected the journal and it now agrees with reality, that counts as a pass.

- Can you easily recall and understand the purpose of each transaction, with the help of your descriptions ?

You don't need to categorise, you don't need to track anything other than the amount of cash in your pocket, and it's only for seven days.
Can you complete this challenge ? Keep at it! :)
I couldn't do this when I started using hledger, but I can now. Build that muscle.

-->


## USEFUL ACCOUNTING CONCEPTS

### Assets, Liabilities and Equity

Accounting describes the status of a business, person or other entity at any point in time in terms of three amounts:

- **Assets**      - Things owned
- **Liabilities** - Things owed
- **Equity**      - The amount invested by owners/shareholders

The foundation of double-entry accounting is the [accounting equation](http://en.wikipedia.org/wiki/accounting_equation), which says
that Equity is always equal to Assets minus Liabilities (or, Net Assets).

This is also written as: Assets = Liabilities + Equity.
Another way to say it: what the entity owns is funded either by debt or by the capital provided by its owners.

These three are called the Balance Sheet accounts. Their balances summarise the overall financial status at some point in time.


### Revenue and Expenses

Two more amounts are used to describe changes in the above during a given period:

- **Revenue**     - Money flowing in
- **Expenses**    - Money flowing out

You may be accustomed to using the word Income instead Revenue.
That's fine, just remember that Income is sometimes used to mean Net
Income, which is Revenue - Expenses.

These two are called the Income Statement accounts.  The balances they
accumulate during some period of time indicate the inflows and
outflows during that period (which will affect the Assets and
Liabilities balances).


### Chart of Accounts

Five numbers does not give a lot of detail. If you want to know what
portion of expenses went to buy food, you could add up just the
transactions with (say) "supermarket" in their description. You know how to do this with hledger:

```
$ hledger register desc:supermarket expenses
2014/02/12 trip to the super..  expenses                       $10           $10
```

But descriptions are irregular, and as you can see we missed the $5 purchase on the following day.

Instead, the major "top-level" accounts above are subdivided into subaccounts which can be used
in transactions, thereby categorising them in a more structured way.
If needed, these subaccounts can be subdivided further.
This tree of accounts is called the Chart of Accounts. Here's a simple example
where `assets`, `revenue` and `expenses` each have a few subaccounts:

```
assets
  checking
  cash
liabilities
equity
revenue
  business income
  gifts received
expenses
  food
  rent
  supplies
```

In some organisations and accounting systems (eg, QuickBooks), the
tree structure is de-emphasised, so the above is represented more
like:

```
 Account name      Account type
 ------------------------------- 
 checking          ASSET
 cash              ASSET
 business income   REVENUE
 gifts received    REVENUE
 food              EXPENSE
 rent              EXPENSE
 supplies          EXPENSE
```

In others, the tree structure is encoded as decimal account numbers, something like this:

```
1000 assets
1100   checking
1200   cash
2000 liabilities
3000 equity
4000 revenue
4100   business income
4200   gifts received
5000 expenses
5100   food
5200   rent
5300   supplies
```

### Subaccounts in hledger

With hledger, tree structure is implied by writing account names like `ACCOUNT:SUBACCOUNT`.
Try it: edit your journal file and change the account names like so:

```
$ cat ~/.hledger.journal

2014/02/12 trip to the supermarket
    expenses:supplies     $10
    assets:checking

2014/02/13 forgot the bread
    expenses:food      $5
    assets:cash
```

hledger will infer the chart of accounts from these names, and `balance` will indent subaccounts to show the tree structure:

```
$ hledger balance
                $-15  assets
                 $-5    cash
                $-10    checking
                 $15  expenses
                  $5    food
                 $10    supplies
--------------------
                   0
```

For clarity, the common part of the subaccount names is not displayed.
You can see the full account names used internally like this:

```
$ hledger balance --flat --empty
                $-15  assets
                 $-5  assets:cash
                $-10  assets:checking
                 $15  expenses
                  $5  expenses:food
                 $10  expenses:supplies
--------------------
                   0
```

With `--flat`, the balance command shows full account names without indentation.
The `--empty` flag here requests that the accounts with no direct postings of their own be displayed.
Normally the balance report omits or elides such "uninteresting" accounts when they don't add much information.
(In the default indented view, they are included to clarify the structure.)

As you can see, the balance reported for parent accounts includes the
balances of any subaccounts (it would also include any postings to the
parent account itself.)

hledger accepts whatever account names you choose, so you can use as much or as little account hierarchy as you need.
Most users have at least two levels of accounts.
You can always limit the amount of detail in a balance report with `--depth`:

```
$ hledger balance --depth 1
                $-15  assets
                 $15  expenses
--------------------
                   0
```





<!--

### Transactions

A transaction is a movement of money from some account(s) to some
other account(s).  There are many common types of transaction.  A
purchase is where money moves from an asset account to an expense
account.  Eg, buying food.

-->
