% hledger_journal(5) hledger _version_
% _author_
% _monthyear_

_web_({{
_docversionlinks_({{journal}})
_toc_
}})
_man_({{

# NAME

Journal - hledger's default file format, representing a General Journal

# DESCRIPTION

}})

hledger's usual data source is a plain text file containing journal entries in hledger journal format.
This file represents a standard accounting [general journal](http://en.wikipedia.org/wiki/General_journal).
I use file names ending in `.journal`, but that's not required.
The journal file contains a number of transaction entries,
each describing a transfer of money (or any commodity) between two or more named accounts,
in a simple format readable by both hledger and humans.

hledger's journal format is a compatible subset, mostly, of [ledger's
journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with [compatible](https://github.com/simonmichael/hledger/wiki/FAQ#file-formats)
ledger journal files as well.  It's safe, and encouraged, to run both
hledger and ledger on the same journal file, eg to validate the results
you're getting.

You can use hledger without learning any more about this file;
just use the [add](#add) or [web](#web) commands to create and update it.
Many users, though, also edit the journal file directly with a text editor, perhaps assisted by the helper modes for emacs or vim.

Here's an example:

```journal
; A sample journal file. This is a comment.

2008/01/01 income               ; <- transaction's first line starts in column 0, contains date and description
    assets:bank:checking  $1    ; <- posting lines start with whitespace, each contains an account name
    income:salary        $-1    ;    followed by at least two spaces and an amount

2008/06/01 gift
    assets:bank:checking  $1    ; <- at least two postings in a transaction
    income:gifts         $-1    ; <- their amounts must balance to 0

2008/06/02 save
    assets:bank:saving    $1
    assets:bank:checking        ; <- one amount may be omitted; here $-1 is inferred

2008/06/03 eat & shop           ; <- description can be anything
    expenses:food         $1
    expenses:supplies     $1    ; <- this transaction debits two expense accounts
    assets:cash                 ; <- $-2 inferred

2008/10/01 take a loan
    assets:bank:checking  $1
    liabilities:debts    $-1

2008/12/31 * pay off            ; <- an optional * or ! after the date means "cleared" (or anything you want)
    liabilities:debts     $1
    assets:bank:checking
```

# FILE FORMAT
<!-- Now let's explore the available journal file syntax in detail. -->

## Transactions

Transactions are movements of some quantity of commodities between named accounts.
Each transaction is represented by a journal entry beginning with a [simple date](#simple-dates) in column 0. 
This can be followed by any of the following, separated by spaces:

- (optional) a [status](#status) character (empty, `!`, or `*`) 
- (optional) a transaction code (any short number or text, enclosed in parentheses)
- (optional) a transaction description (any remaining text until end of line or a semicolon)
- (optional) a transaction comment (any remaining text following a semicolon until end of line)

Then comes zero or more (but usually at least 2) indented lines representing...

##  Postings
 
A posting is an addition of some amount to, or removal of some amount from, an account. 
Each posting line begins with at least one space or tab (2 or 4 spaces is common), followed by:

- (optional) a [status](#status) character (empty, `!`, or `*`), followed by a space
- (required) an [account name](#account-names) (any text, optionally containing **single spaces**, until end of line or a double space)
- (optional) **two or more spaces** or tabs followed by an [amount](#amounts). 

Positive amounts are being added to the account, negative amounts are being removed. 

The amounts within a transaction must always sum up to zero.
As a convenience, one amount may be left blank; it will be inferred so as to balance the transaction.

Be sure to note the unusual two-space delimiter between account name and amount.
This makes it easy to write account names containing spaces.
But if you accidentally leave only one space (or tab) before the amount, the amount will be considered part of the account name.

## Dates

### Simple dates

Within a journal file, transaction dates use Y/M/D (or Y-M-D or Y.M.D)
Leading zeros are optional.
The year may be omitted, in which case it will be inferred from the context - the current transaction, the default year set with a
[default year directive](#default-year), or the current date when the command is run.
Some examples: `2010/01/31`, `1/31`, `2010-01-31`, `2010.1.31`.

### Secondary dates

Real-life transactions sometimes involve more than one date - eg the date
you write a cheque, and the date it clears in your bank.  When you want to
model this, eg for more accurate balances, you can specify individual
[posting dates](#posting-dates), which I recommend. Or, you can use the
secondary dates (aka auxiliary/effective dates) feature, supported for compatibility
with Ledger.

A secondary date can be written after the primary date, separated by
an equals sign. The primary date, on the left, is used by default; the
secondary date, on the right, is used when the `--date2` flag is
specified (`--aux-date` or `--effective` also work).

The meaning of secondary dates is up to you, but it's best to follow a
consistent rule.  Eg write the bank's clearing date as primary, and
when needed, the date the transaction was initiated as secondary.

Here's an example. Note that a secondary date will use the year of the
primary date if unspecified.

```journal
2010/2/23=2/19 movie ticket
  expenses:cinema                   $10
  assets:checking
```

```shell
$ hledger register checking
2010/02/23 movie ticket         assets:checking                $-10         $-10
```

```shell
$ hledger register checking --date2
2010/02/19 movie ticket         assets:checking                $-10         $-10
```

Secondary dates require some effort; you must use them consistently in
your journal entries and remember whether to use or not use the
`--date2` flag for your reports. They are included in hledger for
Ledger compatibility, but posting dates are a more powerful and less
confusing alternative.

### Posting dates

You can give individual postings a different date from their parent
transaction, by adding a [posting comment](#comments) containing a
[tag](#tags) (see below) like `date:DATE`.  This is probably the best
way to control posting dates precisely. Eg in this example the expense
should appear in May reports, and the deduction from checking should
be reported on 6/1 for easy bank reconciliation:

```journal
2015/5/30
    expenses:food     $10   ; food purchased on saturday 5/30
    assets:checking         ; bank cleared it on monday, date:6/1
```

```shell
$ hledger -f t.j register food
2015/05/30                      expenses:food                  $10           $10
```

```shell
$ hledger -f t.j register checking
2015/06/01                      assets:checking               $-10          $-10
```

DATE should be a [simple date](#simple-dates); if the year is not
specified it will use the year of the transaction's date.  You can set
the secondary date similarly, with `date2:DATE2`.  The `date:` or
`date2:` tags must have a valid simple date value if they are present,
eg a `date:` tag with no value is not allowed.

Ledger's earlier, more compact bracketed date syntax is also
supported: `[DATE]`, `[DATE=DATE2]` or `[=DATE2]`. hledger will
attempt to parse any square-bracketed sequence of the `0123456789/-.=`
characters in this way. With this syntax, DATE infers its year from
the transaction and DATE2 infers its year from DATE.

## Status

Transactions, or individual postings within a transaction, 
can have a status mark, which is a single character before
the transaction description or posting account name, 
separated from it by a space, indicating one of three statuses:

mark &nbsp; | status
:-----------|:-------------------
&nbsp;      | unmarked
`!`         | pending
`*`         | cleared

When reporting, you can filter by status with
the `-U/--unmarked`, `-P/--pending`, and `-C/--cleared` flags;
or the `status:`, `status:!`, and `status:*` [queries](/manual.html#queries);
or the U, P, C keys in hledger-ui.

Note, in Ledger and in older versions of hledger, the "unmarked" state is called
"uncleared". As of hledger 1.3 we have renamed it to unmarked for clarity.

To replicate Ledger and old hledger's behaviour of also matching pending, combine -U and -P.

Status marks are optional, but can be helpful eg for reconciling with real-world accounts.
Some editor modes provide highlighting and shortcuts for working with status.
Eg in Emacs ledger-mode, you can toggle transaction status with C-c C-e, or posting status with C-c C-c.

What "uncleared", "pending", and "cleared" actually mean is up to you.
Here's one suggestion:

status    | meaning
:---------|:------------------------------------------------------------
uncleared | recorded but not yet reconciled; needs review
pending   | tentatively reconciled (if needed, eg during a big reconciliation)
cleared   | complete, reconciled as far as possible, and considered correct

With this scheme, you would use
`-PC` to see the current balance at your bank,
`-U` to see things which will probably hit your bank soon (like uncashed checks),
and no flags to see the most up-to-date state of your finances.

## Description

A transaction's description is the rest of the line following the date and status mark (or until a comment begins).
Sometimes called the "narration" in traditional bookkeeping, it can be used for whatever you wish,
or left blank. Transaction descriptions can be queried, unlike [comments](#comments). 

### Payee and note

You can optionally include a `|` (pipe) character in a description to 
subdivide it into a payee/payer name on the left and additional notes on the right.
This may be worthwhile if you need to do more precise
[querying](/hledger.html#queries) and [pivoting](/hledger.html#pivoting) by payee.

## Account names

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `income`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.
Because of this, they must always be followed by **two or more spaces** (or newline).

Account names can be [aliased](#rewriting-accounts).

## Amounts

After the account name, there is usually an amount.
Important: between account name and amount, there must be **two or more spaces**.

Amounts consist of a number and (usually) a currency symbol or commodity name. Some examples:

  `2.00001`\
  `$1`\
  `4000 AAPL`\
  `3 "green apples"`\
  `-$1,000,000.00`\
  `INR 9,99,99,999.00`\
  `EUR -2.000.000,00`\
  `1 999 999.9455`\
  `EUR 1E3`\
  `1000E-6s`

As you can see, the amount format is somewhat flexible:

- amounts are a number (the "quantity") and optionally a currency symbol/commodity name (the "commodity").
- the commodity is a symbol, word, or phrase, on the left or right, with or without a separating space.
  If the commodity contains numbers, spaces or non-word punctuation it must be enclosed in double quotes.
- negative amounts with a commodity on the left can have the minus sign before or after it
- digit groups (thousands, or any other grouping) can be separated by space or comma or period and should be used as separator between all groups
- decimal part can be separated by comma or period and should be different from digit groups separator
- scientific E-notation is allowed. 
  Be careful not to use a digit group separator character in scientific notation, as it's not supported and it might get mistaken for a decimal point. 
  (Declaring the digit group separator character explicitly with a commodity directive will prevent this.) 

You can use any of these variations when recording data. However, there is some ambiguous way of representing numbers like `$1.000` and `$1,000` both may mean either one thousand or one dollar. By default hledger will assume that this is sole delimiter is used only for decimals. On the other hand commodity format declared prior to that line will help to resolve that ambiguity differently:

``` journal
commodity $1,000.00

2017/12/25 New life of Scrooge
    expenses:gifts  $1,000
    assets
```

Though journal may contain mixed styles to represent amount, when hledger displays amounts, it will choose a consistent format for each commodity.
(Except for [price amounts](#prices), which are always formatted as written). The display format is chosen as follows:

- if there is a [commodity directive](#declaring-commodities) specifying the format, that is used
- otherwise the format is inferred from the first posting amount in that commodity in the journal, and the precision (number of decimal places) will be the maximum from all posting amounts in that commmodity
- or if there are no such amounts in the journal, a default format is used (like `$1000.00`).

Price amounts and amounts in `D` directives usually don't affect amount format inference, but in some situations they can do so indirectly. (Eg when D's default commodity is applied to a commodity-less amount, or when an amountless posting is balanced using a price's commodity, or when -V is used.) If you find this causing problems, set the desired format with a commodity directive.

## Virtual Postings

When you parenthesise the account name in a posting, we call that a *virtual posting*, which
means:

- it is ignored when checking that the transaction is balanced
- it is excluded from reports when the `--real/-R` flag is used, or the `real:1` query.

You could use this, eg, to set an account's opening balance without needing to use the
`equity:opening balances` account:

```journal
1/1 special unbalanced posting to set initial balance
  (assets:checking)   $1000
```

When the account name is bracketed, we call it a *balanced virtual posting*. This is like an ordinary virtual posting except the balanced virtual postings in a transaction must balance to 0, like the real postings (but separately from them). Balanced virtual postings are also excluded by `--real/-R` or `real:1`.

```journal
1/1 buy food with cash, and update some budget-tracking subaccounts elsewhere
  expenses:food                   $10
  assets:cash                    $-10
  [assets:checking:available]     $10
  [assets:checking:budget:food]  $-10
```

Virtual postings have some legitimate uses, but those are few. You can usually find an equivalent journal entry using real postings, which is more correct and provides better error checking.

## Balance Assertions

hledger supports 
[Ledger-style balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions)
in journal files.
These look like `=EXPECTEDBALANCE` following a posting's amount. Eg in
this example we assert the expected dollar balance in accounts a and b after
each posting:

```journal
2013/1/1
  a   $1  =$1
  b       =$-1

2013/1/2
  a   $1  =$2
  b  $-1  =$-2
```

After reading a journal file, hledger will check all balance
assertions and report an error if any of them fail. Balance assertions
can protect you from, eg, inadvertently disrupting reconciled balances
while cleaning up old entries. You can disable them temporarily with
the `--ignore-assertions` flag, which can be useful for
troubleshooting or for reading Ledger files.

### Assertions and ordering

hledger sorts an account's postings and assertions first by date and
then (for postings on the same day) by parse order. Note this is
different from Ledger, which sorts assertions only by parse
order. (Also, Ledger assertions do not see the accumulated effect of
repeated postings to the same account within a transaction.)

So, hledger balance assertions keep working if you reorder
differently-dated transactions within the journal. But if you reorder
same-dated transactions or postings, assertions might break and require
updating. This order dependence does bring an advantage: precise
control over the order of postings and assertions within a day, so you
can assert intra-day balances.

### Assertions and included files

With [included files](#including-other-files), things are a little
more complicated. Including preserves the ordering of postings and
assertions. If you have multiple postings to an account on the same
day, split across different files, and you also want to assert the
account's balance on the same day, you'll have to put the assertion
in the right file.

### Assertions and multiple -f options

Balance assertions don't work well across files specified
with multiple -f options. Use include or [concatenate the files](/hledger.html#input-files)
instead.

### Assertions and commodities

The asserted balance must be a simple single-commodity amount, and in
fact the assertion checks only this commodity's balance within the
(possibly multi-commodity) account balance.  We could call this a
partial balance assertion.  This is compatible with Ledger, and makes
it possible to make assertions about accounts containing multiple
commodities.

To assert each commodity's balance in such a multi-commodity account,
you can add multiple postings (with amount 0 if necessary). But note
that no matter how many assertions you add, you can't be sure the
account does not contain some unexpected commodity. (We'll add support
for this kind of total balance assertion if there's demand.)

### Assertions and subaccounts

Balance assertions do not count the balance from subaccounts; they check
the posted account's exclusive balance. For example:
```journal
1/1
  checking:fund   1 = 1  ; post to this subaccount, its balance is now 1
  checking        1 = 1  ; post to the parent account, its exclusive balance is now 1
  equity
```
The balance report's flat mode shows these exclusive balances more clearly:
```shell
$ hledger bal checking --flat
                   1  checking
                   1  checking:fund
--------------------
                   2
```

### Assertions and virtual postings

Balance assertions are checked against all postings, both real and
[virtual](#virtual-postings). They are not affected by the `--real/-R`
flag or `real:` query.


## Balance Assignments

[Ledger-style balance assignments](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assignments) are also supported.
These are like [balance assertions](#balance-assertions), but with no posting amount on the left side of the equals sign;
instead it is calculated automatically so as to satisfy the assertion.
This can be a convenience during data entry, eg when setting opening balances:
```journal
; starting a new journal, set asset account balances 
2016/1/1 opening balances
  assets:checking            = $409.32
  assets:savings             = $735.24
  assets:cash                 = $42
  equity:opening balances
```
or when adjusting a balance to reality:
```journal
; no cash left; update balance, record any untracked spending as a generic expense
2016/1/15
  assets:cash    = $0
  expenses:misc
```

The calculated amount depends on the account's balance in the commodity at that point 
(which depends on the previously-dated postings of the commodity to that account
since the last balance assertion or assignment).
Note that using balance assignments makes your journal a little less explicit;
to know the exact amount posted, you have to run hledger or do the calculations yourself,
instead of just reading it.


## Transaction prices

Within a transaction, you can note an amount's price in another commodity.
This can be used to document the cost (in a purchase) or selling price (in a sale).
For example, transaction prices are useful to record purchases of a foreign currency.
Note transaction prices are fixed at the time of the transaction, and do not change over time.
See also [market prices](#market-prices), which represent prevailing exchange rates on a certain date.

There are several ways to record a transaction price:

1. Write the price per unit, as `@ UNITPRICE` after the amount:

    ```journal
    2009/1/1
      assets:euros     €100 @ $1.35  ; one hundred euros purchased at $1.35 each
      assets:dollars                 ; balancing amount is -$135.00
    ```

2. Write the total price, as `@@ TOTALPRICE` after the amount:

    ```journal
    2009/1/1
      assets:euros     €100 @@ $135  ; one hundred euros purchased at $135 for the lot
      assets:dollars
    ```

3. Specify amounts for all postings, using exactly two commodities,
   and let hledger infer the price that balances the transaction:

    ```journal
    2009/1/1
      assets:euros     €100          ; one hundred euros purchased
      assets:dollars  $-135          ; for $135
    ```

(Ledger users: Ledger uses a different [syntax](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices)
for fixed prices, `{=UNITPRICE}`, which hledger currently ignores).

Use the [`-B/--cost`](hledger.html#reporting-options) flag to convert 
amounts to their transaction price's commodity, if any.
(mnemonic: "B" is from "cost Basis", as in Ledger).
Eg here is how -B affects the balance report for the example above:

```shell
$ hledger bal -N --flat
               $-135  assets:dollars
                €100  assets:euros
$ hledger bal -N --flat -B
               $-135  assets:dollars
                $135  assets:euros    # <- the euros' cost
```

Note -B is sensitive to the order of postings when a transaction price is inferred:
the inferred price will be in the commodity of the last amount. 
So if example 3's postings are reversed, while the transaction
is equivalent, -B shows something different:

```journal
2009/1/1
  assets:dollars  $-135               ; 135 dollars sold
  assets:euros     €100               ; for 100 euros
```
```shell
$ hledger bal -N --flat -B
               €-100  assets:dollars  # <- the dollars' selling price
                €100  assets:euros
```

## Comments

Lines in the journal beginning with a semicolon (`;`) or hash (`#`) or
star (`*`) are comments, and will be ignored. (Star comments cause
org-mode nodes to be ignored, allowing emacs users to fold and navigate
their journals with org-mode or orgstruct-mode.)

You can attach comments to a transaction by writing them after the
description and/or indented on the following lines (before the
postings).  Similarly, you can attach comments to an individual
posting by writing them after the amount and/or indented on the
following lines. 
Transaction and posting comments must begin with a semicolon (`;`).

Some examples:

```journal
# a file comment

; also a file comment

comment
This is a multiline file comment,
which continues until a line
where the "end comment" string
appears on its own (or end of file).
end comment

2012/5/14 something  ; a transaction comment
    ; the transaction comment, continued
    posting1  1  ; a comment for posting 1
    posting2
    ; a comment for posting 2
    ; another comment line for posting 2
; a file comment (because not indented)
```

You can also comment larger regions of a file using [`comment` and `end comment` directives](#comment-blocks).


## Tags

Tags are a way to add extra labels or labelled data to postings and transactions,
which you can then [search](/hledger.html#queries) or [pivot](/hledger.html#pivoting) on.

A simple tag is a word (which may contain hyphens) followed by a full colon,
written inside a transaction or posting [comment](#comments) line:
```journal
2017/1/16 bought groceries    ; sometag:
```

Tags can have a value, which is the text after the colon, up to the next comma or end of line, with leading/trailing whitespace removed:
```journal
    expenses:food    $10   ; a-posting-tag: the tag value
```

Note this means hledger's tag values can not contain commas or newlines.
Ending at commas means you can write multiple short tags on one line, comma separated:
```journal
    assets:checking       ; a comment containing tag1:, tag2: some value ...
```
Here,

- "`a comment containing `" is just comment text, not a tag
- "`tag1`" is a tag with no value
- "`tag2`" is another tag, whose value is "`some value ...`"

Tags in a transaction comment affect the transaction and all of its postings,
while tags in a posting comment affect only that posting.
For example, the following transaction has three tags (`A`, `TAG2`, `third-tag`)
and the posting has four (those plus `posting-tag`):

```journal
1/1 a transaction  ; A:, TAG2:
    ; third-tag: a third transaction tag, <- with a value
    (a)  $1  ; posting-tag:
```

Tags are like Ledger's
[metadata](http://ledger-cli.org/3.0/doc/ledger3.html#Metadata)
feature, except hledger's tag values are simple strings.

## Directives

A directive is a line in the journal beginning with a special keyword,
that influences how the journal is processed.
hledger's directives are based on a subset of Ledger's, but there are many differences 
(and also some differences between hledger versions).

Directives' behaviour and interactions can get a little bit [complex](https://github.com/simonmichael/hledger/issues/793), 
so here is a table summarising the directives and their effects, with links to more detailed docs.

<!-- <style> -->
<!-- table a code { white-space:nowrap; } -->
<!-- h1,h2,h3,h4,h5,h6 { color:red; } -->
<!-- </style> -->

| directive         | end directive       | subdirectives   | purpose                                                            | can affect (as of 2018/06)
|:------------------|:--------------------|:----------------|:-------------------------------------------------------------------|:---------------------------------------------
| [`account`]       |                     | any text        | declare an account name & optional account code                    | account code: balance reports (except `balance` single-column mode)  <!-- all entries in all files -->
| [`alias`]         | `end aliases`       |                 | rewrite account names                                              | following inline/included entries until end of current file or end directive
| [`apply account`] | `end apply account` |                 | prepend a common parent to account names                           | following inline/included entries until end of current file or end directive
| [`comment`]       | `end comment`       |                 | ignore part of journal                                             | following inline/included entries until end of current file or end directive
| [`commodity`]     |                     | `format`        | declare a commodity and its number notation & display style        | number notation: following entries in that commodity in all files; <br>display style: amounts of that commodity in reports
| [`D`]             |                     |                 | declare a commodity, number notation & display style for commodityless amounts  | commodity: all commodityless entries in all files; <br>number notation: following commodityless entries and entries in that commodity in all files; <br>display style: amounts of that commodity in reports
| [`include`]       |                     |                 | include entries/directives from another file                       | what the included directives affect
| [`P`]             |                     |                 | declare a market price for a commodity                             | amounts of that commodity in reports, when -V is used
| [`Y`]             |                     |                 | declare a year for yearless dates                                  | following inline/included entries until end of current file

[`account`]:       #declaring-accounts
[`alias`]:         #rewriting-accounts
[`apply account`]: #default-parent-account
[`comment`]:       #comment-blocks
[`commodity`]:     #declaring-commodities
[`D`]:             #default-commodity
[`include`]:       #including-other-files
[`P`]:             #market-prices
[`Y`]:             #default-year

And some definitions:

|||
|:----------------|:--------------------------------------------------------------------------------------------------------------------
| subdirective    | optional indented directive or unparsed text lines immediately following a parent directive
| account code    | numeric code influencing account display order in most balance reports
| number notation | how to interpret numbers when parsing journal entries (the identity of the decimal separator character). (Currently each commodity can have its own notation, even in the same file.)
| display style   | how to display amounts of a commodity in reports (symbol side and spacing, digit groups, decimal separator, decimal places)
| directive scope | which entries and (when there are multiple files) which files are affected by a directive

<!-- | **entries affected:**  | -->
<!-- | following     | subsequent entries in the file/parse stream -->
<!-- | delimited     | subsequent entries, until an optional end directive -->
<!-- | all           | all preceding and following entries -->
<!-- | **files affected:**    | -->
<!-- | current       | affects current file only -->
<!-- | children      | affects current file and files included by it -->
<!-- | siblings      | affects current file, included files, and other same-level files, but not higher-level files -->
<!-- | all           | affects all files -->

As you can see, directives vary in which journal entries and files they affect,
and whether they are focussed on input (parsing) or output (reports).
Some directives have multiple effects.

If you have a journal made up of multiple files, or pass multiple -f options on the command line,
note that directives which affect input typically last only until the end of their defining file.
This provides more simplicity and predictability, eg reports are not changed by writing file options in a different order.
It can be surprising at times though. 
<!-- TODO: retest
For example, in:

    hledger -f a.aliases -f b.journal

you might expect account aliases defined in a.aliases to affect b.journal, but they will not,
unless you `include a.aliases` in b.journal, or vice versa.
-->

### Comment blocks

A line containing just `comment` starts a commented region of the file,
and a line containing just `end comment` (or the end of the current file) ends it.
See also [comments](#comments).

### Including other files

You can pull in the content of additional files by writing an include directive, like this:

```journal
include path/to/file.journal
```

If the path does not begin with a slash, it is relative to the current file.
The include file path may contain
[common glob patterns](https://hackage.haskell.org/package/Glob-0.9.2/docs/System-FilePath-Glob.html#v:compile)
(e.g. `*`).

The `include` directive can only be used in journal files.
It can include journal, timeclock or timedot files, but not CSV files.

### Default year

You can set a default year to be used for subsequent dates which don't
specify a year. This is a line beginning with `Y` followed by the year. Eg:

```journal
Y2009      ; set default year to 2009

12/15      ; equivalent to 2009/12/15
  expenses  1
  assets

Y2010      ; change default year to 2010

2009/1/30  ; specifies the year, not affected
  expenses  1
  assets

1/31       ; equivalent to 2010/1/31
  expenses  1
  assets
```

### Declaring commodities

The `commodity` directive declares commodities which may be used in the journal (though currently we do not enforce this).
It may be written on a single line, like this:

```journal
; commodity EXAMPLEAMOUNT

; display AAAA amounts with the symbol on the right, space-separated,
; using period as decimal point, with four decimal places, and
; separating thousands with comma.
commodity 1,000.0000 AAAA
```

or on multiple lines, using the "format" subdirective. In this case
the commodity symbol appears twice and should be the same in both places:

```journal
; commodity SYMBOL
;   format EXAMPLEAMOUNT

; display indian rupees with currency name on the left,
; thousands, lakhs and crores comma-separated,
; period as decimal point, and two decimal places.
commodity INR
  format INR 9,99,99,999.00
```

Commodity directives have a second purpose: they define the standard display format for amounts in the commodity. 
Normally the display format is inferred from journal entries, but this can be unpredictable; 
declaring it with a commodity directive overrides this and removes ambiguity. 
Towards this end, amounts in commodity directives must always be written with a decimal point 
(a period or comma, followed by 0 or more decimal digits). 

### Default commodity

The `D` directive sets a default commodity (and display format), to be used for amounts without a commodity symbol (ie, plain numbers).
(Note this differs from Ledger's default commodity directive.)
The commodity and display format will be applied to all subsequent commodity-less amounts, or until the next `D` directive.

```journal
# commodity-less amounts should be treated as dollars
# (and displayed with symbol on the left, thousands separators and two decimal places)
D $1,000.00

1/1
  a     5    ; <- commodity-less amount, becomes $1
  b
```

As with the `commodity` directive, the amount must always be written with a decimal point. 

### Market prices

The `P` directive declares a market price, which is 
an exchange rate between two commodities on a certain date.
(In Ledger, they are called "historical prices".)
These are often obtained from a
[stock exchange](https://en.wikipedia.org/wiki/Stock_exchange),
cryptocurrency exchange, or the
[foreign exchange market](https://en.wikipedia.org/wiki/Foreign_exchange_market).

Here is the format:

```journal
P DATE COMMODITYA COMMODITYBAMOUNT
```
- DATE is a [simple date](#simple-dates)
- COMMODITYA is the symbol of the commodity being priced
- COMMODITYBAMOUNT is an [amount](#amounts) (symbol and quantity) in a
  second commodity, giving the price in commodity B of one unit of commodity A.

These two market price directives say that one euro was worth 1.35 US dollars during 2009, 
and $1.40 from 2010 onward:
```journal
P 2009/1/1 € $1.35
P 2010/1/1 € $1.40
```

The [`-V/--value`](manual.html#market-value) flag can be used to convert reported amounts
to another commodity using these prices.

### Declaring accounts

The `account` directive predeclares account names. The simplest form is `account ACCTNAME`, eg:
```journal
account assets:bank:checking
```
Currently this mainly helps with account name autocompletion in eg 
hledger add, hledger-iadd, hledger-web, and ledger-mode.  
In future it will also help detect misspelled accounts.

An account directive can also have indented subdirectives following it, which are currently ignored. Here is the full syntax:
```journal
; account ACCTNAME
;   [OPTIONALSUBDIRECTIVES]

account assets:bank:checking
  a comment
  some-tag:12345
```

### Account display order

Account directives have another purpose: they set the display order of accounts in reports.
For example, say you have these top-level accounts:
```shell
$ accounts -1
assets
equity
expenses
liabilities
misc
other
revenues
```

Ie without account declarations, they are displayed in alphabetical order.
But if you add the following account directives to the journal:
```journal
account assets
account liabilities
account equity
account revenues
account expenses
```

the display order changes to:
```shell
$ accounts -1
assets
liabilities
equity
revenues
expenses
misc
other
```

Ie, declared accounts first, in declaration order, followed by undeclared accounts in alphabetic order. 

Warning: work in progress.
This is supported by 
the accounts command
and by tabular balance reports (`balancesheet`, `balance -Y`, etc).
It is not yet supported by
non-tabular balance reports,
budget reports,
or hledger-web's sidebar. 

### Rewriting accounts

You can define account alias rules which rewrite your account names, or parts of them, 
before generating reports. 
This can be useful for:

- expanding shorthand account names to their full form, allowing easier data entry and a less verbose journal
- adapting old journals to your current chart of accounts
- experimenting with new account organisations, like a new hierarchy or combining two accounts into one
- customising reports

Account aliases also rewrite account names in [account directives](#declaring-accounts).
They do not affect account names being entered via hledger add or hledger-web.

See also [Cookbook: Rewrite account names](https://github.com/simonmichael/hledger/wiki/Rewrite-account-names).

#### Basic aliases

To set an account alias, use the `alias` directive in your journal file.
This affects all subsequent journal entries in the current file or its
[included files](#including-other-files).
The spaces around the = are optional:

```journal
alias OLD = NEW
```

Or, you can use the `--alias 'OLD=NEW'` option on the command line.
This affects all entries. It's useful for trying out aliases interactively.

OLD and NEW are case sensitive full account names.
hledger will replace any occurrence of the old account name with the
new one. Subaccounts are also affected. Eg:

```journal
alias checking = assets:bank:wells fargo:checking
# rewrites "checking" to "assets:bank:wells fargo:checking", or "checking:a" to "assets:bank:wells fargo:checking:a"
```

#### Regex aliases

There is also a more powerful variant that uses a regular expression,
indicated by the forward slashes:

```journal
alias /REGEX/ = REPLACEMENT
```

or `--alias '/REGEX/=REPLACEMENT'`.

<!-- (Can also be written `'/REGEX/REPLACEMENT/'`). -->
REGEX is a case-insensitive regular expression. Anywhere it matches
inside an account name, the matched part will be replaced by
REPLACEMENT.
If REGEX contains parenthesised match groups, these can be referenced
by the usual numeric backreferences in REPLACEMENT.
Eg:

```journal
alias /^(.+):bank:([^:]+)(.*)/ = \1:\2 \3
# rewrites "assets:bank:wells fargo:checking" to  "assets:wells fargo checking"
```

Also note that REPLACEMENT continues to the end of line (or on command line,
to end of option argument), so it can contain trailing whitespace. 

#### Multiple aliases

You can define as many aliases as you like using directives or command-line options.
Aliases are recursive - each alias sees the result of applying previous ones.
(This is different from Ledger, where aliases are non-recursive by default).
Aliases are applied in the following order:

1. alias directives, most recently seen first (recent directives take precedence over earlier ones; directives not yet seen are ignored)
2. alias options, in the order they appear on the command line

#### `end aliases`

You can clear (forget) all currently defined aliases with the `end aliases` directive:

```journal
end aliases
```

### Default parent account

You can specify a parent account which will be prepended to all accounts
within a section of the journal. Use the `apply account` and `end apply account`
directives like so:

```journal
apply account home

2010/1/1
    food    $10
    cash

end apply account
```
which is equivalent to:
```journal
2010/01/01
    home:food           $10
    home:cash          $-10
```

If `end apply account` is omitted, the effect lasts to the end of the file.
Included files are also affected, eg:

```journal
apply account business
include biz.journal
end apply account
apply account personal
include personal.journal
```

Prior to hledger 1.0, legacy `account` and `end` spellings were also supported.

A default parent account also affects [account directives](#declaring-accounts).
It does not affect account names being entered via hledger add or hledger-web.
If account aliases are present, they are applied after the default parent account.

## Periodic transactions

Periodic transaction rules describe transactions that recur.
They allow you to generate future transactions for forecasting,
without having to write them out explicitly in the journal (with `--forecast`).
Secondly, they also can be used to define budget goals (with `--budget`).

A periodic transaction rule looks like a normal journal entry,
with the date replaced by a tilde (`~`) followed by a [period expression](manual.html#period-expressions)
(mnemonic: `~` looks like a recurring sine wave.):
```journal
~ monthly
    expenses:rent          $2000
    assets:bank:checking
```
There is an additional constraint on the period expression:
the start date must fall on a natural boundary of the interval.
Eg `monthly from 2018/1/1` is valid, but `monthly from 2018/1/15` is not.

If you write a transaction description or same-line comment, 
it must be separated from the period expression by **two or more spaces**. Eg:

```
;                              2 or more spaces
;                                    ||
;                                    vv
~ every 2 weeks from 2018/6 to 2018/9  paycheck
    assets:bank:checking   $1500
    income:acme inc
```

### Forecasting with periodic transactions

With the `--forecast` flag, each periodic transaction rule generates
future transactions recurring at the specified interval.
These are not saved in the journal, but appear in all reports.
They will look like normal transactions, but with an extra
[tag](manual.html#tags-1) named `recur`, whose value is the generating period expression.

Forecast transactions start on the first occurrence, and end on the last occurrence,
of their interval within the forecast period. The forecast period:

- begins on the later of
  - the report start date if specified with -b/-p/date:
  - the day after the latest normal (non-periodic) transaction in the journal,
    or today if there are no normal transactions.

- ends on the report end date if specified with -e/-p/date:,
  or 180 days from today.

where "today" means the current date at report time.
The "later of" rule ensures that forecast transactions do not overlap normal transactions in time;
they will begin only after normal transactions end.

Forecasting can be useful for estimating balances into the future, 
and experimenting with different scenarios.
Note the start date logic means that forecasted transactions are automatically replaced
by normal transactions as you add those.

Forecasting can also help with data entry:
describe most of your transactions with periodic rules,
and every so often copy the output of `print --forecast` to the journal.

You can generate one-time transactions too:
just write a period expression specifying a date with no report interval.
(You could also write a normal transaction with a future date, but remember this disables forecast transactions on previous dates.)

### Budgeting with periodic transactions

With the `--budget` flag, currently supported by the balance command,
each periodic transaction rule declares recurring budget goals for the specified accounts.
Eg the first example above declares a goal of spending $2000 on rent
(and also, a goal of depositing $2000 into checking) every month.
Goals and actual performance can then be compared in [budget reports](/manual.html#budget-report).

For more details, see:
[balance: Budget report](manual.html#budget-report)
and
[Cookbook: Budgeting and Forecasting](https://github.com/simonmichael/hledger/wiki/Budgeting-and-forecasting).


<a name="automated-postings"></a>

## Transaction Modifiers

Transaction modifier rules describe changes that should be applied automatically to certain transactions.
Currently, this means adding extra postings (also known as "automated postings").
Transaction modifiers are enabled by the `--auto` flag.

A transaction modifier rule looks a bit like a normal journal entry,
except the first line is an equal sign (`=`) followed by a [query](manual.html#queries)
(mnemonic: `=` suggests matching something.):
```journal
= expenses:gifts
    budget:gifts  *-1
    assets:budget  *1
```

The posting amounts can be of the form `*N`, which means "the amount of the matched transaction's first posting, multiplied by N".
They can also be ordinary fixed amounts. 
Fixed amounts with no commodity symbol will be given the same commodity as the matched transaction's first posting.

This example adds a corresponding ([unbalanced](#virtual-postings)) budget posting to every transaction involving the `expenses:gifts` account:
```journal
= expenses:gifts
    (budget:gifts)  *-1

2017-12-14
  expenses:gifts  $20
  assets
```
```shell
$ hledger print --auto
2017/12/14
    expenses:gifts             $20
    (budget:gifts)            $-20
    assets
```

Like postings recorded by hand, automated postings participate in
[transaction balancing, missing amount inference](#postings)
and [balance assertions](#balance-assertions).

# EDITOR SUPPORT

Add-on modes exist for various text editors, to make working with journal
files easier. They add colour, navigation aids and helpful commands.
For hledger users who edit the journal file directly (the majority),
using one of these modes is quite recommended.

These were written with Ledger in mind, but also work with hledger files:

| Editor              |                                                                                            |  
| ------------------- | ------------------------------------------------------------------------------------------ |  
| Emacs               | <http://www.ledger-cli.org/3.0/doc/ledger-mode.html>                                       |  
| Vim                 | <https://github.com/ledger/vim-ledger>                                                     |  
| Sublime Text        | <https://github.com/ledger/ledger/wiki/Editing-Ledger-files-with-Sublime-Text-or-RubyMine> |  
| Textmate            | <https://github.com/ledger/ledger/wiki/Using-TextMate-2>                                   |  
| Text Wrangler &nbsp;| <https://github.com/ledger/ledger/wiki/Editing-Ledger-files-with-TextWrangler>             |  
| Visual Studio Code  | <https://marketplace.visualstudio.com/items?itemName=mark-hansen.hledger-vscode>           |  

<!-- Some related LedgerTips:
https://twitter.com/LedgerTips/status/504061626233159681
https://twitter.com/LedgerTips/status/502820400276193280
https://twitter.com/LedgerTips/status/502503912084361216
https://twitter.com/LedgerTips/status/501767602067472384
-->


