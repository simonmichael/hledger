% hledger_journal(5) hledger _version_
% _author_
% _monthyear_

_web_({{
_versions_({{journal}})
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

hledger's journal format is a compatible subset, [mostly](faq.html#file-format-differences),
of [ledger's journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with [compatible](faq.html#file-format-differences) ledger journal files as well.
It's safe, and encouraged, to run both hledger and ledger on the same journal file,
eg to validate the results you're getting.

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

2008/12/31 * pay off            ; <- an optional * or ! after the date means "cleared" (or anything you want)
    liabilities:debts     $1
    assets:bank:checking
```

# FILE FORMAT
<!-- Now let's explore the available journal file syntax in detail. -->

## Transactions

Transactions are represented by journal entries. Each begins with a
[simple date](#simple-dates) in column 0, followed by three optional
fields with spaces between them:

- a status flag, which can be empty or `!` or `*` (meaning "uncleared",
  "pending" and "cleared", or whatever you want)
- a transaction code (eg a check number),
- and/or a description

then some number of postings, of some amount to some account. Each
posting is on its own line, consisting of:

- indentation of one or more spaces (or tabs)
- optionally, a `!` or `*` status flag followed by a space
- an account name, optionally containing single spaces
- optionally, two or more spaces or tabs followed by an amount

Usually there are two or more postings, though one or none is also
possible. The posting amounts within a transaction must always balance,
ie add up to 0. Optionally one amount can be left blank, in which case
it will be inferred.

## Dates

### Simple dates

Within a journal file, transaction dates use Y/M/D (or Y-M-D or Y.M.D)
Leading zeros are optional.
Year/month may be omitted, in which case they will be inferred from the context - the current transaction, default values set with a
[default year directive](#default-year), or the current date when the command is run.
Some examples: `2010/01/31`, `1/31`, `2010-01-31`, `2010.1.31`, `29`.

### Secondary dates

Real-life transactions sometimes involve more than one date - eg the date
you write a cheque, and the date it clears in your bank.  When you want to
model this, eg for more accurate balances, you can specify individual
[#posting-dates](posting dates), which I recommend. Or, you can use the
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

## Account names

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `income`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.
Because of this, they must always be followed by **two or more spaces** (or newline).

Account names can be [aliased](#account-aliases).

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
  `EUR -2.000.000,00`

As you can see, the amount format is somewhat flexible:

- amounts are a number (the "quantity") and optionally a currency symbol/commodity name (the "commodity").
- the commodity is a symbol, word, or double-quoted phrase, on the left or right, with or without a separating space
- negative amounts with a commodity on the left can have the minus sign before or after it
- digit groups (thousands, or any other grouping) can be separated by commas (in which case period is used for decimal point) or periods (in which case comma is used for decimal point)

You can use any of these variations when recording data, but when hledger displays amounts, it will choose a consistent format for each commodity.
(Except for [price amounts](#prices), which are always formatted as written). The display format is chosen as follows:

- if there is a [commodity directive](#commodity-directive) specifying the format, that is used
- otherwise the format is inferred from the first posting amount in that commodity in the journal, and the precision (number of decimal places) will be the maximum from all posting amounts in that commmodity
- or if there are no such amounts in the journal, a default format is used (like `$1000.00`).

Price amounts and amounts in D directives usually don't affect amount format inference, but in some situations they can do so indirectly. (Eg when D's default commodity is applied to a commodity-less amount, or when an amountless posting is balanced using a price's commodity, or when -V is used.) If you find this causing problems, set the desired format with a commodity directive.

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

hledger supports ledger-style
[balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions)
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

With [included files](#including-other-files), things are a little
more complicated. Including preserves the ordering of postings and
assertions. If you have multiple postings to an account on the same
day, split across different files, and you also want to assert the
account's balance on the same day, you'll have to put the assertion
in the right file.


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


## Prices

### Transaction prices

When recording a transaction, you can also record an amount's price in another commodity.
This documents the exchange rate, cost (of a purchase), or selling price (of a sale) that was in effect within this particular transaction (or more precisely, within the particular posting).
These transaction prices are fixed, and do not change.

Such priced amounts can be displayed in their transaction price's
commodity, by using the `--cost/-B` flag (B for "cost Basis"),
supported by most hledger commands.

There are three ways to specify a transaction price:

1. Write the unit price (aka exchange rate), as `@ UNITPRICE` after the amount:

    ```journal
    2009/1/1
      assets:foreign currency   €100 @ $1.35  ; one hundred euros at $1.35 each
      assets:cash
    ```

2. Or write the total price, as `@@ TOTALPRICE` after the amount:

    ```journal
    2009/1/1
      assets:foreign currency   €100 @@ $135  ; one hundred euros at $135 for the lot
      assets:cash
    ```

3. Or let hledger infer the price so as to balance the transaction.
   To permit this, you must fully specify all posting amounts, and
   their sum must have a non-zero amount in exactly two commodities:

    ```journal
    2009/1/1
      assets:foreign currency   €100          ; one hundred euros
      assets:cash              $-135          ; exchanged for $135
    ```

With any of the above examples we get:

```shell
$ hledger print -B
2009/01/01
    assets:foreign currency       $135.00
    assets:cash                  $-135.00
```

Example use for transaction prices: recording the effective conversion
rate of purchases made in a foreign currency.

### Market prices

Market prices are not tied to a particular transaction; they represent
historical exchange rates between two commodities, usually from some
public market which publishes such rates.

When market prices are known, the `-V/--value` option will use them to
convert reported amounts to their market value as of the report end
date. This option is currently available only with the
[balance](#balance) command.

You record market prices (Ledger calls them historical prices) with a
P directive, in the journal or perhaps in a separate
[included](#including-other-files) file.  Market price directives have
the format:
```journal
P DATE COMMODITYSYMBOL UNITPRICE
```
<!-- (A time and numeric time zone are allowed but ignored, like ledger.) -->

For example, the following directives say that the euro's exchange rate was 1.35 US dollars during 2009, and $1.40 from 2010 onward (and unknown before 2009).
```journal
P 2009/1/1 € $1.35
P 2010/1/1 € $1.40
```

Example use for market prices: tracking the value of stocks.

<!--
This is different from Ledger, where transaction prices fluctuate by
default.  Ledger has a different syntax for specifying
[fixed prices](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices):
`{=PRICE}`.  hledger parses that syntax, and (currently) ignores it.
-->
<!-- hledger treats this as an alternate spelling of `@ PRICE`, for greater compatibility with Ledger files. -->

## Comments

Lines in the journal beginning with a semicolon (`;`) or hash (`#`) or
asterisk (`*`) are comments, and will be ignored. (Asterisk comments
make it easy to treat your journal like an org-mode outline in emacs.)

Also, anything between [`comment` and `end comment` directives](#multi-line-comments) is a (multi-line) comment.
If there is no `end comment`, the comment extends to the end of the file.

You can attach comments to a transaction by writing them after the
description and/or indented on the following lines (before the
postings).  Similarly, you can attach comments to an individual
posting by writing them after the amount and/or indented on the
following lines.

Some examples:

```journal
# a journal comment

; also a journal comment

comment
This is a multiline comment,
which continues until a line
where the "end comment" string
appears on its own.
end comment

2012/5/14 something  ; a transaction comment
    ; the transaction comment, continued
    posting1  1  ; a comment for posting 1
    posting2
    ; a comment for posting 2
    ; another comment line for posting 2
; a journal comment (because not indented)
```

## Tags

A *tag* is a word followed by a full colon inside a transaction or
posting [comment](#comments).  You can write multiple tags, comma
separated. Eg: `; a comment containing sometag:, anothertag:`.  You can search for tags
with the [`tag:` query](manual#queries).

A tag can also have a value, which is any text between the colon and
the next comma or newline, excluding leading/trailing whitespace.
(So hledger tag values can not contain commas or newlines).

Tags in a transaction comment affect the transaction and all of its postings,
while tags in a posting comment affect only that posting.
For example, the following transaction has three tags (A, TAG2, third-tag)
and the posting has four (A, TAG2, third-tag, posting-tag):

```journal
1/1 a transaction  ; A:, TAG2:
    ; third-tag: a third transaction tag, this time with a value
    (a)  $1  ; posting-tag:
```

Tags are like Ledger's
[metadata](http://ledger-cli.org/3.0/doc/ledger3.html#Metadata)
feature, except hledger's tag values are simple strings.

## Directives

### Account aliases

You can define aliases which rewrite your account names (after reading the journal,
before generating reports). hledger's account aliases can be useful for:

- expanding shorthand account names to their full form, allowing easier data entry and a less verbose journal
- adapting old journals to your current chart of accounts
- experimenting with new account organisations, like a new hierarchy or combining two accounts into one
- customising reports

See also [How to use account aliases](how-to-use-account-aliases.html).

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

OLD and NEW are full account names.
hledger will replace any occurrence of the old account name with the
new one. Subaccounts are also affected. Eg:

```journal
alias checking = assets:bank:wells fargo:checking
# rewrites "checking" to "assets:bank:wells fargo:checking", or "checking:a" to "assets:bank:wells fargo:checking:a"
```

#### Regex aliases

There is also a more powerful variant that uses a regular expression,
indicated by the forward slashes. (This was the default behaviour in hledger 0.24-0.25):

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
Note, currently regular expression aliases may cause noticeable slow-downs.
(And if you use Ledger on your hledger file, they will be ignored.)
Eg:

```journal
alias /^(.+):bank:([^:]+)(.*)/ = \1:\2 \3
# rewrites "assets:bank:wells fargo:checking" to  "assets:wells fargo checking"
```

#### Multiple aliases

You can define as many aliases as you like using directives or command-line options.
Aliases are recursive - each alias sees the result of applying previous ones.
(This is different from Ledger, where aliases are non-recursive by default).
Aliases are applied in the following order:

1. alias directives, most recently seen first (recent directives take precedence over earlier ones; directives not yet seen are ignored)
2. alias options, in the order they appear on the command line

#### end aliases

You can clear (forget) all currently defined aliases with the `end aliases` directive:

```journal
end aliases
```

### account directive

The `account` directive predefines account names, as in Ledger and Beancount.
This may be useful for your own documentation; hledger doesn't make use of it yet.

```journal
; account ACCT
;   OPTIONAL COMMENTS/TAGS...

account assets:bank:checking
 a comment
 acct-no:12345

account expenses:food

; etc.
```

### apply account directive

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

Prior to hledger 0.28, legacy `account` and `end` spellings were also supported.

### Multi-line comments

A line containing just `comment` starts a multi-line comment, and a
line containing just `end comment` ends it. See [comments](#comments).

### commodity directive

The `commodity` directive predefines commodities (currently this is just informational),
and also it may define the display format for amounts in this commodity (overriding the automatically inferred format).

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

### Default commodity

You can set a default commodity, to be used for amounts without one.
Use the D directive with a sample amount.
The commodity (and the sample amount's display format) will be applied to all subsequent commodity-less amounts, up to the next D directive.
(Note this is different from Ledger's default commodity directive.)

### Default year

You can set a default year/month to be used for subsequent dates which don't
specify a year/month. This is a line beginning with `Y` followed by the year and optionally a month. Eg:

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
  
Y2016/01   ; change default year to 2016 and month to 01

12         ; equivalent to 2016/01/12
  expanses  1
  assets
```

### Including other files

You can pull in the content of additional journal files by writing an
include directive, like this:

```journal
include path/to/file.journal
```

If the path does not begin with a slash, it is relative to the current file.
Glob patterns (`*`) are not currently supported.

The `include` directive can only be used in journal files.
It can include journal, timeclock or timedot files, but not CSV files.

# EDITOR SUPPORT

Add-on modes exist for various text editors, to make working with journal
files easier. They add colour, navigation aids and helpful commands.
For hledger users who edit the journal file directly (the majority),
using one of these modes is quite recommended.

These were written with Ledger in mind, but also work with hledger files:

|
|----------------|----------------------------------------------------|
| Emacs          | <http://www.ledger-cli.org/3.0/doc/ledger-mode.html> |
| Vim            | <https://github.com/ledger/ledger/wiki/Getting-started> |
| Sublime Text   | <https://github.com/ledger/ledger/wiki/Using-Sublime-Text> |
| Textmate       | <https://github.com/ledger/ledger/wiki/Using-TextMate-2> |
| Text Wrangler &nbsp; | <https://github.com/ledger/ledger/wiki/Editing-Ledger-files-with-TextWrangler> |

<!-- Some related LedgerTips:
https://twitter.com/LedgerTips/status/504061626233159681
https://twitter.com/LedgerTips/status/502820400276193280
https://twitter.com/LedgerTips/status/502503912084361216
https://twitter.com/LedgerTips/status/501767602067472384
-->


