% hledger_csv(5) hledger _version_
% _author_
% _monthyear_

_man_({{
# NAME
}})

CSV - how hledger reads CSV data, and the CSV rules file format

_man_({{
# DESCRIPTION
}})

hledger can read
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values)
(Comma Separated Value/Character Separated Value) files as if they were journal files,
automatically converting each CSV record into a transaction.  (To
learn about *writing* CSV, see [CSV output](hledger.html#csv-output).)

We describe each CSV file's format with a corresponding *rules file*.
By default this is named like the CSV file with a `.rules` extension
added. Eg when reading `FILE.csv`, hledger also looks for
`FILE.csv.rules` in the same directory. You can specify a different
rules file with the `--rules-file` option. If a rules file is not
found, hledger will create a sample rules file, which you'll need to
adjust.

This file contains rules describing the CSV data (header line, fields
layout, date format etc.), and how to construct hledger journal
entries (transactions) from it. Often there will also be a list of
conditional rules for categorising transactions based on their
descriptions. Here's an overview of the CSV rules;
these are described more fully below, after the examples:

|                                           |                                                         |
|-------------------------------------------|---------------------------------------------------------|
| [**`skip`**](#skip)                       | skip one or more header lines or matched CSV records    |
| [**`fields`**](#fields)                   | name CSV fields, assign them to hledger fields          |
| [**field assignment**](#field-assignment) | assign a value to one hledger field, with interpolation |
| [**`separator`**](#separator)             | a custom field separator                                |
| [**`if`**](#if)                           | apply some rules to matched CSV records                 |
| [**`end`**](#end)                         | skip the remaining CSV records                          |
| [**`date-format`**](#date-format)         | describe the format of CSV dates                        |
| [**`newest-first`**](#newest-first)       | disambiguate record order when there's only one date    |
| [**`include`**](#include)                 | inline another CSV rules file                           |

Note, for best error messages when reading CSV files, use a `.csv`, `.tsv` or `.ssv` 
file extension or file prefix - see [File Extension](#file-extension) below.

There's an introductory [Convert CSV files](convert-csv-files.html) tutorial on hledger.org.

# EXAMPLES

Here are some sample hledger CSV rules files. See also the full collection at:\
<https://github.com/simonmichael/hledger/tree/master/examples/csv>

## Basic

At minimum, the rules file must identify the date and amount fields,
and often it also specifies the date format and how many header lines
there are. Here's a simple CSV file and a rules file for it:
```csv
Date, Description, Id, Amount
12/11/2019, Foo, 123, 10.23
```
```rules
# basic.csv.rules
skip         1
fields       date, description, _, amount
date-format  %d/%m/%Y
```
```shell
$ hledger print -f basic.csv
2019-11-12 Foo
    expenses:unknown           10.23
    income:unknown            -10.23

```
Default account names are chosen, since we didn't set them.

## Bank of Ireland

Here's a CSV with two amount fields (Debit and Credit), and a balance field,
which we can use to add balance assertions, which is not necessary but
provides extra error checking:

```csv
Date,Details,Debit,Credit,Balance
07/12/2012,LODGMENT       529898,,10.0,131.21
07/12/2012,PAYMENT,5,,126
```
```rules
# bankofireland-checking.csv.rules

# skip the header line
skip

# name the csv fields, and assign some of them as journal entry fields
fields  date, description, amount-out, amount-in, balance

# We generate balance assertions by assigning to "balance"
# above, but you may sometimes need to remove these because:
#
# - the CSV balance differs from the true balance, 
#   by up to 0.0000000000005 in my experience
#
# - it is sometimes calculated based on non-chronological ordering,
#   eg when multiple transactions clear on the same day

# date is in UK/Ireland format
date-format  %d/%m/%Y

# set the currency
currency  EUR

# set the base account for all txns
account1  assets:bank:boi:checking
```
```shell
$ hledger -f bankofireland-checking.csv print
2012-12-07 LODGMENT       529898
    assets:bank:boi:checking         EUR10.0 = EUR131.2
    income:unknown                  EUR-10.0

2012-12-07 PAYMENT
    assets:bank:boi:checking         EUR-5.0 = EUR126.0
    expenses:unknown                  EUR5.0

```
The balance assertions don't raise an error above, because we're
reading directly from CSV, but they will be checked if these entries
are imported into a journal file.

## Amazon

Here we convert amazon.com order history, and use an if block to
generate a third posting if there's a fee.
(In practice you'd probably get this data from your bank instead,
but it's an example.)

```csv
"Date","Type","To/From","Name","Status","Amount","Fees","Transaction ID"
"Jul 29, 2012","Payment","To","Foo.","Completed","$20.00","$0.00","16000000000000DGLNJPI1P9B8DKPVHL"
"Jul 30, 2012","Payment","To","Adapteva, Inc.","Completed","$25.00","$1.00","17LA58JSKRD4HDGLNJPI1P9B8DKPVHL"
```
```rules
# amazon-orders.csv.rules

# skip one header line
skip 1

# name the csv fields, and assign the transaction's date, amount and code.
# Avoided the "status" and "amount" hledger field names to prevent confusion.
fields date, _, toorfrom, name, amzstatus, amzamount, fees, code

# how to parse the date
date-format %b %-d, %Y

# combine two fields to make the description
description %toorfrom %name

# save the status as a tag
comment     status:%amzstatus

# set the base account for all transactions
account1    assets:amazon
# leave amount1 blank so it can balance the other(s).
# I'm assuming amzamount excludes the fees, don't remember

# set a generic account2
account2    expenses:misc
amount2     %amzamount
# and maybe refine it further:
#include categorisation.rules

# add a third posting for fees, but only if they are non-zero.
# Commas in the data makes counting fields hard, so count from the right instead.
# (Regex translation: "a field containing a non-zero dollar amount, 
# immediately before the 1 right-most fields")
if ,\$[1-9][.0-9]+(,[^,]*){1}$
 account3    expenses:fees
 amount3     %fees
```
```shell
$ hledger -f amazon-orders.csv print
2012-07-29 (16000000000000DGLNJPI1P9B8DKPVHL) To Foo.  ; status:Completed
    assets:amazon
    expenses:misc          $20.00

2012-07-30 (17LA58JSKRD4HDGLNJPI1P9B8DKPVHL) To Adapteva, Inc.  ; status:Completed
    assets:amazon
    expenses:misc          $25.00
    expenses:fees           $1.00

```

## Paypal

Here's a real-world rules file for (customised) Paypal CSV,
with some Paypal-specific rules, and a second rules file included:

```csv
"Date","Time","TimeZone","Name","Type","Status","Currency","Gross","Fee","Net","From Email Address","To Email Address","Transaction ID","Item Title","Item ID","Reference Txn ID","Receipt ID","Balance","Note"
"10/01/2019","03:46:20","PDT","Calm Radio","Subscription Payment","Completed","USD","-6.99","0.00","-6.99","simon@joyful.com","memberships@calmradio.com","60P57143A8206782E","MONTHLY - $1 for the first 2 Months: Me - Order 99309. Item total: $1.00 USD first 2 months, then $6.99 / Month","","I-R8YLY094FJYR","","-6.99",""
"10/01/2019","03:46:20","PDT","","Bank Deposit to PP Account ","Pending","USD","6.99","0.00","6.99","","simon@joyful.com","0TU1544T080463733","","","60P57143A8206782E","","0.00",""
"10/01/2019","08:57:01","PDT","Patreon","PreApproved Payment Bill User Payment","Completed","USD","-7.00","0.00","-7.00","simon@joyful.com","support@patreon.com","2722394R5F586712G","Patreon* Membership","","B-0PG93074E7M86381M","","-7.00",""
"10/01/2019","08:57:01","PDT","","Bank Deposit to PP Account ","Pending","USD","7.00","0.00","7.00","","simon@joyful.com","71854087RG994194F","Patreon* Membership","","2722394R5F586712G","","0.00",""
"10/19/2019","03:02:12","PDT","Wikimedia Foundation, Inc.","Subscription Payment","Completed","USD","-2.00","0.00","-2.00","simon@joyful.com","tle@wikimedia.org","K9U43044RY432050M","Monthly donation to the Wikimedia Foundation","","I-R5C3YUS3285L","","-2.00",""
"10/19/2019","03:02:12","PDT","","Bank Deposit to PP Account ","Pending","USD","2.00","0.00","2.00","","simon@joyful.com","3XJ107139A851061F","","","K9U43044RY432050M","","0.00",""
"10/22/2019","05:07:06","PDT","Noble Benefactor","Subscription Payment","Completed","USD","10.00","-0.59","9.41","noble@bene.fac.tor","simon@joyful.com","6L8L1662YP1334033","Joyful Systems","","I-KC9VBGY2GWDB","","9.41",""
```

```rules
# paypal-custom.csv.rules

# Tips:
# Export from Activity -> Statements -> Custom -> Activity download
# Suggested transaction type: "Balance affecting"
# Paypal's default fields in 2018 were:
# "Date","Time","TimeZone","Name","Type","Status","Currency","Gross","Fee","Net","From Email Address","To Email Address","Transaction ID","Shipping Address","Address Status","Item Title","Item ID","Shipping and Handling Amount","Insurance Amount","Sales Tax","Option 1 Name","Option 1 Value","Option 2 Name","Option 2 Value","Reference Txn ID","Invoice Number","Custom Number","Quantity","Receipt ID","Balance","Address Line 1","Address Line 2/District/Neighborhood","Town/City","State/Province/Region/County/Territory/Prefecture/Republic","Zip/Postal Code","Country","Contact Phone Number","Subject","Note","Country Code","Balance Impact"
# This rules file assumes the following more detailed fields, configured in "Customize report fields":
# "Date","Time","TimeZone","Name","Type","Status","Currency","Gross","Fee","Net","From Email Address","To Email Address","Transaction ID","Item Title","Item ID","Reference Txn ID","Receipt ID","Balance","Note"

fields date, time, timezone, description_, type, status_, currency, grossamount, feeamount, netamount, fromemail, toemail, code, itemtitle, itemid, referencetxnid, receiptid, balance, note

skip  1

date-format  %-m/%-d/%Y

# ignore some paypal events
if
In Progress
Temporary Hold
Update to 
 skip

# add more fields to the description
description %description_ %itemtitle 

# save some other fields as tags
comment  itemid:%itemid, fromemail:%fromemail, toemail:%toemail, time:%time, type:%type, status:%status_

# convert to short currency symbols
# Note: in conditional block regexps, the line of csv being matched is
# a synthetic one: the unquoted field values, with commas between them.
if ,USD,
 currency $
if ,EUR,
 currency E
if ,GBP,
 currency P

# generate postings

# the first posting will be the money leaving/entering my paypal account
# (negative means leaving my account, in all amount fields)
account1 assets:online:paypal
amount1  %netamount

# the second posting will be money sent to/received from other party
# (account2 is set below)
amount2  -%grossamount

# if there's a fee (9th field), add a third posting for the money taken by paypal.
# TODO: This regexp fails when fields contain a comma (generates a third posting with zero amount)
if ^([^,]+,){8}[^0]
 account3 expenses:banking:paypal
 amount3  -%feeamount
 comment3 business:

# choose an account for the second posting

# override the default account names:
# if amount (8th field) is positive, it's income (a debit)
if ^([^,]+,){7}[0-9]
 account2 income:unknown
# if negative, it's an expense (a credit)
if ^([^,]+,){7}-
 account2 expenses:unknown

# apply common rules for setting account2 & other tweaks
include common.rules

# apply some overrides specific to this csv

# Transfers from/to bank. These are usually marked Pending, 
# which can be disregarded in this case.
if 
Bank Account
Bank Deposit to PP Account
 description %type for %referencetxnid %itemtitle
 account2 assets:bank:wf:pchecking
 account1 assets:online:paypal

# Currency conversions
if Currency Conversion
 account2 equity:currency conversion
```

```rules
# common.rules

if
darcs
noble benefactor
 account2 revenues:foss donations:darcshub
 comment2 business:

if
Calm Radio
 account2 expenses:online:apps

if
electronic frontier foundation
Patreon
wikimedia
Advent of Code
 account2 expenses:dues

if Google
 account2 expenses:online:apps
 description google | music

```

```shell
$ hledger -f paypal-custom.csv  print
2019-10-01 (60P57143A8206782E) Calm Radio MONTHLY - $1 for the first 2 Months: Me - Order 99309. Item total: $1.00 USD first 2 months, then $6.99 / Month  ; itemid:, fromemail:simon@joyful.com, toemail:memberships@calmradio.com, time:03:46:20, type:Subscription Payment, status:Completed
    assets:online:paypal          $-6.99 = $-6.99
    expenses:online:apps           $6.99

2019-10-01 (0TU1544T080463733) Bank Deposit to PP Account for 60P57143A8206782E  ; itemid:, fromemail:, toemail:simon@joyful.com, time:03:46:20, type:Bank Deposit to PP Account, status:Pending
    assets:online:paypal               $6.99 = $0.00
    assets:bank:wf:pchecking          $-6.99

2019-10-01 (2722394R5F586712G) Patreon Patreon* Membership  ; itemid:, fromemail:simon@joyful.com, toemail:support@patreon.com, time:08:57:01, type:PreApproved Payment Bill User Payment, status:Completed
    assets:online:paypal          $-7.00 = $-7.00
    expenses:dues                  $7.00

2019-10-01 (71854087RG994194F) Bank Deposit to PP Account for 2722394R5F586712G Patreon* Membership  ; itemid:, fromemail:, toemail:simon@joyful.com, time:08:57:01, type:Bank Deposit to PP Account, status:Pending
    assets:online:paypal               $7.00 = $0.00
    assets:bank:wf:pchecking          $-7.00

2019-10-19 (K9U43044RY432050M) Wikimedia Foundation, Inc. Monthly donation to the Wikimedia Foundation  ; itemid:, fromemail:simon@joyful.com, toemail:tle@wikimedia.org, time:03:02:12, type:Subscription Payment, status:Completed
    assets:online:paypal             $-2.00 = $-2.00
    expenses:dues                     $2.00
    expenses:banking:paypal      ; business:

2019-10-19 (3XJ107139A851061F) Bank Deposit to PP Account for K9U43044RY432050M  ; itemid:, fromemail:, toemail:simon@joyful.com, time:03:02:12, type:Bank Deposit to PP Account, status:Pending
    assets:online:paypal               $2.00 = $0.00
    assets:bank:wf:pchecking          $-2.00

2019-10-22 (6L8L1662YP1334033) Noble Benefactor Joyful Systems  ; itemid:, fromemail:noble@bene.fac.tor, toemail:simon@joyful.com, time:05:07:06, type:Subscription Payment, status:Completed
    assets:online:paypal                       $9.41 = $9.41
    revenues:foss donations:darcshub         $-10.00  ; business:
    expenses:banking:paypal                    $0.59  ; business:

```


# CSV RULES

The following kinds of rule can appear in the rules file, in any order.
Blank lines and lines beginning with `#` or `;` are ignored.


## `skip`

```rules
skip N
```
The word "skip" followed by a number (or no number, meaning 1)
tells hledger to ignore this many non-empty lines preceding the CSV data.
(Empty/blank lines are skipped automatically.)
You'll need this whenever your CSV data contains header lines.

It also has a second purpose: it can be used inside [if blocks](#if)
to ignore certain CSV records (described below).


## `fields`

```rules
fields FIELDNAME1, FIELDNAME2, ...
```
A fields list (the word "fields" followed by comma-separated field
names) is the quick way to assign CSV field values to hledger fields.
It does two things:

1. it names the CSV fields. 
   This is optional, but can be convenient later for interpolating them.

2. when you use a standard hledger field name,
   it assigns the CSV value to that part of the hledger transaction.

Here's an example that says 
"use the 1st, 2nd and 4th fields as the transaction's date, description and amount;
name the last two fields for later reference; and ignore the others":
```rules
fields date, description, , amount, , , somefield, anotherfield
```

Field names may not contain whitespace. 
Fields you don't care about can be left unnamed.
Currently there must be least two items (there must be at least one comma).

Note, always use comma in the fields list, even if your CSV uses
[another separator character](#separator).

Here are the standard hledger field/pseudo-field names. 
For more about the transaction parts they refer to, see the manual for hledger's journal format.

### Transaction field names

`date`, `date2`, `status`, `code`, `description`, `comment` can be used to form the
[transaction's](journal.html#transactions) first line.

### Posting field names

`accountN`, where N is 1 to 9, generates a
[posting](journal.html#postings), with that account name.
Most often there are two postings, so you'll want to set `account1` and `account2`.
If a posting's account name is left unset but its amount is set, 
a default account name will be chosen (like expenses:unknown or income:unknown).

`amountN` sets posting N's amount. Or, `amount` with no N sets posting
1's. If the CSV has debits and credits in separate fields, use
`amountN-in` and `amountN-out` instead. Or `amount-in` and
`amount-out` with no N for posting 1.

For convenience and backwards compatibility, if you set the amount of
posting 1 only, a second posting with the negative amount will be
generated automatically.
(Unless the account name is parenthesised indicating an 
[unbalanced posting](journal.html#virtual-postings).)

If the CSV has the currency symbol in a separate field, you can use
`currencyN` to prepend it to posting N's amount. `currency` with no N
affects ALL postings.

`balanceN` sets a [balance assertion](journal.html#balance-assertions) amount 
(or if the posting amount is left empty, a [balance assignment](journal.html#balance-assignments)).
You may need to adjust this with the [`balance-type` rule](#balance-type).

Finally, `commentN` sets a [comment](journal.html#comments) on the Nth posting. 
Comments can also contain [tags](journal.html#tags), as usual.

See TIPS below for more about setting amounts and currency.


## field assignment

```rules
HLEDGERFIELDNAME FIELDVALUE
```

Instead of or in addition to a [fields list](#fields), you can use a
"field assignment" rule to set the value of a single hledger field, by
writing its name (any of the standard hledger field names above)
followed by a text value.
The value may contain interpolated CSV fields, 
referenced by their 1-based position in the CSV record (`%N`),
or by the name they were given in the fields list (`%CSVFIELDNAME`).
Some examples:
```rules
# set the amount to the 4th CSV field, with " USD" appended
amount %4 USD

# combine three fields to make a comment, containing note: and date: tags
comment note: %somefield - %anotherfield, date: %1
```
Interpolation strips outer whitespace (so a CSV value like `" 1 "`
becomes `1` when interpolated)
([#1051](https://github.com/simonmichael/hledger/issues/1051)).
See TIPS below for more about referencing other fields.

## `separator`

You can use the `separator` directive to read other kinds of
character-separated data. Eg to read SSV (Semicolon Separated Values), use:
```
separator ;
```

The separator directive accepts exactly one single byte character as a
separator. To specify whitespace characters, you may use the special
words `TAB` or `SPACE`. Eg to read TSV (Tab Separated Values), use:
```
separator TAB
```

See also: [File Extension](#file-extension).

## `if`

```rules
if PATTERN
 RULE

if
PATTERN
PATTERN
PATTERN
 RULE
 RULE
```

Conditional blocks ("if blocks") are a block of rules that are applied
only to CSV records which match certain patterns. They are often used
for customising account names based on transaction descriptions.

A single pattern can be written on the same line as the "if";
or multiple patterns can be written on the following lines, non-indented.
Multiple patterns are OR'd (any one of them can match).
Patterns are case-insensitive regular expressions
which try to match anywhere within the whole CSV record
(POSIX extended regular expressions with some additions, see https://hledger.org/hledger.html#regular-expressions).
Note the CSV record they see is close to, but not identical to, the one in the CSV file;
enclosing double quotes will be removed, and the separator character is always comma.

It's not yet easy to match within a specific field.
If the data does not contain commas, you can hack it with a regular expression like:
```rules
# match "foo" in the fourth field
if ^([^,]*,){3}foo
```

After the patterns there should be one or more rules to apply, all
indented by at least one space. Three kinds of rule are allowed in
conditional blocks:

- [field assignments](#field-assignment) (to set a hledger field)
- [skip](#skip) (to skip the matched CSV record)
- [end](#end) (to skip all remaining CSV records).

Examples:
```rules
# if the CSV record contains "groceries", set account2 to "expenses:groceries"
if groceries
 account2 expenses:groceries
```
```rules
# if the CSV record contains any of these patterns, set account2 and comment as shown
if
monthly service fee
atm transaction fee
banking thru software
 account2 expenses:business:banking
 comment  XXX deductible ? check it
```


## `end`

This rule can be used inside [if blocks](#if) (only), to make hledger stop
reading this CSV file and move on to the next input file, or to command execution. 
Eg:
```rules
# ignore everything following the first empty record
if ,,,,
 end
```


## `date-format`

```rules
date-format DATEFMT
```
This is a helper for the `date` (and `date2`) fields.
If your CSV dates are not formatted like `YYYY-MM-DD`, `YYYY/MM/DD` or `YYYY.MM.DD`,
you'll need to add a date-format rule describing them with a
strptime date parsing pattern, which must parse the CSV date value completely.
Some examples:
``` rules
# MM/DD/YY
date-format %m/%d/%y
```
``` rules
# D/M/YYYY
# The - makes leading zeros optional.
date-format %-d/%-m/%Y
```
``` rules
# YYYY-Mmm-DD
date-format %Y-%h-%d
```
``` rules
# M/D/YYYY HH:MM AM some other junk
# Note the time and junk must be fully parsed, though only the date is used.
date-format %-m/%-d/%Y %l:%M %p some other junk
```
For the supported strptime syntax, see:\
<https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime>


## `newest-first`

hledger always sorts the generated transactions by date.
Transactions on the same date should appear in the same order as their CSV records,
as hledger can usually auto-detect whether the CSV's normal order is oldest first or newest first.
But if all of the following are true:

- the CSV might sometimes contain just one day of data (all records having the same date)
- the CSV records are normally in reverse chronological order (newest at the top)
- and you care about preserving the order of same-day transactions

then, you should add the `newest-first` rule as a hint. Eg:
```rules
# tell hledger explicitly that the CSV is normally newest first
newest-first
```


## `include`

```rules
include RULESFILE
```

This includes the contents of another CSV rules file at this point.
`RULESFILE` is an absolute file path or a path relative to the current file's directory.
This can be useful for sharing common rules between several rules files, eg:
```rules
# someaccount.csv.rules

## someaccount-specific rules
fields   date,description,amount
account1 assets:someaccount
account2 expenses:misc

## common rules
include categorisation.rules
```


## `balance-type`

Balance assertions generated by [assigning to balanceN](#posting-field-names)
are of the simple `=` type by default,
which is a [single-commodity](https://hledger.org/journal.html#assertions-and-commodities),
[subaccount-excluding](https://hledger.org/journal.html#assertions-and-subaccounts) assertion.
You may find the subaccount-including variants more useful, 
eg if you have created some virtual subaccounts of checking to help with budgeting.
You can select a different type of assertion with the `balance-type` rule:
```rules
# balance assertions will consider all commodities and all subaccounts
balance-type ==*
```

Here are the balance assertion types for quick reference:
```
=    single commodity, exclude subaccounts
=*   single commodity, include subaccounts
==   multi commodity,  exclude subaccounts
==*  multi commodity,  include subaccounts
```

# TIPS

## Rapid feedback

It's a good idea to get rapid feedback while creating/troubleshooting CSV rules.
Here's a good way, using entr from http://eradman.com/entrproject :
```shell
$ ls foo.csv* | entr bash -c 'echo ----; hledger -f foo.csv print desc:SOMEDESC'
```
A desc: query (eg) is used to select just one, or a few, transactions of interest.
"bash -c" is used to run multiple commands, so we can echo a separator each time
the command re-runs, making it easier to read the output.

## Valid CSV

hledger accepts CSV conforming to [RFC 4180](https://tools.ietf.org/html/rfc4180).
When CSV values are enclosed in quotes, note:

- they must be double quotes (not single quotes)
- spaces outside the quotes are [not allowed](https://stackoverflow.com/questions/4863852/space-before-quote-in-csv-field)

## File Extension

CSV ("Character Separated Values") files
should be named with one of these filename extensions: `.csv`, `.ssv`, `.tsv`.
Or, the file path should be prefixed with one of `csv:`, `ssv:`, `tsv:`.
This helps hledger identify the format and show the right error messages.
For example:
```shell
$ hledger -f foo.ssv print
```
or:
```
$ cat foo | hledger -f ssv:- foo
```
More about this: [Input files](hledger.html#input-files) in the hledger manual.

## Reading multiple CSV files

If you use multiple `-f` options to read multiple CSV files at once,
hledger will look for a correspondingly-named rules file for each CSV
file. But if you use the `--rules-file` option, that rules file will
be used for all the CSV files.

## Valid transactions

After reading a CSV file, hledger post-processes and validates the
generated journal entries as it would for a journal file - balancing
them, applying balance assignments, and canonicalising amount styles.
Any errors at this stage will be reported in the usual way, displaying
the problem entry.

There is one exception: balance assertions, if you have generated
them, will not be checked, since normally these will work only when
the CSV data is part of the main journal. If you do need to check
balance assertions generated from CSV right away, pipe into another hledger:
```shell
$ hledger -f file.csv print | hledger -f- print
```

## Deduplicating, importing

When you download a CSV file periodically, eg to get your latest bank
transactions, the new file may overlap with the old one, containing
some of the same records.

The [import](hledger.html#import) command will (a) detect the new
transactions, and (b) append just those transactions to your main
journal. It is idempotent, so you don't have to remember how many
times you ran it or with which version of the CSV.
(It keeps state in a hidden `.latest.FILE.csv` file.)
This is the easiest way to import CSV data. Eg:
```shell
# download the latest CSV files, then run this command.
# Note, no -f flags needed here.
$ hledger import *.csv [--dry]
```
This method works for most CSV files.
(Where records have a stable chronological order, and new records appear only at the new end.)

A number of other tools and workflows, hledger-specific and otherwise,
exist for converting, deduplicating, classifying and managing CSV
data. See:

- <https://hledger.org> -> sidebar -> real world setups
- <https://plaintextaccounting.org> -> data import/conversion

## Setting amounts

A posting amount can be set in one of these ways:

- by assigning (with a fields list or field assigment) to
  `amountN` (posting N's amount) or `amount` (posting 1's amount)

- by assigning to `amountN-in` and `amountN-out` (or `amount-in` and `amount-out`).
  For each CSV record, whichever of these has a non-zero value will be used, with appropriate sign. 
  If both contain a non-zero value, this may not work.

- by assigning to `balanceN` (or `balance`) instead of the above,
  setting the amount indirectly via a 
  [balance assignment](journal.html#balance-assignments).
  If you do this the default account name may be wrong, so you should set that explicitly.

There is some special handling for an amount's sign:

- If an amount value is parenthesised, it will be de-parenthesised and sign-flipped.
- If an amount value begins with a double minus sign, those cancel out and are removed.
- If an amount value begins with a plus sign, that will be removed

## Setting currency/commodity

If the currency/commodity symbol is included in the  CSV's amount field(s),
you don't have to do anything special.

If the currency is provided as a separate CSV field, you can either:

- assign that to `currency`, which adds it to all posting amounts. The
  symbol will prepended to the amount quantity (on the left side). If
  you write a trailing space after the symbol, there will be a space
  between symbol and amount (an exception to the usual whitespace
  stripping).

- or assign it to `currencyN` which adds it to posting N's amount only.

- or for more control, construct the amount from symbol and quantity
  using field assignment, eg:

   ```rules
   fields date,description,currency,quantity
   # add currency symbol on the right:
   amount %quantity %currency
   ```

## Referencing other fields

In field assignments, you can interpolate only CSV fields, not hledger
fields. In the example below, there's both a CSV field and a hledger
field named amount1, but %amount1 always means the CSV field, not
the hledger field:

```rules
# Name the third CSV field "amount1"
fields date,description,amount1

# Set hledger's amount1 to the CSV amount1 field followed by USD
amount1 %amount1 USD

# Set comment to the CSV amount1 (not the amount1 assigned above)
comment %amount1
```

Here, since there's no CSV amount1 field, %amount1 will produce a literal "amount1":
```rules
fields date,description,csvamount
amount1 %csvamount USD
# Can't interpolate amount1 here
comment %amount1
```

When there are multiple field assignments to the same hledger field,
only the last one takes effect. Here, comment's value will be be B,
or C if "something" is matched, but never A:
```rules
comment A
comment B
if something
 comment C
```

## How CSV rules are evaluated

Here's how to think of CSV rules being evaluated (if you really need to).
First,

- `include` - all includes are inlined, from top to bottom, depth
  first. (At each include point the file is inlined and scanned for
  further includes, recursively, before proceeding.)

Then "global" rules are evaluated, top to bottom. If a rule is
repeated, the last one wins:

- `skip` (at top level)
- `date-format`
- `newest-first`
- `fields` - names the CSV fields, optionally sets up initial assignments to hledger fields

Then for each CSV record in turn:

- test all `if` blocks. If any of them contain a `end` rule, skip all remaining CSV records.
  Otherwise if any of them contain a `skip` rule, skip that many CSV records.
  If there are multiple matched `skip` rules, the first one wins.
- collect all field assignments at top level and in matched `if` blocks.
  When there are multiple assignments for a field, keep only the last one.
- compute a value for each hledger field - either the one that was assigned to it
  (and interpolate the %CSVFIELDNAME references), or a default
- generate a synthetic hledger transaction from these values.

This is all part of the CSV reader, one of several readers hledger can
use to parse input files. When all files have been read successfully,
the transactions are passed as input to whichever hledger command the
user specified.
