% hledger_csv(5) hledger _version_
% _author_
% _monthyear_

_web_({{
_docversionlinks_({{csv}})
}})
_man_({{

# NAME

CSV - how hledger reads CSV data, and the CSV rules file format

# DESCRIPTION

}})

hledger can read
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values)
(comma-separated value, or character-separated value) files as if they were journal files,
automatically converting each CSV record into a transaction.  (To
learn about *writing* CSV, see [CSV output](hledger.html#csv-output).)

To instruct hledger how to convert CSV records to transactions, we
must provide a CSV rules file. By default this is named like the CSV
file with a `.rules` extension added. Eg when reading `FILE.csv`,
hledger also looks for `FILE.csv.rules` in the same directory. You can
specify a different rules file with the `--rules-file` option. If a
rules file is not found, hledger will auto-create it with some example
rules, which you'll need to adjust.

The CSV rules describe the CSV data (header line, fields layout,
date format etc.), and how to construct hledger journal entries
(transactions) from it. Often there will also be a list of
conditional rules for categorising transactions based on their
descriptions.

At minimum, the rules file must identify the date and amount fields,
and often it also specifies the date format and how many header lines
there are. Here's a typical simple rules file:
```
fields       date, description, _, amount
date-format  %d/%m/%Y
skip         1
```

More examples can be found in the EXAMPLES section below.


# CSV RULES

The following kinds of rule can appear in the rules file, in any order
(`end` can appear only inside an `if` block).
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

Here are the standard hledger field/pseudo-field names. 
For more about the transaction parts they refer to, see the manual for hledger's journal format.

### Transaction field names

`date`, `date2`, `status`, `code`, `description`, `comment` can be used to form the
[transaction's](journal.html#transactions) first line.

### Posting field names

`accountN`, where N is 1 to 9, sets the Nth [posting's](journal.html#postings) account name.
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
(This also means you can't generate a transaction with just one posting.)

If the CSV has the currency symbol in a separate field, you can use
`currencyN` to prepend it to posting N's amount. `currency` with no N
affects ALL postings.

`balanceN` sets a separate [balance assertion](journal.html#balance-assertions) amount 
(or if the posting amount is left empty, a [balance assignment](journal.html#balance-assignments)).

Finally, `commentN` sets a [comment](journal.html#comments) on the Nth posting. 
Comments can also contain [tags](journal.html#tags), as usual.

See TIPS below for more about setting amounts and currency.


## `(field assignment)`

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
Patterns are case-insensitive [regular expressions](hledger.html#regular-expressions)
which try to match any part of the whole CSV record.
Note the CSV record they see is close but not identical to the one in the CSV file;
eg double quotes are removed, and the separator character is always comma.

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


## `include`

```rules
include RULESFILE
```

This includes another CSV rules file at this point, as if it were written inline. 
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


# EXAMPLES

A more complete example, generating two- or three-posting transactions
from amazon.com order history:
```csv
"Date","Type","To/From","Name","Status","Amount","Fees","Transaction ID"
"Jul 29, 2012","Payment","To","Foo.","Completed","$20.00","$0.00","16000000000000DGLNJPI1P9B8DKPVHL"
"Jul 30, 2012","Payment","To","Adapteva, Inc.","Completed","$25.00","$1.00","17LA58JSKRD4HDGLNJPI1P9B8DKPVHL"
```
```rules
# hledger CSV rules for amazon.csv

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
$ hledger -f amazon.csv print
2012/07/29 (16000000000000DGLNJPI1P9B8DKPVHL) To Foo.  ; status:Completed
    assets:amazon
    expenses:misc          $20.00

2012/07/30 (17LA58JSKRD4HDGLNJPI1P9B8DKPVHL) To Adapteva, Inc.  ; status:Completed
    assets:amazon
    expenses:misc          $25.00
    expenses:fees           $1.00

```

For more examples, see [Convert CSV files](https://github.com/simonmichael/hledger/wiki/Convert-CSV-files).


# TIPS

## Valid CSV

hledger accepts CSV conforming to [RFC 4180](https://tools.ietf.org/html/rfc4180).
When CSV values are enclosed in quotes, note:

- they must be double quotes (not single quotes)
- spaces outside the quotes are [not allowed](https://stackoverflow.com/questions/4863852/space-before-quote-in-csv-field)

## Other separator characters

With the `--separator 'CHAR'` option (experimental), hledger will expect the
separator to be CHAR instead of a comma. Ie it will read other
"Character Separated Values" formats, such as TSV (Tab Separated Values).
Note: on the command line, use a real tab character in quotes, not \t. Eg:
```shell
$ hledger -f foo.tsv --separator '	' print
```

## Reading multiple CSV files

If you use multiple `-f` options to read multiple CSV files at once,
hledger will look for a correspondingly-named rules file for each CSV file.
But if you use the `--rules-file` option, that rules file will be used for all the CSV files.

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

- or assign it to `currencyN` which adds it to the amount set with
  `amountN` only. (Note: does not affect amounts set with `amount`,
  currently).

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


