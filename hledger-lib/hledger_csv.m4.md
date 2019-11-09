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
(comma-separated value) files as if they were journal files,
automatically converting each CSV record into a transaction.  (To
learn about *writing* CSV, see [CSV output](hledger.html#csv-output).)

Converting CSV to transactions requires some special conversion rules.
These do several things:

- they describe the layout and format of the CSV data
- they can customize the generated journal entries (transactions) using a simple templating language
- they can add refinements based on patterns in the CSV data, eg categorizing transactions with more detailed account names.

When reading a CSV file named `FILE.csv`, hledger looks for a
conversion rules file named `FILE.csv.rules` in the same directory.
You can override this with the `--rules-file` option.
If the rules file does not exist, hledger will auto-create one with
some example rules, which you'll need to adjust.

At minimum, the rules file must identify the date and amount fields. 
It's often necessary to specify the date format, and the number of header lines to skip, also.
Eg:
```
fields date, _, _, amount
date-format  %d/%m/%Y
skip 1
```

More examples in the EXAMPLES section below.

# CSV RULES

The following kinds of rule can appear in the rules file, in any order
(except for `end` which can appear only inside a conditional block).
Blank lines and lines beginning with `#` or `;` are ignored.

## `skip`

```rules
skip N
```
The word "skip" followed by a number (or no number, meaning 1)
tells hledger to ignore this many non-empty lines preceding the CSV data.
(Empty/blank lines are skipped automatically.)
You'll need this whenever your CSV data contains header lines.

It also has a second purpose: it can be used to ignore certain CSV
records, see [conditional blocks](#if) below.

## `fields`

```rules
fields FIELDNAME1, FIELDNAME2, ...
```
A fields list ("fields" followed by one or more comma-separated field names) is the quick way to assign CSV field values to hledger fields.
It  (a) names the CSV fields, in order (names may not contain whitespace; fields you don't care about can be left unnamed),
and (b) assigns them to hledger fields if you use standard hledger field names.
Here's an example:
```rules
# use the 1st, 2nd and 4th CSV fields as the transaction's date, description and amount,
# ignore the 3rd, 5th and 6th fields,
# and name the 7th and 8th fields for later reference:
#      1     2           3  4       5 6  7          8

fields date, description, , amount1, , , somefield, anotherfield
```

Here are the standard hledger field names:

### Transaction fields

`date`, `date2`, `status`, `code`, `description`, `comment` can be used to form the
[transaction's](journal.html#transactions) first line. Only `date` is required.
(See also [date-format](#date-format) below.)

### Posting fields

`accountN`, where N is 1 to 9, sets the Nth [posting's](journal.html#postings) account name.
Most often there are two postings, so you'll want to set `account1` and `account2`.
<!-- (Often, `account1` is fixed and `account2` will be set later by a [conditional block](#if).) -->

A number of field/pseudo-field names are available for setting posting [amounts](journal.html#amounts):

- `amountN` sets posting N's amount
- `amountN-in` and `amountN-out` can be used instead, if the CSV has separate fields for debits and credits
- `currencyN` sets a currency symbol to be left-prefixed to the amount, useful if the CSV provides that as a separate field
- `balanceN` sets a (separate) [balance assertion](journal.html#balance-assertions) amount 
   (or when no posting amount is set, a [balance assignment](journal.html#balance-assignments))

If you write these with no number
(`amount`, `amount-in`, `amount-out`, `currency`, `balance`),
it means posting 1.
Also, if you set an amount for posting 1 only, 
a second posting that balances the transaction will be generated automatically.
This helps support CSV rules created before hledger 1.16.
<!-- XXX check exact behaviour, eg in three-posting example below -->

Finally, `commentN` sets a [comment](journal.html#comments) on the Nth posting. 
Comments can of course contain [tags](journal.html#tags).

## `(field assignment)`

```rules
HLEDGERFIELDNAME FIELDVALUE
```

Instead of or in addition to a [fields list](#fields), you can
assign a value to a hledger field by writing its name
(any of the standard names above) followed by a text value.
The value may contain interpolated CSV fields ([only](#referencing-other-fields)), 
referenced by their 1-based position in the CSV record (`%N`),
or by the name they were given in the fields list (`%CSVFIELDNAME`).
Eg:
```rules
# set the amount to the 4th CSV field, with " USD" appended
amount %4 USD
```
```rules
# combine three fields to make a comment, containing note: and date: tags
comment note: %somefield - %anotherfield, date: %1
```
Interpolation strips any outer whitespace, so a CSV value like `" 1 "`
becomes `1` when interpolated
([#1051](https://github.com/simonmichael/hledger/issues/1051)).

## `date-format`

```rules
date-format DATEFMT
```
This is a helper for the `date` (and `date2`) fields.
If your CSV dates are not formatted like `YYYY-MM-DD`, `YYYY/MM/DD` or `YYYY.MM.DD`,
you'll need to specify the format by writing "date-format" followed by 
a [strptime-like date parsing pattern](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime),
which must parse the date field values completely. Examples:

``` rules
# for dates like "11/06/2013":
date-format %m/%d/%Y
```

``` rules
# for dates like "6/11/2013". The - allows leading zeros to be optional.
date-format %-d/%-m/%Y
```

``` rules
# for dates like "2013-Nov-06":
date-format %Y-%h-%d
```

``` rules
# for dates like "11/6/2013 11:32 PM":
date-format %-m/%-d/%Y %l:%M %p
```

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

Conditional blocks apply one or more rules to CSV records which are
matched by any of the PATTERNs. This allows transactions to be
customised or categorised based on patterns in the data.

A single pattern can be written on the same line as the "if";
or multiple patterns can be written on the following lines, non-indented.

Patterns are case-insensitive [regular expressions](hledger.html#regular-expressions)
which try to match any part of the whole CSV record.
It's not yet possible to match within a specific field.
Note the CSV record they see is close but not identical to the one in the CSV file;
eg double quotes are removed, and the separator character becomes comma.

After the patterns, there should be one or more rules to apply, all
indented by at least one space. Three kinds of rule are allowed in
conditional blocks:

- [field assignments](#field-assignment) (to set a field's value)
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

As mentioned above, this rule can be used inside conditional blocks
(only) to cause hledger to stop reading CSV records and proceed with
command execution. Eg:
```rules
# ignore everything following the first empty record
if ,,,,
 end
```

## `include`

```rules
include RULESFILE
```

Include another CSV rules file at this point, as if it were written inline. 
`RULESFILE` is an absolute file path or a path relative to the current file's directory.

This can be useful eg for reusing common rules in several rules files:
```rules
# someaccount.csv.rules

## someaccount-specific rules
fields date,description,amount
account1 some:account
account2 some:misc

## common rules
include categorisation.rules
```

## `newest-first`

hledger always sorts the generated transactions by date.
Transactions on the same date should appear in the same order as their CSV records,
as hledger can usually auto-detect whether the CSV's normal order is oldest first or newest first.
But if all of the following are true:

- the CSV might sometimes contain just one day of data (all records having the same date)
- the CSV records are normally in reverse chronological order (newest first)
- and you care about preserving the order of same-day transactions

you should add the `newest-first` rule as a hint. Eg:
```rules
# tell hledger explicitly that the CSV is normally newest-first
newest-first
```

# EXAMPLES

A more complete example, generating three-posting transactions:
```
# hledger CSV rules for amazon.com order history

# sample:
# "Date","Type","To/From","Name","Status","Amount","Fees","Transaction ID"
# "Jul 29, 2012","Payment","To","Adapteva, Inc.","Completed","$25.00","$0.00","17LA58JSK6PRD4HDGLNJQPI1PB9N8DKPVHL"

# skip one header line
skip 1

# name the csv fields (and assign the transaction's date, amount and code)
fields date, _, toorfrom, name, amzstatus, amount1, fees, code

# how to parse the date
date-format %b %-d, %Y

# combine two fields to make the description
description %toorfrom %name

# save these fields as tags
comment     status:%amzstatus

# set the base account for all transactions
account1    assets:amazon

# flip the sign on the amount
amount      -%amount

# Put fees in a separate posting
amount3     %fees
comment3    fees
```

For more examples, see [Convert CSV files](https://github.com/simonmichael/hledger/wiki/Convert-CSV-files).

# TIPS

## Reading multiple CSV files

You can read multiple CSV files at once using multiple `-f` arguments on the command line.
hledger will look for a correspondingly-named rules file for each CSV file.
If you use the `--rules-file` option, that rules file will be used for all the CSV files.

## Deduplicating, importing

When you download a CSV file repeatedly, eg to get your latest bank
transactions, the new file may contain some of the same records as the
old one. The [print --new](hledger.html#print) command is one simple
way to detect just the new transactions. Or better still, the
[import](hledger.html#import) command appends those new transactions
to your main journal. This is the easiest way to import CSV data. Eg,
after downloading your latest CSV files:
```shell
$ hledger import *.csv [--dry]
```

## Other import methods

A number of other tools and workflows, hledger-specific and otherwise,
exist for converting, deduplicating, classifying and managing CSV data.
See:

- <https://hledger.org> -> sidebar -> real world setups
- <https://plaintextaccounting.org> -> data import/conversion

## Valid CSV

hledger accepts CSV conforming to [RFC 4180](https://tools.ietf.org/html/rfc4180).
Some things to note when values are enclosed in quotes:

- you must use double quotes (not single quotes)
- spaces outside the quotes are [not allowed](https://stackoverflow.com/questions/4863852/space-before-quote-in-csv-field)

## Other separator characters

With the `--separator 'CHAR'` option, hledger will expect the
separator to be CHAR instead of a comma. Ie it will read other
"Character Separated Values" formats, such as TSV (Tab Separated Values).
Note: on the command line, use a real tab character in quotes, not \t. Eg:
```shell
$ hledger -f foo.tsv --separator '	' print
```
(Experimental.)

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

   ```
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

## Valid transactions

hledger currently does not post-process and validate transactions
generated from CSV as thoroughly as transactions read from a journal
file. This means that if your rules are wrong, you can generate invalid
transactions. Or, amounts may not be displayed with a canonical
display style.

So when setting up or adjusting CSV rules, you should check your
results visually with the print command. You can also pipe the output
through hledger once more to fully validate and canonicalise it:

```shell
$ hledger -f some.csv print | hledger -f- print -I
```

(The -I/--ignore-assertions flag disables balance assertion checks,
usually needed when re-parsing print output.)
