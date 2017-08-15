% hledger_csv(5) hledger _version_
% _author_
% _monthyear_

_web_({{
_docversionlinks_({{csv}})
_toc_
}})
_man_({{

# NAME

CSV - how hledger reads CSV data, and the CSV rules file format

# DESCRIPTION

}})

hledger can read
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) files,
converting each CSV record into a journal entry (transaction),
if you provide some conversion hints in a "rules file".
This file should be named like the CSV file with an additional `.rules` suffix (eg: `mybank.csv.rules`);
or, you can specify the file with `--rules-file PATH`.
hledger will create it if necessary, with some default rules which you'll need to adjust.
At minimum, the rules file must specify the `date` and `amount` fields.
For an example, see [Cookbook: convert CSV files](csv-import.html).

To learn about *exporting* CSV, see [CSV output](hledger.html#csv-output).


# CSV RULES

The following seven kinds of rule can appear in the rules file, in any order.
Blank lines and lines beginning with `#` or `;` are ignored.

## skip

`skip `*`N`*

Skip this number of CSV records at the beginning.
You'll need this whenever your CSV data contains header lines. Eg:
<!-- XXX -->
<!-- hledger tries to skip initial CSV header lines automatically. -->
<!-- If it guesses wrong, use this directive to skip exactly N lines. -->
<!-- This can also be used in a conditional block to ignore certain CSV records. -->
```rules
# ignore the first CSV line
skip 1
```

## date-format

`date-format `*`DATEFMT`*

When your CSV date fields are not formatted like `YYYY/MM/DD` (or `YYYY-MM-DD` or `YYYY.MM.DD`),
you'll need to specify the format.
DATEFMT is a [strptime-like date parsing pattern](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime),
which must parse the date field values completely. Examples:

``` {.rules .display-table}
# for dates like "6/11/2013":
date-format %-d/%-m/%Y
```

``` {.rules .display-table}
# for dates like "11/06/2013":
date-format %m/%d/%Y
```

``` {.rules .display-table}
# for dates like "2013-Nov-06":
date-format %Y-%h-%d
```

``` {.rules .display-table}
# for dates like "11/6/2013 11:32 PM":
date-format %-m/%-d/%Y %l:%M %p
```

## field list

`fields `*`FIELDNAME1`*, *`FIELDNAME2`*...

This (a) names the CSV fields, in order (names may not contain whitespace; uninteresting names may be left blank),
and (b) assigns them to journal entry fields if you use any of these standard field names:
`date`, `date2`, `status`, `code`, `description`, `comment`, `account1`, `account2`, `amount`, `amount-in`, `amount-out`, `currency`, `balance`.
Eg:
```rules
# use the 1st, 2nd and 4th CSV fields as the entry's date, description and amount,
# and give the 7th and 8th fields meaningful names for later reference:
#
# CSV field:
#      1     2            3 4       5 6 7          8
# entry field:
fields date, description, , amount, , , somefield, anotherfield
```

## field assignment

*`ENTRYFIELDNAME`* *`FIELDVALUE`*

This sets a journal entry field (one of the standard names above) to the given text value,
which can include CSV field values interpolated by name (`%CSVFIELDNAME`) or 1-based position (`%N`).
<!-- Whitespace before or after the value is ignored. -->
Eg:
```{.rules .display-table}
# set the amount to the 4th CSV field with "USD " prepended
amount USD %4
```
```{.rules .display-table}
# combine three fields to make a comment (containing two tags)
comment note: %somefield - %anotherfield, date: %1
```
Field assignments can be used instead of or in addition to a field list.

## conditional block

`if` *`PATTERN`*\
&nbsp;&nbsp;&nbsp;&nbsp;*`FIELDASSIGNMENTS`*...

`if`\
*`PATTERN`*\
*`PATTERN`*...\
&nbsp;&nbsp;&nbsp;&nbsp;*`FIELDASSIGNMENTS`*...

This applies one or more field assignments, only to those CSV records matched by one of the PATTERNs.
The patterns are case-insensitive regular expressions which match anywhere
within the whole CSV record (it's not yet possible to match within a
specific field).  When there are multiple patterns they can be written
on separate lines, unindented.
The field assignments are on separate lines indented by at least one space.
Examples:
```{.rules .display-table}
# if the CSV record contains "groceries", set account2 to "expenses:groceries"
if groceries
 account2 expenses:groceries
```
```{.rules .display-table}
# if the CSV record contains any of these patterns, set account2 and comment as shown
if
monthly service fee
atm transaction fee
banking thru software
 account2 expenses:business:banking
 comment  XXX deductible ? check it
```

## include

`include `*`RULESFILE`*

Include another rules file at this point. `RULESFILE` is either an absolute file path or
a path relative to the current file's directory. Eg:
```rules
# rules reused with several CSV files
include common.rules
```

## newest-first

`newest-first`

Consider adding this rule if all of the following are true: 
you might be processing just one day of data,
your CSV records are in reverse chronological order (newest first),
and you care about preserving the order of same-day transactions.
It usually isn't needed, because hledger autodetects the CSV order,
but when all CSV records have the same date it will assume they are oldest first.

# CSV TIPS

## CSV ordering

The generated [journal entries](/journal.html#transactions) will be sorted by date. 
The order of same-day entries will be preserved 
(except in the special case where you might need [`newest-first`](#newest-first), see above).

## CSV accounts

Each journal entry will have two [postings](/journal.html#postings), to `account1` and `account2` respectively.
It's not yet possible to generate entries with more than two postings.
It's conventional and recommended to use `account1` for the account whose CSV we are reading.

## CSV amounts

The `amount` field sets the [amount](/journal.html#amounts) of the `account1` posting.

If the CSV has debit/credit amounts in separate fields, assign to the `amount-in` and `amount-out` pseudo fields instead.
(Whichever one has a value will be used, with appropriate sign. If both contain a value, it may not work so well.)

If an amount value is parenthesised, it will be de-parenthesised and sign-flipped.

If an amount value begins with a double minus sign, those will cancel out and be removed.

If the CSV has the currency symbol in a separate field, 
assign that to the `currency` pseudo field to have it prepended to the amount.
Or, you can use a [field assignment](#field-assignment) to `amount` that interpolates both CSV fields
(giving more control, eg to put the currency symbol on the right).

## CSV balance assertions

If the CSV includes a running balance, you can assign that to the `balance` pseudo field;
whenever the running balance value is non-empty, 
it will be [asserted](/journal.html#balance-assertions) as the balance after the `account1` posting. 
