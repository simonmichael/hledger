print\
Show transaction journal entries, sorted by date. 

_FLAGS

The print command displays full journal entries (transactions) from
the journal file, sorted by date
(or with `--date2`, by [secondary date](#secondary-dates)).

Amounts are shown mostly normalised to 
[commodity display style](#commodity-display-styles), 
eg the placement of commodity symbols will be consistent.
All of their decimal places are shown, as in the original journal entry
(with one alteration: in some cases trailing zeroes are added.)

Amounts are shown right-aligned within each transaction (but not across all transactions). 

Directives and inter-transaction comments are not shown, currently.
This means the print command is somewhat lossy, and if you are using it to
reformat your journal you should take care to also copy over the directives
and file-level comments.

Eg:

```shell
$ hledger print
2008/01/01 income
    assets:bank:checking            $1
    income:salary                  $-1

2008/06/01 gift
    assets:bank:checking            $1
    income:gifts                   $-1

2008/06/02 save
    assets:bank:saving              $1
    assets:bank:checking           $-1

2008/06/03 * eat & shop
    expenses:food                $1
    expenses:supplies            $1
    assets:cash                 $-2

2008/12/31 * pay off
    liabilities:debts               $1
    assets:bank:checking           $-1
```

print's output is usually a valid [hledger journal](https://hledger.org/hledger.html), and you can process it again with a second hledger command. This can be useful for certain kinds of search, eg:

```shell
# Show running total of food expenses paid from cash.
# -f- reads from stdin. -I/--ignore-assertions is sometimes needed.
$ hledger print assets:cash | hledger -f- -I reg expenses:food
```

There are some situations where print's output can become unparseable:

- [Valuation](#valuation) affects posting amounts but not [balance assertion](#balance-assertions) or [balance assignment](#balance-assignments) amounts, potentially causing those to [fail](https://github.com/simonmichael/hledger/issues/1429).
- [Auto postings](#auto-postings) can generate postings with [too many missing amounts](https://github.com/simonmichael/hledger/issues/1276).

Normally, the journal entry's explicit or implicit amount style is preserved.
For example, when an amount is omitted in the journal, it will not appear in the output.
Similarly, when a transaction price is implied but not written, it will not appear in the output.
You can use the `-x`/`--explicit` flag to make all amounts and transaction prices explicit, 
which can be useful for troubleshooting or for making your journal more readable and
robust against data entry errors.
`-x` is also implied by using any of `-B`,`-V`,`-X`,`--value`.

Note, `-x`/`--explicit` will cause postings with a multi-commodity amount
(these can arise when a multi-commodity transaction has an implicit amount)
to be split into multiple single-commodity postings, 
keeping the output parseable.

With `-B`/`--cost`, amounts with [transaction prices](https://hledger.org/hledger.html#transaction-prices)
are converted to cost using that price. This can be used for troubleshooting.

With `-m`/`--match` and a STR argument, print will show at most one transaction: the one 
one whose description is most similar to STR, and is most recent. STR should contain at
least two characters. If there is no similar-enough match, no transaction will be shown.

With `--new`, hledger prints only transactions it has not seen on a previous run.
This uses the same deduplication system as the [`import`](#import) command.
(See import's docs for details.)

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, and (experimental) `json` and `sql`.

Here's an example of print's CSV output:

```shell
$ hledger print -Ocsv
"txnidx","date","date2","status","code","description","comment","account","amount","commodity","credit","debit","posting-status","posting-comment"
"1","2008/01/01","","","","income","","assets:bank:checking","1","$","","1","",""
"1","2008/01/01","","","","income","","income:salary","-1","$","1","","",""
"2","2008/06/01","","","","gift","","assets:bank:checking","1","$","","1","",""
"2","2008/06/01","","","","gift","","income:gifts","-1","$","1","","",""
"3","2008/06/02","","","","save","","assets:bank:saving","1","$","","1","",""
"3","2008/06/02","","","","save","","assets:bank:checking","-1","$","1","","",""
"4","2008/06/03","","*","","eat & shop","","expenses:food","1","$","","1","",""
"4","2008/06/03","","*","","eat & shop","","expenses:supplies","1","$","","1","",""
"4","2008/06/03","","*","","eat & shop","","assets:cash","-2","$","2","","",""
"5","2008/12/31","","*","","pay off","","liabilities:debts","1","$","","1","",""
"5","2008/12/31","","*","","pay off","","assets:bank:checking","-1","$","1","","",""
```

- There is one CSV record per posting, with the parent transaction's fields repeated.
- The "txnidx" (transaction index) field shows which postings belong to the same transaction.
  (This number might change if transactions are reordered within the file,
  files are parsed/included in a different order, etc.)
- The amount is separated into "commodity" (the symbol) and "amount" (numeric quantity) fields.
- The numeric amount is repeated in either the "credit" or "debit" column, for convenience.
  (Those names are not accurate in the accounting sense; it just puts negative amounts under
  credit and zero or greater amounts under debit.)
