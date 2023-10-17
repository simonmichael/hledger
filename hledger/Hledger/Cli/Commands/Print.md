## print

Show transaction journal entries, sorted by date. 

_FLAGS

The print command displays full journal entries (transactions) 
from the journal file, sorted by date
(or with `--date2`, by [secondary date](#secondary-dates)).

Directives and inter-transaction comments are not shown, currently.
This means the print command is somewhat lossy, and if you are using it to
reformat/regenerate your journal you should take care to also copy over 
the directives and inter-transaction comments.

Eg:

```shell
$ hledger print -f examples/sample.journal date:200806
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

```

### print explicitness

Normally, whether posting amounts are implicit or explicit is preserved.
For example, when an amount is omitted in the journal, it will not appear in the output.
Similarly, if a conversion cost is implied but not written, it will not appear in the output.

You can use the `-x`/`--explicit` flag to force explicit display of all amounts and costs.
This can be useful for troubleshooting or for making your journal more readable and
robust against data entry errors.
`-x` is also implied by using any of `-B`,`-V`,`-X`,`--value`.

The `-x`/`--explicit` flag will cause any postings with a multi-commodity amount
(which can arise when a multi-commodity transaction has an implicit amount)
to be split into multiple single-commodity postings, 
keeping the output parseable.


### print amount style

Amounts are shown right-aligned within each transaction
(but not aligned across all transactions; you can do that with ledger-mode in Emacs). 

Amounts will be (mostly) normalised to their [commodity display style](#commodity-display-styles):
their symbol placement, decimal mark, and digit group marks will be made consistent.
By default, decimal digits are shown as they are written in the journal.

With the `--round` option, `print` will try increasingly hard to
display decimal digits according to the [commodity display styles](#commodity-display-style):

- `--round=none` show amounts with original precisions (default)
- `--round=soft` add/remove decimal zeros in amounts (except costs)
- `--round=hard` round amounts (except costs), possibly hiding significant digits
- `--round=all`  round all amounts and costs

`soft` is good for non-lossy cleanup, formatting amounts more
consistently where it's safe to do so.

`hard` and `all` can cause `print` to show invalid unbalanced journal entries;
they may be useful eg for journal cleanup, with manual fixups where needed.


### print parseability

print's output is usually a valid [hledger journal](#journal), 
and you can process it again with a second hledger command. 
This can be useful for certain kinds of search
(though the same can be achieved with `expr:` queries now):

```shell
# Show running total of food expenses paid from cash.
# -f- reads from stdin. -I/--ignore-assertions is sometimes needed.
$ hledger print assets:cash | hledger -f- -I reg expenses:food
```

There are some situations where print's output can become unparseable:

- [Value reporting](#value-reporting) affects posting amounts but not [balance assertion](#balance-assertions) or [balance assignment](#balance-assignments) amounts, potentially causing those to [fail](https://github.com/simonmichael/hledger/issues/1429).
- [Auto postings](#auto-postings) can generate postings with [too many missing amounts](https://github.com/simonmichael/hledger/issues/1276).
- [Account aliases can generate bad account names](#aliases-can-generate-bad-account-names).


### print, other features

With `-B`/`--cost`, amounts with [costs](https://hledger.org/hledger.html#costs)
are shown converted to cost.

With `--new`, print shows only transactions it has not seen on a previous run.
This uses the same deduplication system as the [`import`](#import) command.
(See import's docs for details.)

With `-m DESC`/`--match=DESC`, print shows one recent transaction
whose description is most similar to DESC.
DESC should contain at least two characters.
If there is no similar-enough match, 
no transaction will be shown and the program exit code will be non-zero.


### print output format

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
