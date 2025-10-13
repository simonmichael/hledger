## print

Show full journal entries, representing transactions.

```flags
Flags:
  -x --explicit             show all amounts explicitly
     --invert               display all amounts with reversed sign
     --locations            add tags showing file paths and line numbers
  -m --match=DESC           fuzzy search for one recent transaction with
                            description closest to DESC
     --new                  show only newer-dated transactions added in each
                            file since last run
     --no-lots              remove lot subaccounts and their balance
                            assertions
     --no-lots2             remove lot subaccounts and their costs and
                            balance assertions (can produce unbalanced entries)
     --round=TYPE           how much rounding or padding should be done when
                            displaying amounts ?
                            none - show original decimal digits,
                                   as in journal (default)
                            soft - just add or remove decimal zeros
                                   to match precision
                            hard - round posting amounts to precision
                                   (can unbalance transactions)
                            all  - also round cost amounts to precision
                                   (can unbalance transactions)
     --base-url=URLPREFIX   in html output, generate links to hledger-web,
                            with this prefix. (Usually the base url shown by
                            hledger-web; can also be relative.)
  -O --output-format=FMT    select the output format. Supported formats:
                            txt, beancount, csv, tsv, html, fods, json, sql.
  -o --output-file=FILE     write output to FILE. A file extension matching
                            one of the above formats selects that format.
```

The print command displays full journal entries (transactions) 
from the journal file, sorted by date
(or with `--date2`, by [secondary date](#secondary-dates)).

Directives and inter-transaction comments are not shown, currently.
This means the print command is somewhat lossy, and if you are using it to
reformat/regenerate your journal you should take care to also copy over 
the directives and inter-transaction comments.

Eg:

```cli
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

### print amount explicitness

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


### print alignment

Amounts are shown right-aligned within each transaction
(but not aligned across all transactions; you can achieve that with ledger-mode in Emacs). 


### print amount style

Amounts will be displayed mostly in their [commodity's display style](#commodity-display-style),
with standardised symbol placement, decimal mark, and digit group marks.
This does not apply to their decimal digits;
`print` normally shows the same decimal digits that are recorded in each journal entry.

You can override the decimal precisions with `print`'s special `--round` option (*since 1.32*).
`--round` tries to show amounts with their commodities' standard decimal precisions, increasingly strongly:

- `--round=none` show amounts with original precisions (default)
- `--round=soft` add/remove decimal zeros in amounts (except costs)
- `--round=hard` round amounts (except costs), possibly hiding significant digits
- `--round=all`  round all amounts and costs

`soft` is good for non-lossy cleanup, displaying more consistent decimals where possible, without making entries unbalanced.

`hard` or `all` can be good for stronger cleanup, when decimal rounding is wanted. 
Note rounding can produce unbalanced journal entries, perhaps requiring manual fixup.


### print parseability

Normally, print's output is a valid [hledger journal](#journal), 
which you can "pipe" to a second hledger command for further processing.
This is sometimes convenient for achieving certain kinds of query
(though less needed now that queries have become more powerful):

```cli
# Show running total of food expenses paid from cash.
# -f- reads from stdin. -I/--ignore-assertions is sometimes needed.
$ hledger print assets:cash | hledger -f- -I reg expenses:food
```

But here are some things which can cause print's output to become unparseable:

- `--round` (see above) can disrupt transaction balancing.
- [Account aliases](#alias-directive) or [pivoting](#pivoting) can disrupt account names, balance assertions, or balance assignments.
- [Value reporting](#value-reporting) also can disrupt balance assertions or balance assignments.
- [Auto postings](#auto-postings) can generate too many amountless postings.
- [`--infer-costs or --infer-equity`](#equity-conversion-postings) can generate too-complex redundant costs.

### print, other features

With `-B`/`--cost`, amounts with [costs](https://hledger.org/hledger.html#costs)
are shown converted to cost.

With `--invert`, posting amounts are shown with their sign flipped.
It could be useful if you have accidentally recorded some transactions with the wrong signs.

With `--new`, print shows only transactions it has not seen on a previous run.
This uses the same deduplication system as the [`import`](#import) command.
(See import's docs for details.)

With `-m DESC`/`--match=DESC`, print shows one recent transaction
whose description is most similar to DESC.
DESC should contain at least two characters.
If there is no similar-enough match, 
no transaction will be shown and the program exit code will be non-zero.

With `--locations`, print adds the source file and line number to every transaction, as a tag.

### print output format

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `beancount` (*Added in 1.32*), `csv`, `tsv` (*Added in 1.32*), `json` and `sql`.

The `beancount` format tries to produce Beancount-compatible output, as follows:

- Transaction and postings with unmarked status are converted to cleared (`*`) status.
- Transactions' payee and note are backslash-escaped and double-quote-escaped and wrapped in double quotes.
- Transaction tags are copied to Beancount #tag format.
- Commodity symbols are converted to upper case, and a small number of currency symbols
  like `$` are converted to the corresponding currency names.
- Account name parts are capitalised and unsupported characters are replaced with `-`.
  If an account name part does not begin with a letter, or if the first part
  is not Assets, Liabilities, Equity, Income, or Expenses, an error is raised.
  (Use `--alias` options to bring your accounts into compliance.)
- An `open` directive is generated for each account used, on the earliest transaction date.

Some limitations:

- Balance assertions are removed.
- Balance assignments become missing amounts.
- Virtual and balanced virtual postings become regular postings.
- Directives are not converted.

Here's an example of print's CSV output:

```cli
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
