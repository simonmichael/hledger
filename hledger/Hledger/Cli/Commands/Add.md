add\
Prompt for transactions and add them to the journal.

_FLAGS

Many hledger users edit their journals directly with a text editor, or generate them from CSV.
For more interactive data entry, there is the `add` command, 
which prompts interactively on the console for new transactions, and appends
them to the journal file (if there are multiple `-f FILE` options, the first file is used.)
Existing transactions are not changed.
This is the only hledger command that writes to the journal file.

To use it, just run `hledger add` and follow the prompts.
You can add as many transactions as you like; when you are finished,
enter `.` or press control-d or control-c to exit.

Features:

- add tries to provide useful defaults, using the most similar (by description)
  recent transaction (filtered by the query, if any) as a template.
- You can also set the initial defaults with command line arguments.
- [Readline-style edit keys](http://tiswww.case.edu/php/chet/readline/rluserman.html#SEC3)
  can be used during data entry.
- The tab key will auto-complete whenever possible - accounts,
  descriptions, dates (`yesterday`, `today`, `tomorrow`). If the input
  area is empty, it will insert the default value.
- If the journal defines a [default commodity](#default-commodity),
  it will be added to any bare numbers entered.
- A parenthesised transaction [code](#entries) may be entered following a date.
- [Comments](#comments) and tags may be entered following a description or amount.
- If you make a mistake, enter `<` at any prompt to go one step backward.
- Input prompts are displayed in a different colour when the terminal supports it.

Example (see the [tutorial](step-by-step.html#record-a-transaction-with-hledger-add) for a detailed explanation):

``` shell
$ hledger add
Adding transactions to journal file /src/hledger/examples/sample.journal
Any command line arguments will be used as defaults.
Use tab key to complete, readline keys to edit, enter to accept defaults.
An optional (CODE) may follow transaction dates.
An optional ; COMMENT may follow descriptions or amounts.
If you make a mistake, enter < at any prompt to go one step backward.
To end a transaction, enter . when prompted.
To quit, enter . at a date prompt or press control-d or control-c.
Date [2015/05/22]: 
Description: supermarket
Account 1: expenses:food
Amount  1: $10
Account 2: assets:checking
Amount  2 [$-10.0]: 
Account 3 (or . or enter to finish this transaction): .
2015/05/22 supermarket
    expenses:food             $10
    assets:checking        $-10.0

Save this transaction to the journal ? [y]: 
Saved.
Starting the next transaction (. or ctrl-D/ctrl-C to quit)
Date [2015/05/22]: <CTRL-D> $
```

On Microsoft Windows, the add command makes sure that no part of the
file path ends with a period, as it can cause data loss on that platform
(cf [#1056](https://github.com/simonmichael/hledger/issues/1056)).
