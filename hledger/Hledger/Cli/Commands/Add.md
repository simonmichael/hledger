## add

Add new transactions to a journal file, with interactive prompting.

```flags
Flags:
     --no-new-accounts      don't allow creating new accounts
```

Many hledger users edit their journals directly with a text editor, or generate them from CSV.
For more interactive data entry, there is the `add` command, 
which prompts interactively on the console for new transactions, 
and appends them to the main journal file (which should be in journal format).
Existing transactions are not changed.
This is one of the few hledger commands that writes to the journal file (see also `import`).

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
  payees/descriptions, dates (`yesterday`, `today`, `tomorrow`).
  If the input area is empty, it will insert the default value.
- A parenthesised transaction [code](#entries) may be entered following a date.
- [Comments](#transaction-comments) and tags may be entered following a description or amount.
- If you make a mistake, enter `<` at any prompt to go one step backward.
- Input prompts are displayed in a different colour when the terminal supports it.

Notes:

- If you enter a number with no commodity symbol,
  and you have declared a default commodity with a `D` directive,
  you might expect `add` to add this symbol for you.
  It does not do this; we assume that if you are using a `D` directive
  you prefer not to see the commodity symbol repeated on amounts in the journal.
- `add` creates entries in journal format; it won't work with timeclock or timedot files.

Examples:

- Record new transactions, saving to the default journal file:

  `hledger add`

- Add transactions to 2024.journal, but also load 2023.journal for completions:

  `hledger add --file 2024.journal --file 2023.journal`

- Provide answers for the first four prompts:

  `hledger add today 'best buy' expenses:supplies '$20'`

There is a detailed tutorial at <https://hledger.org/add.html>.

## add and balance assertions

Since hledger 1.43, you can add a [balance assertion](#balance-assertions) by writing `AMOUNT = BALANCE` when asked for an amount. Eg `100 = 500`.

Also, each time you enter a new amount, hledger re-checks all balance assertions in the journal
and rejects the new amount if it would make any of them fail.
You can run `add` with `-I`/`--ignore-assertions` to disable balance assertion checking.

## add and balance assignments

Since hledger 1.51, you can add a [balance assignment](#balance-assignments) by writing just `= BALANCE` when asked for an amount.
The missing amount will be calculated automatically.

`add` normally won't let you add a new posting which is dated earlier than an existing balance assignment.
(Because when `add` runs, existing balance assignments have already been calculated and converted to amounts and balance assertions.)
You can allow it by disabling balance assertion checking with `-I`.

