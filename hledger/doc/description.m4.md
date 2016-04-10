This is hledger’s command-line interface (there are also curses and web
interfaces). Its basic function is to read a plain text file describing
financial transactions (in accounting terms, a general journal) and
print useful reports on standard output, or export them as CSV. hledger
can also read CSV files, converting them semi-automatically to journal
format. Additionally, hledger lists other hledger-\* executables found
in the user’s \$PATH and can invoke them as subcommands.

The journal file is `~/.hledger.journal` by default, or another file path
specified by `$LEDGER_FILE`.
(This should be a real environment variable, not a shell variable.)
You can also specify a file with `-f FILE`,
or standard input with `-f-`.

Transactions are dated movements of money between two (or more) named
accounts, and are recorded with journal entries like this:

_journal_({{
2015/10/16 bought food
 expenses:food          $10
 assets:cash
}})

For more about the format, see hledger_journal(5).

Most users use a text editor to edit the journal, usually with an editor
mode such as ledger-mode for added convenience. hledger’s interactive
add command is another way to record new transactions. hledger never
changes existing transactions.

To get started, you can either save some entries like the above in
`~/.hledger.journal`, or run `hledger add` and follow the prompts. Then
try some commands like `hledger print` or `hledger balance`.
See COMMANDS and EXAMPLES below.

