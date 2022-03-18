Here are journals/scripts for reproducing hledger's journal error messages.
They are named similarly to [hledger check][]'s checks.

In the CLI, execute them to see the error messages (`./showall`).

In Emacs with [flycheck-hledger][], 
customize flycheck-hledger-* to enable all appropriate checks,
and open the files to see how flycheck handles them.
Some files contain extra declarations to ease flycheck testing.

[hledger-check]:    https://hledger.org/hledger.html#check
[flycheck-hledger]: https://github.com/DamienCassou/flycheck-hledger 
[#1436]:            https://github.com/simonmichael/hledger/issues/1436

hledger error messages are currently quite varied in format.
Tools like flycheck-hledger parse them and need updating
whenever they change ([#1436][]).
Getting consistent high-quality errors and accurate flycheck region
highlighting, not to mention LSP support, for all of our journal
errors is a big project, but it's crowd-sourceable and any progress
brings immediate practical benefits. Here is the approximate current status:

|                   | consistent | accurate line | accurate column | flycheck detects | flycheck region |
|-------------------|------------|---------------|-----------------|------------------|-----------------|
| parseable         |            |               |                 | Y                |                 |
| parseable-dates   |            |               |                 | Y                |                 |
| parseable-regexps |            |               |                 | Y                |                 |
| balanced          |            |               |                 | Y                |                 |
| assertions        |            |               |                 | Y                |                 |
| accounts          |            |               |                 | Y                |                 |
| commodities       |            |               |                 | Y                |                 |
| payees            |            |               |                 | Y                |                 |
| ordereddates      |            |               |                 | Y                |                 |
| uniqueleafnames   |            |               |                 | Y                |                 |

Key:
- consistent: the error message follows a consistent format
- accurate line - the optimal line(s) is(are) selected
- accurate column - the optimal column(s) is(are) selected
- flycheck detects - flycheck recognises the error output, reports the error and doesn't give a "suspicious" warning
- flycheck shows region - flycheck highlights the text region containing the error
