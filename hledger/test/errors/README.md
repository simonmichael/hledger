Here are journals for reproducing all of hledger's journal error messages.
Each has an easy command at the top for reproducing in the CLI.
Some have additional declarations to help with reproducing in Emacs with flycheck-hledger.

hledger error messages are still pretty inconsistent. 
Tools like https://github.com/DamienCassou/flycheck-hledger parse them and need updating whenever they change.
Related: https://github.com/simonmichael/hledger/issues/1436.

Getting consistent high-quality errors and accurate flycheck region
highlighting, not to mention LSP support, for all of our journal
errors is a big project.
But (once we converge on a standard format) it's quite crowd-sourceable, 
tackling each error separately, and any progress means immediate practical benefits.
Below is the approximate status of hledger's error messages and related tool support.

Key:
- consistent: the error message follows a consistent format
- accurate line - the optimal line(s) is(are) selected
- accurate column - the optimal column(s) is(are) selected
- flycheck detects - flycheck recognises the error output, reports the error and doesn't give a "suspicious" warning
- flycheck shows region - flycheck highlights the text region containing the error

|                           | consistent | accurate line | accurate column | flycheck detects | flycheck region |
|---------------------------|------------|---------------|-----------------|------------------|-----------------|
| failing-balance-assertion |            |               |                 | Y                |                 |
| invalid-date              |            |               |                 | Y                |                 |
| invalid-regex             |            |               |                 | Y                |                 |
| nonunique-leaf-names      |            |               |                 | Y                |                 |
| parse-error               |            |               |                 | Y                |                 |
| unbalanced-txn            |            |               |                 | Y                |                 |
| undeclared-acct           |            |               |                 | Y                |                 |
| undeclared-commodity      |            |               |                 | Y                |                 |
| undeclared-payee          |            |               |                 | Y                |                 |
| unordered-dates           |            |               |                 | Y                |                 |
