accounts\
Show account names.

_FLAGS

This command lists account names, either declared with account directives
(--declared), posted to (--used), or both (the default).
With query arguments, only matched account names and account names 
referenced by matched postings are shown.
It shows a flat list by default. With `--tree`, it uses indentation to
show the account hierarchy.
In flat mode you can add `--drop N` to omit the first few account name components.
Account names can be depth-clipped with `depth:N` or `--depth N` or `-N`.

Examples:

```shell
$ hledger accounts
assets:bank:checking
assets:bank:saving
assets:cash
expenses:food
expenses:supplies
income:gifts
income:salary
liabilities:debts
```
