# How to use another account separator character

[Timedot format](manual.html#timedot) makes me want to use dots (`.`) for separating account components, instead of colon (`:`). 
For example, instead of `fos:hledger:timedot` I'd like to write `fos.hledger.timedot`.
We can use the powerful [account aliases](manual.html#account-aliases) feature 
to rewrite account names before hledger's account name parser sees them.

In journal files, we can use an alias directive.
Note the backslash which tells the regular expression engine it's a literal `.` not a wildcard:

```journal
# alias /REGEX/=REPLACEMENT
alias /\./=:

2008/01/01 income
    assets.bank.checking  $1
    income.salary
```
Check that subaccounts are recognised:
```shell
$ hledger -f t.journal bal --no-elide
                  $1  assets
                  $1    bank
                  $1      checking
                 $-1  income
                 $-1    salary
--------------------
                   0
```

Alias directives aren't supported in the timedot format,

```timedot
2016/2/4
fos.hledger.timedot  2
fos.ledger           1
```
so we would use the `--alias` command line option instead.
The second backslash tells the shell that's a literal backslash, not a shell escape sequence:
```shell
$ hledger --alias /\\./=: -f t.timedot bal --no-elide
                3.00  fos
                2.00    hledger
                2.00      timedot
                1.00    ledger
--------------------
                3.00
```
