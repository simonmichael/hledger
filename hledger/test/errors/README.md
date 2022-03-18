Here are journals/scripts for reproducing hledger's journal error messages.
They are named similarly to [hledger check][]'s checks.

In the CLI, execute them to see the error messages (`./showall`).

In Emacs with [flycheck-hledger][], 
customize flycheck-hledger-* to enable all appropriate checks,
and open the files to see how flycheck handles them.
Some files contain extra declarations to ease flycheck testing.

[hledger check]:    https://hledger.org/hledger.html#check
[flycheck-hledger]: https://github.com/DamienCassou/flycheck-hledger 
[#1436]:            https://github.com/simonmichael/hledger/issues/1436

hledger error messages are currently quite varied in format.
Tools like flycheck-hledger parse them and need updating
whenever they change ([#1436][]).
Getting consistent high-quality errors and accurate flycheck region
highlighting, not to mention LSP support, for all of our journal
errors is a big project, but it's crowd-sourceable and any progress
brings immediate practical benefits. Here is the approximate current status:

|                          | consistent | accurate line(s) | accurate column(s) | visual | flycheck detects | flycheck region |
|--------------------------|------------|------------------|--------------------|--------|------------------|-----------------|
| parseable                |            | Y                | Y                  | YY     | Y                | Y               |
| parseable-dates          |            | Y                | Y                  | YY     | Y                | Y               |
| parseable-regexps        |            | Y                | Y                  | YY     | Y                | Y               |
| balanced                 |            | Y                | -                  | Y      | Y                |                 |
| balancednoautoconversion |            | Y                | -                  | Y      | Y                |                 |
| assertions               |            | Y                |                    | Y      | Y                | Y               |
| accounts                 |            |                  |                    | Y      | Y                |                 |
| commodities              |            |                  |                    | Y      | Y                |                 |
| payees                   |            |                  |                    | Y      | Y                | Y               |
| ordereddates             |            |                  |                    | Y      | Y                | Y               |
| uniqueleafnames          |            |                  |                    | Y      | Y                |                 |

Key:
- consistent: the error message follows a standard format
- accurate line - the optimal line(s) is(are) selected
- accurate column - the optimal column(s) is(are) selected
- visual - the CLI error message shows a relevant excerpt (Y), ideally with the error highlighted (YY)
- flycheck detects - flycheck recognises the error output, reports the error and doesn't give a "suspicious" warning
- flycheck region - flycheck highlights a reasonably accurate text region containing the error

## Current journal errors

<!-- to update: erase the below then C-u M-! ./showall -->
hledger 1.25.99-ge6bf04fce-20220316 error messages, last updated 2022-03-18:

### parseable
```
hledger: /Users/simon/src/hledger/hledger/test/errors/./parseable.j:3:2:
  |
3 | 1
  |  ^
unexpected newline
expecting date separator or digit

```

### parseable-dates
```
hledger: /Users/simon/src/hledger/hledger/test/errors/./parseable-dates.j:3:1:
  |
3 | 2022/1/32
  | ^^^^^^^^^
well-formed but invalid date: 2022/1/32

```

### parseable-regexps
```
hledger: /Users/simon/src/hledger/hledger/test/errors/./parseable-regexps.j:3:8:
  |
3 | alias /(/ = a
  |        ^
this regular expression could not be compiled: (

```

### balanced
```
hledger: /Users/simon/src/hledger/hledger/test/errors/./balanced.j:3-4
could not balance this transaction:
real postings' sum should be 0 but is: 1
2022-01-01
    a               1

```

### balancednoautoconversion
```
hledger: /Users/simon/src/hledger/hledger/test/errors/./balancednoautoconversion.j:6-8
could not balance this transaction:
real postings' sum should be 0 but is:  1 A
-1 B
2022-01-01
    a             1 A
    b            -1 B

```

### assertions
```
hledger: balance assertion: /Users/simon/src/hledger/hledger/test/errors/./assertions.j:4:8
transaction:
2022-01-01
    a               0 = 1

assertion details:
date:       2022-01-01
account:    a
commodity:  
calculated: 0
asserted:   1
difference: 1

```

### accounts
```
Error: undeclared account "a"
in transaction at: /Users/simon/src/hledger/hledger/test/errors/./accounts.j:3-4

  2022-01-01
      (a)               1

```

### commodities
```
Error: undeclared commodity "A"
in transaction at: /Users/simon/src/hledger/hledger/test/errors/./commodities.j:5-6

  2022-01-01
      (a)             A 1

```

### payees
```
Error: undeclared payee "p"
at: /Users/simon/src/hledger/hledger/test/errors/./payees.j:6-7

> 2022-01-01 p
      (a)             A 1

```

### ordereddates
```
Error: transaction date is out of order
at /Users/simon/src/hledger/hledger/test/errors/./ordereddates.j:10-11:

  2022-01-02 p
      (a)               1
  
> 2022-01-01 p
      (a)               1
  

```

### uniqueleafnames
```
Error: account leaf names are not unique
leaf name "c" appears in account names: "a:c", "b:c"
seen in "a:c" in transaction at: /Users/simon/src/hledger/hledger/test/errors/./uniqueleafnames.j:8-9

> 2022-01-01 p
>     (a:c)               1

```

