hledger error messages are currently quite varied in format.
Tools like flycheck-hledger parse them and need updating
whenever they change ([#1436][]).
Getting consistent high-quality errors and accurate flycheck region
highlighting, not to mention LSP support, for all of our journal
errors is a big project, but it's crowd-sourceable and every bit of
progress brings immediate practical benefits.

Here are journals/scripts for reproducing hledger's journal error messages.
They are named similarly to [hledger check][]'s checks.
In the CLI, execute them to see the error messages (`./showall`).

In Emacs with [flycheck-hledger][],
customize flycheck-hledger-* to enable all appropriate checks,
and open the files to see how flycheck handles them.
Some files contain extra declarations to ease flycheck testing.

[hledger check]:    https://hledger.org/hledger.html#check
[flycheck-hledger]: https://github.com/DamienCassou/flycheck-hledger
[flycheck-hledger-10]: https://github.com/DamienCassou/flycheck-hledger/pull/10
[#1436]:            https://github.com/simonmichael/hledger/issues/1436

## Goals

- [x] phase 1: update flycheck to detect journal errors of current hledger release (and keep a branch updated to detect errors of latest hledger master)
- [x] phase 2: survey/document current journal errors & status
- [x] phase 3: pick a new standard format
- [ ] **phase 4: implement standard format for all**
- [ ] phase 5: implement accurate lines for all
- [ ] phase 6: implement accurate columns for all
- [ ] phase 7: implement useful highlighted excerpts for all
- [ ] phase 8: implement accurate flycheck region for all
- [ ] phase 9: do likewise for timeclock errors
- [ ] phase 10: do likewise for timedot errors
- [ ] phase 11: do likewise for csv errors
- [ ] phase 12: do likewise for other command line errors
- [x] phase 13: decide/add error ids/explanations/web pages ? not needed
- [ ] phase 14: support Language Server Protocol & Visual Code

## Current status

Here is the current status
(hledger 1.25, flycheck 87b275b9):

|                          | std format | accurate line(s) | accurate column(s) | visual | flycheck detects | flycheck region |
|--------------------------|------------|------------------|--------------------|--------|------------------|-----------------|
| parseable                | Y          | Y                | Y                  | YY     | Y                | Y               |
| parseable-dates          | Y          | Y                | Y                  | YY     | Y                | Y               |
| parseable-regexps        | Y          | Y                | Y                  | YY     | Y                | Y               |
| balanced                 |            | Y                | -                  | Y      | Y                |                 |
| balancednoautoconversion |            | Y                | -                  | Y      | Y                |                 |
| assertions               |            | Y                |                    | Y      | Y                | Y               |
| accounts                 | Y          |                  |                    | Y      | Y                |                 |
| commodities              |            |                  |                    | Y      | Y                |                 |
| payees                   |            |                  |                    | Y      | Y                | Y               |
| ordereddates             |            |                  |                    | Y      | Y                | Y               |
| uniqueleafnames          |            |                  |                    | Y      | Y                |                 |

Key:
- std format - the error message follows a standard format
  (location on first line, megaparsec-like excerpt, description).
- accurate line - the optimal line(s) is(are) selected
- accurate column - the optimal column(s) is(are) selected
- visual - the CLI error message shows a relevant excerpt (Y), ideally with the error highlighted (YY)
- flycheck detects - flycheck recognises the error output, reports the error and doesn't give a "suspicious" warning
- flycheck region - flycheck highlights a reasonably accurate text region containing the error

## Preferred error format

Here is our preferred error message layout for now:
```
hledger: Error: FILE:LOCATION:
EXCERPT
SUMMARY
[DETAILS]
```

Notes (see also [#1436][]):

- the "hledger: " prefix could be dropped later with a bit more effort
- includes the word "Error" and the error position on line 1
- FILE is the file path
- LOCATION is `LINE[-ENDLINE][:COLUMN[-ENDCOLUMN]]`
- EXCERPT is a short visual snippet whenever possible, with the error region highlighted, line numbers, and colour when supported. This section must be easy for flycheck to ignore.
- SUMMARY is a one line description/explanation of the problem. 
  These are currently dynamic, they can include helpful contextual info.
  ShellCheck uses static summaries.
- DETAILS is optional additional details/advice when needed.
- this layout is based on megaparsec's
- for comparison: rustc puts summary on line 1 and location on line 2:
  ```
  Error[ID]: SUMMARY
  at FILE:LOCATION
  EXCERPT
  [DETAILS]
  ```
- try https://github.com/mesabloo/diagnose later

## Current journal errors

<!-- to update: erase the below then C-u M-! ./showall -->
hledger 1.25.99-gb8d78661e-20220422 error messages, last updated 2022-04-22:

### parseable
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/parseable.j:3:2:
  |
3 | 1
  |  ^
unexpected newline
expecting date separator or digit

```

### parseable-dates
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/parseable-dates.j:3:1:
  |
3 | 2022/1/32
  | ^^^^^^^^^
well-formed but invalid date: 2022/1/32

```

### parseable-regexps
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./parseable-regexps.j:3:8:
  |
3 | alias /(/ = a
  |        ^
this regular expression could not be compiled: (

```

### balanced
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./balanced.j:3-4
could not balance this transaction:
real postings' sum should be 0 but is: 1
2022-01-01
    a               1

```

### balancednoautoconversion
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./balancednoautoconversion.j:6-8
could not balance this transaction:
real postings' sum should be 0 but is:  1 A
-1 B
2022-01-01
    a             1 A
    b            -1 B

```

### assertions
```
hledger: Error: balance assertion: /Users/simon/src/hledger/hledger/test/errors/./assertions.j:4:8
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
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/accounts.j:4:6:
  | 2022-01-01
4 |     (a)               1
  |      ^
undeclared account "a"

```

### commodities
```
hledger: Error: undeclared commodity "A"
in transaction at: /Users/simon/src/hledger/hledger/test/errors/./commodities.j:5-6

  2022-01-01
      (a)             A 1

```

### payees
```
hledger: Error: undeclared payee "p"
at: /Users/simon/src/hledger/hledger/test/errors/./payees.j:6-7

> 2022-01-01 p
      (a)             A 1

```

### ordereddates
```
hledger: Error: transaction date is out of order
at /Users/simon/src/hledger/hledger/test/errors/./ordereddates.j:10-11:

  2022-01-02 p
      (a)               1
  
> 2022-01-01 p
      (a)               1
  

```

### uniqueleafnames
```
hledger: Error: account leaf names are not unique
leaf name "c" appears in account names: "a:c", "b:c"
seen in "a:c" in transaction at: /Users/simon/src/hledger/hledger/test/errors/./uniqueleafnames.j:8-9

> 2022-01-01 p
>     (a:c)               1

```
