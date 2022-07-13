hledger error messages are currently quite varied in format.
Tools like flycheck-hledger parse them and need updating
whenever they change ([#1436][]).
Getting consistent high-quality errors and accurate flycheck region
highlighting, not to mention LSP support, for all of our journal
errors is a big project, but it's crowd-sourceable and every bit of
progress brings immediate practical benefits.

Here are journals/scripts for reproducing hledger's journal error messages.
They are named similarly to [hledger check][]'s checks.
In the CLI, execute them to see the error messages.

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
(hledger 1.26.99-gb7e6583a7-20220710, flycheck 87b275b9):

|                          | std format | line | column    | excerpt | flycheck | flycheck region |
|--------------------------|------------|------|-----------|---------|----------|-----------------|
| accounts                 | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| assertions               | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| balanced                 | ✓          | ✓    | -         | ✓       |          |                 |
| balancednoautoconversion | ✓          | ✓    | -         | ✓       |          |                 |
| commodities              | ✓          | ✓    | ✓(approx) | ✓✓      |          |                 |
| ordereddates             | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| parseable                | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| parseable-dates          | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| parseable-regexps        | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| payees                   | ✓          | ✓    | ✓         | ✓✓      |          |                 |
| uniqueleafnames          | ✓          | ✓    | ✓         | ✓✓      |          |                 |

Key:
- std format      - the error message follows a standard format (location on first line, megaparsec-like excerpt, description).
- line            - the optimal line(s) are reported
- column          - the optimal column(s) are reported
- excerpt         - a useful excerpt is shown, ideally with the error highlighted (✓✓)
- flycheck        - the current flycheck release recognises and reports the error, with no "suspicious state" warning
- flycheck region - flycheck highlights a reasonably accurate region containing the error

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
- we may show 0 for LINE or COLUMN when unknown
- EXCERPT is a short visual snippet whenever possible, with the error region highlighted, line numbers, and colour when supported. This section must be easy for flycheck to ignore.
- SUMMARY is a one line description/explanation of the problem. 
  These are currently dynamic, they can include helpful contextual info.
  ShellCheck uses static summaries.
- DETAILS is optional additional details/advice when needed.
- this layout is based on megaparsec's. For comparison, rustc puts summary on line 1 and location on line 2:
  ```
  Error[ID]: SUMMARY
  at FILE:LOCATION
  EXCERPT
  [DETAILS]
  ```
- try https://github.com/mesabloo/diagnose / https://hackage.haskell.org/package/errata / https://hackage.haskell.org/package/chapelure later

## Current journal errors

<!-- to update: erase the below then C-u M-! ./showall -->
<!-- GENERATED: -->
hledger 1.26.99-gc22e9f6cc-20220713 error messages:

### accounts
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./accounts.j:4:
  | 2022-01-01
4 |     (a)               1
  |      ^

Strict account checking is enabled, and
account "a" has not been declared.
Consider adding an account directive. Examples:

account a
account a    ; type:A  ; (L,E,R,X,C,V)
```


### assertions
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./assertions.j:4:8:
  | 2022-01-01
4 |     a               0 = 1
  |                       ^^^

This balance assertion failed.
In account:    a
and commodity: 
this balance was asserted: 1
but the actual balance is: 0
a difference of:           1

Consider viewing this account's register to troubleshoot. Eg:

hledger reg -I 'a$' cur:''
```


### balanced
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./balanced.j:3-4:
3 | 2022-01-01
  |     a               1

This transaction is unbalanced.
The real postings' sum should be 0 but is: 1
Consider adjusting this entry's amounts, or adding missing postings.
```


### balancednoautoconversion
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./balancednoautoconversion.j:6-8:
6 | 2022-01-01
  |     a             1 A
  |     b            -1 B

This multi-commodity transaction is unbalanced.
Automatic commodity conversion is not enabled.
The real postings' sum should be 0 but is: 1 A, -1 B
Consider adjusting this entry's amounts, adding missing postings,
or recording conversion price(s) with @, @@ or equity postings.
```


### commodities
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./commodities.j:6:
  | 2022-01-01
6 |     (a)             A 1
  |                     ^^^

Strict commodity checking is enabled, and
commodity "A" has not been declared.
Consider adding a commodity directive. Examples:

commodity A1000.00
commodity 1.000,00 A
```


### ordereddates
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./ordereddates.j:10:
7 | 2022-01-02 p
  |     (a)               1
 
10 | 2022-01-01 p
   | ^^^^^^^^^^
   |     (a)               1

Ordered dates checking is enabled, and this transaction's
date (2022-01-01) is out of order with the previous transaction.
Consider moving this entry into date order, or adjusting its date.
```


### parseable-dates
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./parseable-dates.j:3:1:
  |
3 | 2022/1/32
  | ^^^^^^^^^

This date is invalid, please correct it: 2022/1/32
```


### parseable-regexps
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./parseable-regexps.j:3:8:
  |
3 | alias /(/ = a
  |        ^

This regular expression is malformed, please correct it:
(
```


### parseable
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./parseable.j:3:2:
  |
3 | 1
  |  ^
unexpected newline
expecting date separator or digit
```


### payees
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./payees.j:6:
6 | 2022-01-01 p
  |            ^
  |     (a)             A 1

Strict payee checking is enabled, and
payee "p" has not been declared.
Consider adding a payee directive. Examples:

payee p
```


### uniqueleafnames
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./uniqueleafnames.j:12:
  | 2022-01-01 p
9 |     (a:c)               1
 ...
   | 2022-01-01 p
12 |     (b:c)               1
   |        ^

Checking for unique account leaf names is enabled, and
account leaf name "c" is not unique.
It appears in these account names, which are used in 2 places:
a:c
b:c

Consider changing these account names so their last parts are different.
```

