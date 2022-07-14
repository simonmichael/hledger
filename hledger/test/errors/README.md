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
- [x] phase 4: implement standard format for all
- [x] phase 5: implement accurate lines for all
- [x] phase 6: implement accurate columns for all  [where possible; we currently do not save the position of every part of the transaction, so most errors do not report columns]
- [x] phase 7: implement useful highlighted excerpts for all  [we show imperfect but useful highlighted regions]
- [x] phase 8: implement accurate flycheck region for all  [flycheck-detected regions are imperfect but useful]
- [ ] phase 9: do likewise for timeclock errors
- [ ] phase 10: do likewise for timedot errors
- [ ] phase 11: do likewise for csv errors
- [ ] phase 12: do likewise for other command line errors
- [x] phase 13: decide/add error ids/explanations/web pages ? not needed
- [ ] phase 14: support Language Server Protocol & Visual Code

## Current status

Here is the current status
(hledger 1.26.99-gb7e6583a7-20220710, flycheck 87b275b9):

| error/check name         | std format | line | column | excerpt | flycheck |
|--------------------------|------------|------|--------|---------|----------|
| accounts                 | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| assertions               | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| balanced                 | ✓          | ✓    | -      | ✓       | ✓        |
| balancednoautoconversion | ✓          | ✓    | -      | ✓       | ✓        |
| commodities              | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| ordereddates             | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| parseable                | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| parseable-dates          | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| parseable-regexps        | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| payees                   | ✓          | ✓    | ✓      | ✓✓      | ✓        |
| uniqueleafnames          | ✓          | ✓    | ✓      | ✓✓      | ✓        |

Key:
- std format - the error message follows a standard format (location on first line, megaparsec-like excerpt, explanation)
- line       - correct line numbers are reported
- column     - useful column numbers are reported
- excerpt    - a useful excerpt is shown, ideally with the error highlighted (✓✓)
- flycheck   - the current flycheck release (or a PR branch) recognises the error and highlights a useful region

## Preferred error format

Here is our current standard error message layout. 
It is similar to the parse error messages we get from megaparsec.
(Easier to follow that than change it.):
```
hledger: Error: FILE:LOCATION:
EXCERPT
EXPLANATION
```

Notes (see also [#1436][]):

- line 1 includes "hledger" (dropping this would require some effort), the word "Error", and the error position
- FILE is the file path
- LOCATION is `LINE[-ENDLINE][:COLUMN[-ENDCOLUMN]]`
- EXCERPT is a short visual snippet whenever possible, with the error region highlighted, line numbers, and colour when supported. 
  This section must be easy for flycheck to ignore. (All lines begin with a space or a digit.)
- EXPLANATION briefly explains the problem, and suggests remedies if possible.
  It can be dynamic, showing context-sensitive info. (ShellCheck's summaries are static.)
- this layout is based on megaparsec's. For comparison, rustc puts summary on line 1 and location on line 2:
  ```
  Error[ID]: SUMMARY
  at FILE:LOCATION
  EXCERPT
  [DETAILS]
  ```
- try https://github.com/mesabloo/diagnose / https://hackage.haskell.org/package/errata / https://hackage.haskell.org/package/chapelure later

## Limitations

Here are some current limitations of hledger's error messages:

- We report only one error at a time. You have to fix or bypass the current error to see any others.

- We currently don't save enough information for perfect analysis of the original data.
  So we generally don't report perfect start/end line/column numbers;
  usually just the line number(s), sometimes with the starting column number.

- For the same reason, the excerpts shown in error messages are not the actual original data.
  Instead we show a synthetic rendering that is similar enough to be explanatory.

## Current journal errors

<!-- to update: make readme -->
<!-- GENERATED: -->
hledger 1.26.99-g32c7f6300-20220714 error messages:

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


### tcclockouttime
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./tcclockouttime.timeclock:5:1:
  | i 2022-01-01 00:01:00   
5 | o 2022-01-01 00:00:00   

This clockout time (2022-01-01 00:00:00) is earlier than the previous clockin.
Please adjust it to be later than 2022-01-01 00:01:00.
```


### tcorderedactions
```
hledger: Error: /Users/simon/src/hledger/hledger/test/errors/./tcorderedactions.timeclock:8:1:
8 | i 2022-01-01 00:01:00   

Expected timeclock o entry but got i.
Only one session may be clocked in at a time, so please alternate i and o.
```

