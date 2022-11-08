# Tests for hledger-ui

Manual tests for hledger-ui.
If you don't want to run them all, spot-checking just a few of them
is still much better than nothing.

Each test is a heading and a literal block containing
a command, keypress sequence, or other tester action on the first line,
followed by the expected output, or a recognisable excerpt of it.
Generally tests assume an 80x25 terminal. 

Another source of hledger-ui tests is the https://hledger.org/ui.html tutorial,
which has nice screenshots but might be less up to date.

## Shows balance sheet accounts by default, in list mode
```
$ hledger-ui -f sample.journal
────────────────── sample.journal balance sheet balances (1/4) ─────────────────
 assets:bank:checking    0                                                      
 assets:bank:saving     $1                                                      
 assets:cash           $-2                                                      
 liabilities:debts      $1                                                      
```

## `t` switches to tree mode
```
t
────────────────── sample.journal balance sheet balances (3/7) ─────────────────
 assets       $-1                                                               
  bank         $1                                                               
   checking     0                                                               
   saving      $1                                                               
  cash        $-2                                                               
 liabilities   $1                                                               
  debts        $1                                                               
                                                                                ```

## `1` clips accounts to depth 1
```
1
──────────── sample.journal balance sheet balances to depth 1 (1/2) ────────────
 assets       $-1                                                               
 liabilities   $1                                                               
```

## `RIGHT` shows assets & subaccounts' transactions (register ignores depth limit)
```
RIGHT
─────────────────────────── assets transactions (5/5) ──────────────────────────
 2008-01-01   income                     in:salary                     $1    $1
 2008-06-01   gift                       in:gifts                      $1    $2
 2008-06-02   save                       as:ba:saving, as:ba:chec..     0    $2
 2008-06-03 * eat & shop                 ex:food, ex:supplies         $-2     0
 2008-12-31 * pay off                    li:debts                     $-1   $-1  # <- selected
```

## `ESC` resets UI state to top menu screen
```
ESC
──────────────────────────────── sample.journal ────────────────────────────────
 All accounts
 Balance sheet accounts (assets, liabilities, equity)  # <- selected
 Income statement accounts (revenues, expenses)
```

## `/` sets a filter query app-wide, affecting all screens
```
/ expenses ENTER
```

## The Income statement accounts screen shows only RX accounts, now filtered by "expenses"
```
DOWN RIGHT
──────── sample.journal income statement changes matching expenses (1/2) ───────
 expenses:food      $1                                                          
 expenses:supplies  $1                                                          
```

## `RIGHT` from register screen shows transaction detail
```
RIGHT RIGHT
────────── Transaction #4 (1 of 1 matching expenses in expenses:food) ──────────
 2008-06-03 * eat & shop                                                        
     expenses:food                  $1                                          
     expenses:supplies              $1                                          
     assets:cash                   $-2                                          
```

## `?` shows help dialog over current screen; all content fits unclipped in 80x25, including the last q quit line
```
?
┌──────────────────────Help (LEFT/ESC/?/q to close help)──────────────────────┐─
│ Navigation                             Filtering                            │
│ UP/DOWN/PUP/PDN/HOME/END/k/j/C-p/C-n   /    set a filter query              │
│      move selection up/down            F    show future & periodic txns     │
│ RIGHT/l/C-f show txns, or txn detail   R    show real/all postings          │
│ LEFT/h/C-b  go back                    z    show nonzero/all amounts        │
│ ESC  cancel, or reset app state        U/P/C  show unmarked/pending/cleared │
│                                        S-DOWN /S-UP   shrink/grow period    │
│ Accounts screen                        S-RIGHT/S-LEFT next/previous period  │
│ 1234567890-+  set/adjust depth limit   T              set period to today   │
│ t  toggle accounts tree/list mode      DEL  reset filters                   │
│ H  toggle historical balance/change                                         │
│                                        Other                                │
│ Register screen                        a    add transaction (hledger add)   │
│ t  toggle subaccount txns              A    add transaction (hledger-iadd)  │
│    (and accounts tree/list mode)       B    show amounts/costs              │
│ H  toggle historical/period total      E    open editor                     │
│                                        I    toggle balance assertions       │
│ Help                                   V    show amounts/market values      │
│ ?     toggle this help                 g    reload data                     │
│ p/m/i while help is open:              C-l  redraw & recenter               │
│       show manual in pager/man/info    C-z  suspend                         │
│                                        q    quit                            │
└─────────────────────────────────────────────────────────────────────────────┘
────────── ?:help LEFT:back UP/DOWN:prev/next E:editor g:reload q:quit ─────────
```

## q with help dialog open closes it
```
q
────────── Transaction #4 (1 of 1 matching expenses in expenses:food) ──────────
 2008-06-03 * eat & shop
     expenses:food                  $1
     expenses:supplies              $1
     assets:cash                   $-2
```

## q in other cases exits the app; terminal is restored cleanly to its previous state
```
q
~/src/hledger/hledger-ui/test$ hledger-ui -f sample.journal
~/src/hledger/hledger-ui/test$ 
```

## date query at startup
```
hledger-ui -f sample.journal --register checking date:200812
──── assets:bank:checking transactions matching date:200812 in 2008-12 (1/1) ───
 2008-12-31 * pay off                     li:debts                      $-1   0  # <- selected
```

## total is now $-1
```
hledger-ui -f sample.journal --register checking date:200812 --change
──── assets:bank:checking transactions matching date:200812 in 2008-12 (1/1) ───
 2008-12-31 * pay off                    li:debts                     $-1   $-1
```

## wide content is elided as shown
```
hledger-ui -f bcexample.journal --tree --register assets
───────────────────────── Assets transactions (518/518) ────────────────────────
 2014-07-26 * ..  ..      16.00 GLD, -1515.83 USD   ..GLD, 17.00 ITOT, 6 more..
 2014-07-31 * ..  ..      2550.60 USD, 4.62 VACHR   ..GLD, 17.00 ITOT, 6 more..
 2014-08-03 * ..  ..                 -2400.00 USD   ..GLD, 17.00 ITOT, 6 more..
 ...  # <- last item selected
```

## future and forecasted txns are hidden by default
```
hledger-ui --today 2021-09-01 -f forecast.journal --register a
───────────────────────────── a transactions (1/1) ─────────────────────────────
 0000-01-01   past transaction             a                              1   1
```

## with --forecast, future ordinary txns, and forecasted txns within the default forecast period, are shown
```
hledger-ui --today 2021-09-01 -f forecast.journal --register a --forecast
───────────────────────────── a transactions (4/4) ─────────────────────────────
 2020-01-01   past transaction             a                              1   1
 2021-12-31   near future transaction      a                              2   3
 2022-01-01   near future forecast tran..  a                              3   6
 2022-02-01   near future forecast tran..  a                              3   9  # <- selected
```

## "=" is required between --forecast and its argument
```
hledger-ui --today 2021-09-01 -f forecast.journal --register a --forecast 2021
────────────────────── a transactions matching 2021 (1/0) ──────────────────────

```

## the forecast period can be specified, and this allows forecast txns to overlap ordinary transactions
```
hledger-ui --today 2021-09-01 -f forecast.journal --register a --forecast=2021
──────────────────────────── a transactions (14/14) ────────────────────────────
 2020-01-01   past transaction             a                             1    1
 2021-01-01   near future forecast tran..  a                             3    4
 2021-02-01   near future forecast tran..  a                             3    7
 2021-03-01   near future forecast tran..  a                             3   10
 2021-04-01   near future forecast tran..  a                             3   13
 2021-05-01   near future forecast tran..  a                             3   16
 2021-06-01   near future forecast tran..  a                             3   19
 2021-07-01   near future forecast tran..  a                             3   22
 2021-08-01   near future forecast tran..  a                             3   25
 2021-09-01   near future forecast tran..  a                             3   28
 2021-10-01   near future forecast tran..  a                             3   31
 2021-11-01   near future forecast tran..  a                             3   34
 2021-12-01   near future forecast tran..  a                             3   37
 2021-12-31   near future transaction      a                             2   39  # <- selected
```

## the future & forecasted txns are toggled every time (#1411)
```
press F four times
```

## future/forecasted txns are hidden
```
hledger-ui --today 2021-09-01 -f forecast.journal --register a --forecast=2021 --watch, press F once
───────────────────────────── a transactions (2/2) ─────────────────────────────
 2020-01-01   past transaction             a                              1   1
 2021-12-31   near future transaction      a                              2   3  # <- selected 

```

## forecast txns reappear (scrolled a bit), even with file modified while hidden (#1204)
```
with the above still running, touch forecast.journal, press F again
──────────────────────────── a transactions (14/14) ────────────────────────────
 2021-01-01   near future forecast tran..  a                             3    4
 2021-02-01   near future forecast tran..  a                             3    7
 2021-03-01   near future forecast tran..  a                             3   10
 2021-04-01   near future forecast tran..  a                             3   13
 2021-05-01   near future forecast tran..  a                             3   16
 2021-06-01   near future forecast tran..  a                             3   19
 2021-07-01   near future forecast tran..  a                             3   22
 2021-08-01   near future forecast tran..  a                             3   25
 2021-09-01   near future forecast tran..  a                             3   28
 2021-10-01   near future forecast tran..  a                             3   31
 2021-11-01   near future forecast tran..  a                             3   34
 2021-12-01   near future forecast tran..  a                             3   37
 2021-12-31   near future transaction      a                             2   39  # <- selected 
```

## in list mode, register of account above depth limit shows only its transactions
```
hledger-ui -f 1468.j, 2, RIGHT
───────────────────────────── a transactions (1/1) ─────────────────────────────
 2021-01-01                                a                              1   1  # <- selected 
```

## in list mode, register of account at depth limit shows its and subaccounts' transactions (#1468)
```
LEFT, DOWN, RIGHT
──────────────────────────── a:aa transactions (2/2) ───────────────────────────
 2021-01-02                              a:aa                          10    10
 2021-01-03                              a:aa:aaa                     100   110  # <- selected 
```

## current screen shows new data after reload
```
hledger-ui -f sample.journal saving, edit and change the "save" txn description and amount to "NEW" and $22, g
───────────── sample.journal account balances matching saving (1/1) ────────────
 assets:bank:saving  $22
```

## newly created screens show new data after reload
```
RIGHT
───────────── assets:bank:saving transactions matching saving (1/1) ────────────
 2008-06-02   NEW                         as:ba:saving                 $22  $22
```
