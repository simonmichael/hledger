# Mockups

<div class=pagetoc>

<!-- toc -->
</div>

Mockups, draft docs and notes exploring possible future features.
See also <https://github.com/simonmichael/hledger/tree/master/doc/mockups>

## Lot terminology

Some investment-related terminology, as we use it here and in the PTA world:

- "Investment" - something whose value fluctuates while you hold it.

- Acquiring, disposing - receiving and getting rid of investments, whether by purchase,
  exchange, gift, stock options..

- Augmenting, reducing - the same thing; terminology used in Beancount docs.
  Most often the investment is an asset and acquiring/augmenting increases a positive balance,
  but with other kinds of investments (options..) it might decrease a negative balance.
  Acquiring/augmenting increases your exposure (risk), disposing/reducing reduces it.

- Lot - a quantity of an investment purchased at a specific time and cost.
  It may also have descriptive note attached.
  With many investments, lots must be tracked individually for tax reporting.

- Cost basis - a lot's acquisition cost. More generally, the combination of acquisition
  time, cost, and note if any.

- Capital gain/loss - your net profit or loss arising from the change in value of an investment
  since you acquired it. Some times abbreviated as "gains" in these docs.
  While you are holding the investment, you have unrealised gains, which fluctuate along with the market value.
  Once you dispose of it, you have realised gains.
  Capital gain/loss has tax consequences.

- Reduction strategy, lot selection - the order in which lots are reduced, eg when you are selling a stock
  or gifting some cryptocurrency, which ones do you reduce first ?
  Common strategies: FIFO (first in first out), LIFO (last in first out),
  and Specific Order (a custom order, which should be recorded). 
  The reduction strategy affects capital gains now and later, and has tax consequences.
  Sometimes you can choose it, at other times it is mandated by the tax authorities.

## Lot ideas

2023-01 Some examples/brainstorming of lot notations and functionality.

I believe one could emulate most of ledger/beancount's lot tracking/selection with simpler syntax -
just @, with less or no need for {} (curly brace syntax).

### Explicit lot accounts

Eg here, using explicit subaccounts to track lots, no {} is needed.:

```journal
2022-01-01 buy at 10
  assets:aaa:_20220101     10 AAA @ $10
  assets:cash           $-100
    
2022-02-01 buy at 12
  assets:aaa:_20220201     10 AAA @ $12
  assets:cash           $-120
    
2022-03-01 sell at 20
  assets:aaa:_20220101    -10 AAA @ $10  ; original cost basis
  assets:aaa:_20220201     -5 AAA @ $12
  assets:cash            $300
  revenues:gains        $-140
```

### Inferring cost from lot account

Assuming each lot subaccount holds only one lot, the cost basis could be recalled automatically when selling, though it's less readable:

```journal
2022-01-01 buy at 10
  assets:aaa:_20220101     10 AAA @ $10
  assets:cash           $-100
    
2022-02-01 buy at 12
  assets:aaa:_20220201     10 AAA @ $12
  assets:cash           $-120
    
2022-03-01 sell at 20
  assets:aaa:_20220101    -10 AAA  ; @ $10 implied
  assets:aaa:_20220201     -5 AAA  ; @ $12 implied
  assets:cash            $300
  revenues:gains        $-140
```

### Cost in lot account name

Cost basis could also be indicated in the subaccount name:

```journal
2022-01-01 buy at 10
  assets:aaa:_20220101_$10     10 AAA @ $10
  assets:cash               $-100
    
2022-02-01 buy at 12
  assets:aaa:_20220201_$12     10 AAA @ $12
  assets:cash               $-120
    
2022-03-01 sell at 20
  assets:aaa:_20220101_$10    -10 AAA  ; @ $10 implied, now more clear
  assets:aaa:_20220201_$12     -5 AAA
  assets:cash                $300
  revenues:gains            $-140
```

### Automatic lot accounts

Lot subaccounts could be created automatically, without having to write them; and could be used to select lots when withdrawing:

```journal
2022-01-01 buy at 10
  assets:aaa                 10 AAA @ $10  ; creates _20220101_$10 subaccount
  assets:cash             $-100
    
2022-02-01 buy at 12
  assets:aaa                 10 AAA @ $12  ; creates _20220201_$12
  assets:cash             $-120
    
2022-03-01 sell at 20
  assets:aaa:_20220201_$12  -10 AAA  ; select lot by subaccount
  assets:aaa:_20220101_$10   -5 AAA  ; LIFO order here
  assets:cash              $300
  revenues:gains          $-130
```

### Implicit lots

Or there could be no lot subaccounts, just lots tracked implictly by the tool, with special commands to view them, as in ledger/beancount:

```journal
2022-01-01 buy at 10
  assets:aaa                 10 AAA @ $10  ; creates an implicit lot
  assets:cash             $-100
    
2022-02-01 buy at 12
  assets:aaa                 10 AAA @ $12  ; view lots with bal --lots
  assets:cash             $-120
```

### Reduction strategy

Whether explicit, automatic or implicit, lots could be selected automatically according to some reduction strategy,
specified eg with a tag:

```journal
2022-03-01 sell at 20, FIFO
  assets:aaa                -15 AAA  ; reduce lots FIFO by default
  assets:cash              $300
  revenue:gains                      ; $-140 calculated
```

```journal
2022-03-01 sell at 20, LIFO
  assets:aaa                -15 AAA  ; reduce:LIFO
  assets:cash              $300
  revenue:gains                      ; $-130 calculated
```

The above are easy to enter but less informative and hard to calculate by eye; you could use the tool to convert to a more explicit entry:

```journal
2022-03-01 sell at 20, FIFO
  assets:aaa                -10 AAA @ $10
  assets:aaa                 -5 AAA @ $12
  assets:cash              $300
  revenue:gains           $-140
```

```journal
2022-03-01 sell at 20, LIFO
  assets:aaa                -10 AAA @ $12
  assets:aaa                 -5 AAA @ $10
  assets:cash              $300
  revenue:gains           $-130
```

### Lot selection syntax

If lots are implicit, ie there are no subaccounts by which to select them,
some special syntax is needed to allow identifying them individually by cost, date, and/or note.
This could be {}, [], tags, or something new. Eg:

```journal
2022-03-01 sell at 20, taking 3 alternately from each lot
  assets:aaa                 -3 AAA {@ $10}                         ; lot 1
  assets:aaa                 -3 AAA {2022-02-01}                    ; lot 2
  assets:aaa                 -3 AAA {buy at 10}                     ; lot 1
  assets:aaa                 -3 AAA {@ $10, 2022-02-01, buy at 12}  ; lot 2
  assets:aaa                 -3 AAA                                 ; lot-date:2022-01-01, lot-cost:$10, lot-note:buy at 10, (lot 1)
  assets:cash              $300
  revenue:gains           $-138
```

### Use of curly braces

I don't see the need to use {} as much as Ledger/Beancount do.
In particular, Ledger/Beancount's {} syntax allows creating a lot with a cost basis
different from what it cost you in the transaction acquiring it.
What is the real need for this, and how often is it needed ?

It's not needed eg when buying a commodity at a rate different from the market rate; you can do:

```journal
2022-01-01 receive AAA, currently worth $10, with effective cost to us of ~$11 because of fees
  revenues:usd              -10 AAA @ $10
  expenses:fees               1 AAA
  equity:basis adjustment    -1 AAA
  assets:cash                 9 AAA @ $11.111

commodity $0.00  ; help hledger balance the above
```

### Investments vs one-time transactions

Not yet mentioned: some commodities/balances fluctuate in value while
you hold them (eg an investment) and others are a one-time conversion
(eg buying foreign currency at the airport). 

@ can be used for both of these, it's essentially a matter of which cost you calculate with when disposing:

```journal
2022-01-01 buy at 10, hold with fluctuating value
  assets:aaa                 10 AAA @ $10     ; today's acquisition cost
  assets:cash             $-100

2022-03-01 sell at 20, with capital gain/loss
  assets:aaa                -10 AAA @ $10     ; original acquisition cost
  assets:cash              $200
  revenue:gains           $-100
```

```journal
2022-01-01 exchange SEK for USD, one-time conversion
  assets:cash              -100 SEK
  assets:cash                10 USD @ 10 SEK  ; today's conversion cost

2022-03-01 exchange back to SEK, one-time conversion
  assets:cash               -10 USD @ 11 SEK  ; today's conversion cost
  assets:cash               110 SEK
```

I believe @ and {} were intended to/can/do distinguish between these.
If using only @ there needs to be some other mechanism to indicate fluctuating value vs one-time conversion, or so it seems -
eg an annotation on the transaction, the account, or the commodity.



## Price syntax

### In Ledger and hledger

- In the journal, a `P DATE COMMODITY AMOUNT` directive some commodity's market price in some other commodity on DATE.
  (A timestamp may be added, but is ignored.)

- In a posting, `AMT @ UNITPRICE` declares the per-unit price that was used to convert AMT into the price's commodity.
  Eg: `2A @ 3B` records that 2A was posted, in exchange for 6B.

- `@@ TOTALPRICE` is another form of `@`, sometimes more convenient.
  Eg: `2A @@ 5.99B` records that 2A was posted in exchange for 5.99B.

### In Ledger

- `@ UNITPRICE`
  Any use of `@` also generates an implicit `P` directive.
  Eg:

      2019/1/1
        a  2A @ 3B
        b

  in the journal is equivalent to writing

      2019/1/1
        a  2A @ 3B
        b

      P 2019/1/1 A 1.5B

- `{UNITPRICE}`

- `{=FIXEDUNITPRICE}`

The following are variants of the above; they work the same way except
that you write the total instead of the unit price:

- `@@ TOTALPRICE`
- `{{TOTALPRICE}}`
- `{{=FIXEDTOTALPRICE}}`

### In hledger

- `@` does not generate a market price
- `{}` and `{=}` are ignored

## Capital gains

### A model for capital gains

Capital gain/loss (when the value of assets you hold increases/decreases
due to market price fluctuations) - is an important topic, since it can
generate tax liability.

Here is a description of how it works, intended for both users and
builders of accounting software (especially, plain text accounting
software). (I'm a software engineer, not an accountant. In places there
may be better accounting terms I'm not familiar with yet.)

- lots/units -
  A quantity of some commodity, acquired at a certain price on a certain date,
  is called a *lot*, or *unit*. (I'm not sure which is the most standard term. Using lot for now.)

- Since you might have purchased the lot on a stock exchange, received it as a gift,
  or something else, we'll call this event *lot acquisition*, on the *acquisition date*.

- Later you might sell the lot for cash, or exchange it for something else, or gift it.
  We'll call this *lot disposal*.

- You might have paid current market value for the lot, or you might have
  paid less or more than that. We'll call what you paid/exchanged the *acquisition amount*.
  
- I think the acquisition amount is also called the *basis* or *cost basis*.
  Or possibly the current market value is the basis, regardless of what you paid.
  Perhaps it depends. To be clarified. The basis at which you acquired a lot is important.

- After acquisition, while you are still holding the lot, if the market value of that commodity goes up (or down),
  your potential return from disposing of the lot increases (or decreases).
  This is known as *capital gain (or loss)* (we'll just call it "capital gain").
  At this stage, the gain is only "on paper", so it is called *unrealised capital gain* (URG).
  This is not considered revenue, or taxable.

- It's common to be holding multiple lots, perhaps many, even in a single account.
  Eg, say you buy a small amount of some stock or cryptocurrency each week.
  Each purchase adds a new lot to your assets. We'll call this a *multi-lot balance*, or *balance*.

- URG is calculated for a lot at a certain point in time.
  Likewise for a multi-lot balance.

- realised capital gain

- lot withdrawal strategies

- specific identification

### Capital gains in hledger

-  postings can have multiple commodities and multiple prices; each of
   these parts is a deposit or withdrawal to the account

- 
  ```haskell
  -- | Given a list of amounts all in the same commodity, interprets them
  -- as a sequence of lot deposits (the positive amounts) and withdrawals
  -- (the negative amounts), and applies them in order using the FIFO
  -- strategy for withdrawals, then returns the resulting lot balance (as
  -- another, shorter, list of amounts).
  sumLots :: [Amount] -> [Amount]
  ```
## Ease of getting started

What could make getting started substantially easier ?

- Official CI-generated binaries for all major platforms
- Builtin access to docs in web format

## Web docs

Provide the embedded user manuals as HTML also. Eg:

- hledger help --html   # temporary static html files
- hledger help --web    # serve from local hledger-web instance if installed
- hledger help --site   # on hledger.org
- hledger-ui ? h/w/s    # same as above
- hledger-web -> help   # served from hledger-web

## Config file

Name: hledger.conf (and possibly ~/.hledger.conf as well).

- easy to say and spell
- good highlighting support in editors

Format: toml/ini-ish format, but customised for our needs (if necessary).

Example:
```
# hledger.conf

[defaults]
# Set options/arguments to be always used with hledger commands.
# Each line is: HLEDGERCMD ARGS, or: hledger ARGS
hledger -f hledger.journal
bal -M --flat -b lastmonth
ui --watch
web -V
help --html

[commands]
# Define aliases for custom hledger commands.
# Each line is: CMDALIAS = HLEDGERCMD ARGS
assets = bal -M ^assets\b
liab   = bal -M ^liabilities\b

# Or use colon, like make ?
bs2:   bs --no-total date:thisyear

# Or just whitespace, like hledger csv rules ?
smui   ui ^sm\b

# Allow arbitrary shell commands ?
2019:    hledger -f 2019.journal
jstatus: git status -sb -- *.journal

# Allow multi-command shell scripts, with optional help string ?
bsis:
  "Show monthly balance sheet and income statement"
  hledger bs -M
  echo
  hledger is -M
  echo
```

Loaded: 

- at startup
and ideally:
- hledger-web: on each page load if changed, like journals
- hledger-ui --watch: on change, like journals

Location: 

Search a number of locations in order.
Values from multiple files are combined, with later files taking precedence.

User config file: should it be "modern"  ~/.config/hledger.conf or "old/simple" ~/.hledger.conf ? 
One or the other may be preferred/easier/more portable.
If we support both, should it be one or the other, or both ?

Parent directory config files: we'd probably like to recognise config files in parent directories.
How far up should we look - 
to the root dir ? 
to the user's home dir ? and if not under the user's home dir, don't look up at all ?
to the nearest VCS working directory root ?

This would be the simplest comprehensive scheme: use all of

1. ~/.config/hledger.conf
2. ~/.hledger.conf
3. hledger.conf in all directories from / down to the current directory

Eg: running hledger in /home/simon/project/finance would combine any of the following which exist:

- ~/.config/hledger.conf
- ~/.hledger.conf
- /hledger.conf
- /home/hledger.conf
- /home/simon/hledger.conf
- /home/simon/project/hledger.conf
- /home/simon/project/finance/hledger.conf

<style>
.wy-table-responsive { 
  overflow:visible;
}
</style>

## 1353-related upgrade notes

*Cf [#1353](https://github.com/simonmichael/hledger/issues/1353)*

User-visible changes when going from 1.20.4 to master:

|                            |                                                                                                                                                              |
|----------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `-B/--cost`                | Now a primary flag.                                                                                                                                          |
| `--value=cost`             | Now an alias for `-B/--cost`, and deprecated.                                                                                                                |
| `--value=cost,COMM`        | No longer supported, suggests `-B --value=X,COMM`.                                                                                                           |
| `--value=end`              | With `--change`, shows change of end values instead of end value of change.<br>`--value=then` approximates and hopefully is preferable to the old behaviour. |

Meaning of the cost/valuation short flags in master:

| Short flag            | Equivalent to              |
|-----------------------|----------------------------|
| `-B`                  | `--cost`                   |
| `-V`                  | `--value=then` (soon)      |
| `-X/--exchange  COMM` | `--value=then,COMM` (soon) |

## Valuation examples

Minimal example for testing some valuation behaviours discussed in
[#1353](https://github.com/simonmichael/hledger/issues/1353).
See [Balance report valuation](#balance-report-valuation) above.

```journal
; every ~15 days: one A is purchased, and A's market price in B increases.

2020-01-01
  (a)  1 A

2020-01-15
  (a)  1 A

2020-02-01
  (a)  1 A

2020-02-15
  (a)  1 A

P 2020-01-01 A  1 B
P 2020-01-15 A  2 B
P 2020-02-01 A  3 B
P 2020-02-15 A  4 B
```

Old `balance --change --value=end` behaviour: shows period-end value of period's balance change:

```cli
$ hledger-1.20.4 bal -M --value=end  # --change is the default
Balance changes in 2020-01-01..2020-02-29, valued at period ends:

   || Jan  Feb 
===++==========
 a || 4 B  8 B 
---++----------
   || 4 B  8 B 
```

New `balance --change --value=end` behaviour in master: shows change between period-end-valued period-end balances:

```cli
$ hledger-master bal -M --value=end
Period-end value changes in 2020-01-01..2020-02-29:

   || Jan   Feb 
===++===========
 a || 4 B  12 B 
---++-----------
   || 4 B  12 B 
```

`balance --value=then` is also supported in master: shows sum of postings' then-values in each period:

```cli
$ hledger-master bal -M --value=then
Balance changes in 2020-01-01..2020-02-29, valued at posting date:

   || Jan  Feb 
===++==========
 a || 3 B  7 B 
---++----------
   || 3 B  7 B 
```


