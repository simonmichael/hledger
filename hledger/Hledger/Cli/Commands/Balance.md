balance, bal\
Show accounts and their balances.

_FLAGS

`balance` is a flexible command for listing account balances, balance
changes, values, value changes, and more. 
Generally it shows a table, with rows representing accounts, and
columns representing time periods. 
Compared to Ledger's balance command, hledger's `balance` adds the
significant feature of multi-period reports.

Note there are some higher-level variants of the `balance` command
with convenient defaults, which can be simpler to use:
[`balancesheet`](#balancesheet),
[`balancesheetequity`](#balancesheetequity),
[`cashflow`](#cashflow) and
[`incomestatement`](#incomestatement). 
When you need more control, then use `balance`. 

Here's a quick overview of `balance` features
(many of these work with the higher-level commands as well),
followed by more detailed descriptions and examples. 
`balance` can show..

- accounts as a [list (`-l`) or a tree (`-t`)](#list-or-tree-mode)
- optionally depth-limited ([`-[1-9]`](#depth-limiting))
- sorted [by declaration order and name](#simple-balance-report),
  or [by amount](#sorting-by-amount)

..and their..

- balance changes ([`--change`](#simple-balance-report), the default report type)
- or actual and planned balance changes ([`--budget`](#budget-report))
- or value of balance changes ([`--change -V`](#valuation-type))
- or change of balance value ([`--valuechange`](#report-type))

..in..

- one time period (the whole journal period by default)
- or multiple periods ([`-D`, `-W`, `-M`, `-Q`, `-Y`, `-p INTERVAL`](#report-intervals))

..either..

- per period ([`--periodic`](#accumulation-type), the default accumulation type)
- or accumulated since report start date ([`--cumulative`](#accumulation-type))
- or accumulated since account creation ([`--historical/-H`](#accumulation-type))

..possibly converted to..

- cost ([`--value=cost[,COMM]/--cost/-B`](#valuation-type))
- or market value, as of transaction dates ([`--value=then[,COMM]`](#valuation-type))
- or at period ends ([`--value=end[,COMM]`](#valuation-type))
- or now ([`--value=now`](#valuation-type))
- or at some other date ([`--value=YYYY-MM-DD`](#valuation-type))

..with..

- totals ([`-T`](#multi-period-balance-report)), 
  averages ([`-A`](#multi-period-balance-report)), 
  percentages ([`-%`](#percentages)), 
  inverted sign ([`--invert`](#sorting-by-amount))
- rows and columns swapped ([`--transpose`](#multi-period-balance-report))
- another field used as account name ([`--pivot`](#multi-period-balance-report))
- custom-formatted line items (single-period reports only) ([`--format`](#customising-single-period-balance-reports))

This command supports the
[output destination](#output-destination) and
[output format](#output-format) options,
with output formats `txt`, `csv`, `json`, and (multi-period reports only:) `html`. 
In `txt` output in a colour-supporting terminal, negative amounts are shown in red.

<a name="classic-balance-report"></a>

### Simple balance report

With no arguments, `balance` shows a list of all accounts and their
change of balance - ie, the sum of posting amounts, both inflows and
outflows - during the entire period of the journal.
For real-world accounts, this should also match their end balance
at the end of the journal period (more on this below).

Accounts are sorted by [declaration order](#declaring-accounts)
if any, and then alphabetically by account name.
For instance, using [examples/sample.journal](https://github.com/simonmichael/hledger/blob/master/examples/sample.journal):

```shell
$ hledger bal
                  $1  assets:bank:saving
                 $-2  assets:cash
                  $1  expenses:food
                  $1  expenses:supplies
                 $-1  income:gifts
                 $-1  income:salary
                  $1  liabilities:debts
--------------------
                   0  
```

Accounts with a zero balance (and no non-zero subaccounts, in tree
mode - see below) are hidden by default. Use `-E/--empty` to show them
(revealing `assets:bank:checking` here):
 
```shell
$ hledger -f examples/sample.journal  bal  -E
                   0  assets:bank:checking
                  $1  assets:bank:saving
                 $-2  assets:cash
                  $1  expenses:food
                  $1  expenses:supplies
                 $-1  income:gifts
                 $-1  income:salary
                  $1  liabilities:debts
--------------------
                   0  
```

The total of the amounts displayed is shown as the last line, unless `-N`/`--no-total` is used.

### Filtered balance report

You can show fewer accounts, a different time period, totals from
cleared transactions only, etc. by using [query](#queries)
arguments or [options](#report-start--end-date) to limit
the postings being matched. Eg:

```shell
$ hledger bal --cleared assets date:200806
                 $-2  assets:cash
--------------------
                 $-2  
```

### List or tree mode

By default, or with `-l/--flat`, accounts are shown as a flat list
with their full names visible, as in the examples above.

With `-t/--tree`, the account hierarchy is shown, with subaccounts'
"leaf" names indented below their parent:

```shell
$ hledger balance
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
                  $2  expenses
                  $1    food
                  $1    supplies
                 $-2  income
                 $-1    gifts
                 $-1    salary
                  $1  liabilities:debts
--------------------
                   0
```

Notes:

- "Boring" accounts are combined with their subaccount for more
compact output, unless `--no-elide` is used. Boring accounts have no
balance of their own and just one subaccount (eg `assets:bank` and
`liabilities` above).

- All balances shown are "inclusive", ie including the balances from
all subaccounts. Note this means some repetition in the output, which
requires explanation when sharing reports with
non-plaintextaccounting-users. A tree mode report's final total is the
sum of the top-level balances shown, not of all the balances shown.

- Each group of sibling accounts (ie, under a common parent) is sorted separately.

### Depth limiting

With a `depth:N` query, or `--depth N` option, or just `-N`, 
balance reports will show accounts only to the specified depth,
hiding the deeper subaccounts. 
Account balances at the depth limit always include the balances from
any hidden subaccounts (even in list mode). 
This can be useful for getting an overview. Eg, limiting to depth 1:

```shell
$ hledger balance -N -1
                 $-1  assets
                  $2  expenses
                 $-2  income
                  $1  liabilities
```

You can also hide top-level account name parts, using `--drop N`.
This can be useful for hiding repetitive top-level account names:

```shell
$ hledger bal expenses --drop 1
                  $1  food
                  $1  supplies
--------------------
                  $2  
```

### Multi-period balance report

With a [report interval](#report-intervals) (set by the `-D/--daily`,
`-W/--weekly`, `-M/--monthly`, `-Q/--quarterly`, `-Y/--yearly`, or
`-p/--period` flag), `balance` shows a tabular report, with columns
representing successive time periods (and a title):

```shell
$ hledger balance --quarterly income expenses -E
Balance changes in 2008:

                   ||  2008q1  2008q2  2008q3  2008q4 
===================++=================================
 expenses:food     ||       0      $1       0       0 
 expenses:supplies ||       0      $1       0       0 
 income:gifts      ||       0     $-1       0       0 
 income:salary     ||     $-1       0       0       0 
-------------------++---------------------------------
                   ||     $-1      $1       0       0 

```

Notes:

- The report's start/end dates will be expanded, if necessary, to fully
encompass the displayed subperiods (so that the first and last
subperiods have the same duration as the others).
- Leading and trailing periods (columns) containing all zeroes are not
shown, unless `-E/--empty` is used.
- Accounts (rows) containing all zeroes are not shown, unless
`-E/--empty` is used.
- Amounts with many commodities are shown in abbreviated form, unless
`--no-elide` is used. *(experimental)*
- Average and/or total columns can be added with the `-A/--average` and
`-T/--row-total` flags.
- The `--transpose` flag can be used to exchange rows and columns.
- The `--pivot FIELD` option causes a different transaction field to be used as
  "account name". See [PIVOTING](#pivoting).
     
Multi-period reports with many periods can be too wide for easy viewing in the terminal.
Here are some ways to handle that:

- Hide the totals row with `-N/--no-total`
- Convert to a single currency with `-V`
- Maximize the terminal window
- Reduce the terminal's font size
- View with a pager like less, eg: `hledger bal -D --color=yes | less -RS`
- Output as CSV and use a CSV viewer like 
  [visidata] (`hledger bal -D -O csv | vd -f csv`),
  Emacs' [csv-mode] (`M-x csv-mode, C-c C-a`), 
  or a spreadsheet (`hledger bal -D -o a.csv && open a.csv`)
- Output as HTML and view with a browser: `hledger bal -D -o a.html && open a.html`

[csv-mode]: https://elpa.gnu.org/packages/csv-mode.html
[visidata]: https://www.visidata.org

### Sorting by amount

With `-S/--sort-amount`, accounts with the largest (most positive) balances are shown first.
Eg: `hledger bal expenses -MAS` shows your biggest averaged monthly expenses first.

Revenues and liability balances are typically negative, however, so `-S` shows these in reverse order.
To work around this, you can add `--invert` to flip the signs.
(Or, use one of the higher-level reports, which flip the sign automatically. Eg: `hledger incomestatement -MAS`).

<a name="tree-mode"></a>

### Percentages

With `-%/--percent`, balance reports show each account's value
expressed as a percentage of the (column) total:

```shell
$ hledger bal expenses -Q -%
Balance changes in 2008:

                   || 2008Q1   2008Q2  2008Q3  2008Q4 
===================++=================================
 expenses:food     ||      0   50.0 %       0       0 
 expenses:supplies ||      0   50.0 %       0       0 
-------------------++---------------------------------
                   ||      0  100.0 %       0       0 
```

Note it is not useful to calculate percentages if the amounts in a
column have mixed signs. In this case, make a separate report for each
sign, eg:

```shell
$ hledger bal -% amt:`>0`
$ hledger bal -% amt:`<0`
```

Similarly, if the amounts in a column have mixed commodities, convert
them to one commodity with `-B`, `-V`, `-X` or `--value`, or make a
separate report for each commodity:

```shell
$ hledger bal -% cur:\\$
$ hledger bal -% cur:€
```

<a name="multicolumn-balance-report"></a>

### Balance change, end balance, historical end balance

It's important to be clear on the meaning of the numbers shown in
balance reports. Here is some terminology we use:

A ***balance change*** is the net amount added to, or removed from, an account during some period.

An ***end balance*** is the amount accumulated in an account as of some date
(and some time, but hledger doesn't model that; assume end of day in your timezone).
It is the sum of previous balance changes.

We call it a ***historical end balance*** if it includes all balance changes since the account was created.
For a real world account, this means it will match the "historical record",
eg the balances reported in your bank statements or bank web UI. (If they are correct!)

In general, balance changes are what you want to see when reviewing
revenues and expenses, and historical end balances are what you want
to see when reviewing or reconciling asset, liability and equity accounts.

As mentioned above, the default `balance` report shows balance changes.
To ensure that you see accurate historical end balances:

1. If the account's full lifetime is not recorded in the journal,
   there should be an "opening balances" transaction (a transfer from
   equity to the account) that initialises its balance on some date
   (often the first day of a year).

2. The report should include all of the account's prior postings, eg
   by leaving the [report start date](#report-start-end-date)
   unspecified, or by using the `-H/--historical` flag, described
   below.

### Balance report types

Fair warning: balance report modes can get confusing, so feel free to
just mimic the examples below, or just use the higher-level
bs/bse/cf/is commands.

For more flexible reporting, there are three important option groups:

`hledger balance [REPORTTYPE] [ACCUMULATIONTYPE] [VALUATIONTYPE] ...`

#### Report type
The general thing that is calculated/shown in each cell. 
It is one of:

- `--change` : show a sum of posting amounts (**default**)
- `--budget` : like --change but also show a budget goal amount
- `--valuechange` : show change of value of period-end historical balances
<!-- - `--gain` : show change of value of period-end historical balances caused by market price fluctuations -->

#### Accumulation type
Which previous periods' postings should be included in calculations
(especially in multiperiod reports).
It is one of:

- `--periodic` : postings from column start to column end. Ie, show
  [changes] in each period. Typically used when reviewing
  revenues/expenses. (**default for balance, incomestatement**)

- `--cumulative` : postings from report start (specified by
  -b/--begin, eg) to column end. Ie, show [accumulated changes] since
  start of report. Rarely used.

- `--historical/-H` : postings from journal start to column end. Ie,
  show [historical balance] at end of each period. Typically used when
  reviewing assets/liabilities/equity. (**default for balancesheet,
  balancesheetequity, cashflow**)

[changes]:             #balance-change-end-balance-historical-end-balance
[accumulated changes]: #balance-change-end-balance-historical-end-balance
[historical balance]:  #balance-change-end-balance-historical-end-balance

#### Valuation type
Which kind of [valuation], [valuation date(s)] and optionally a target [valuation commodity] to use.
It is one of:

- no valuation, show amounts in their original commodities (**default**)
- `--value=cost[,COMM]`       : no valuation, show amounts converted to cost
- `--value=then[,COMM]`       : show value at transaction dates
- `--value=end[,COMM]`        : show value at period end date(s)
- `--value=now[,COMM]`        : show value at today's date
- `--value=YYYY-MM-DD[,COMM]` : show value at another date

or one of their aliases: [`--cost/-B`], [`--market/-V`] or [`--exchange/-X`].

[`--cost/-B`]: #-b-cost
[`--market/-V`]: #-v-value
[`--exchange/-X`]: #-x-value-in-specified-commodity

### Combining balance report options

Many but not all combinations of balance report options are useful.
Eg, `--row-total/-T` is disabled by `--cumulative` or `--historical`,
since summing already-summed end balances usually does not make sense.

Some important reports are:

- `bal revenues expenses` (`bal --periodic --change revenues expenses`)\
  revenues/expenses in each period, an income statement / profit & loss report
  
- `bal -H assets liabilities` (`bal --historical --change assets liabilities`)\
  historical asset/liability balances at end of each period, a simplified balance sheet

- `bal -H assets liabilities equity` (`bal --historical --change assets liabilities equity`)\
  historical asset/liability/equity balances at end of each period, a full balance sheet

- `bal assets not:receivable` (`bal --periodic --change assets not:receivable`)\
  liquid asset changes in each period, a cashflow report

Since these are so often used, they are also available as (enhanced) separate commands:\

- [`is`]  (`incomestatement`)
- [`bs`]  (`balancesheet`)
- [`bse`] (`balancesheetequity`)
- [`cf`]  (`cashflow`)

[`is`]:  #incomestatement
[`bs`]:  #balancesheet
[`bse`]: #balancesheetequity
[`cf`]:  #cashflow

Here is what the accumulation / valuation type combinations show:

| Valuation: ><br>Accumulation: v | no valuation                                                     | `--value= then`                                                    | `--value= end`                                              | `--value= YYYY-MM-DD /now`                            |
|---------------------------------|------------------------------------------------------------------|--------------------------------------------------------------------|-------------------------------------------------------------|-------------------------------------------------------|
| `--periodic`                    | change in period                                                 | sum of posting-date market values in period                        | period-end value of change in period                        | DATE-value of change in period                        |
| `--cumulative`                  | change from report start to period end                           | sum of posting-date market values from report start to period end  | period-end value of change from report start to period end  | DATE-value of change from report start to period end  |
| `--historical /-H`              | change from journal start to period end (historical end balance) | sum of posting-date market values from journal start to period end | period-end value of change from journal start to period end | DATE-value of change from journal start to period end |

<!--
### Balance report examples

```shell
$ hledger balance --quarterly income expenses -E --cumulative
Ending balances (cumulative) in 2008:

                   ||  2008/03/31  2008/06/30  2008/09/30  2008/12/31 
===================++=================================================
 expenses:food     ||           0          $1          $1          $1 
 expenses:supplies ||           0          $1          $1          $1 
 income:gifts      ||           0         $-1         $-1         $-1 
 income:salary     ||         $-1         $-1         $-1         $-1 
-------------------++-------------------------------------------------
                   ||         $-1           0           0           0 

```
```shell
$ hledger balance ^assets ^liabilities --quarterly --historical --begin 2008/4/1
Ending balances (historical) in 2008/04/01-2008/12/31:

                      ||  2008/06/30  2008/09/30  2008/12/31 
======================++=====================================
 assets:bank:checking ||          $1          $1           0 
 assets:bank:saving   ||          $1          $1          $1 
 assets:cash          ||         $-2         $-2         $-2 
 liabilities:debts    ||           0           0          $1 
----------------------++-------------------------------------
                      ||           0           0           0 

```
-->

### Budget report

The `--budget` report type activates extra columns showing any budget goals for each account and period.
The budget goals are defined by [periodic transactions](journal.html#periodic-transactions).
This is very useful for comparing planned and actual income, expenses, time usage, etc.

For example, you can take average monthly expenses in the common expense categories to construct a minimal monthly budget:
```journal
;; Budget
~ monthly
  income  $2000
  expenses:food    $400
  expenses:bus     $50
  expenses:movies  $30
  assets:bank:checking

;; Two months worth of expenses
2017-11-01
  income  $1950
  expenses:food    $396
  expenses:bus     $49
  expenses:movies  $30
  expenses:supplies  $20
  assets:bank:checking

2017-12-01
  income  $2100
  expenses:food    $412
  expenses:bus     $53
  expenses:gifts   $100
  assets:bank:checking
```

You can now see a monthly budget report:
```shell
$ hledger balance -M --budget
Budget performance in 2017/11/01-2017/12/31:

                      ||                      Nov                       Dec 
======================++====================================================
 assets               || $-2445 [  99% of $-2480]  $-2665 [ 107% of $-2480] 
 assets:bank          || $-2445 [  99% of $-2480]  $-2665 [ 107% of $-2480] 
 assets:bank:checking || $-2445 [  99% of $-2480]  $-2665 [ 107% of $-2480] 
 expenses             ||   $495 [ 103% of   $480]    $565 [ 118% of   $480] 
 expenses:bus         ||    $49 [  98% of    $50]     $53 [ 106% of    $50] 
 expenses:food        ||   $396 [  99% of   $400]    $412 [ 103% of   $400] 
 expenses:movies      ||    $30 [ 100% of    $30]       0 [   0% of    $30] 
 income               ||  $1950 [  98% of  $2000]   $2100 [ 105% of  $2000] 
----------------------++----------------------------------------------------
                      ||      0 [              0]       0 [              0] 
```

This is different from a normal balance report in several ways:

- Only accounts with budget goals during the report period are shown, by default.

- In each column, in square brackets after the actual amount, 
  budget goal amounts are shown, and the actual/goal percentage.
  (Note: budget goals should be in the same commodity as the actual amount.)

- All parent accounts are always shown, even in list mode. 
  Eg assets, assets:bank, and expenses above.

- Amounts always include all subaccounts, budgeted or unbudgeted, even in list mode.

This means that the numbers displayed will not always add up!
Eg above, the `expenses`  actual amount includes the gifts and supplies transactions,
but the `expenses:gifts` and `expenses:supplies` accounts are not
shown, as they have no budget amounts declared.

This can be confusing. When you need to make things clearer, use the `-E/--empty` flag, 
which will reveal all accounts including unbudgeted ones, giving the full picture. Eg:

```shell
$ hledger balance -M --budget --empty
Budget performance in 2017/11/01-2017/12/31:

                      ||                      Nov                       Dec 
======================++====================================================
 assets               || $-2445 [  99% of $-2480]  $-2665 [ 107% of $-2480] 
 assets:bank          || $-2445 [  99% of $-2480]  $-2665 [ 107% of $-2480] 
 assets:bank:checking || $-2445 [  99% of $-2480]  $-2665 [ 107% of $-2480] 
 expenses             ||   $495 [ 103% of   $480]    $565 [ 118% of   $480] 
 expenses:bus         ||    $49 [  98% of    $50]     $53 [ 106% of    $50] 
 expenses:food        ||   $396 [  99% of   $400]    $412 [ 103% of   $400] 
 expenses:gifts       ||      0                      $100                   
 expenses:movies      ||    $30 [ 100% of    $30]       0 [   0% of    $30] 
 expenses:supplies    ||    $20                         0                   
 income               ||  $1950 [  98% of  $2000]   $2100 [ 105% of  $2000] 
----------------------++----------------------------------------------------
                      ||      0 [              0]       0 [              0] 
```


You can roll over unspent budgets to next period with `--cumulative`:
```shell
$ hledger balance -M --budget --cumulative
Budget performance in 2017/11/01-2017/12/31:

                      ||                      Nov                       Dec 
======================++====================================================
 assets               || $-2445 [  99% of $-2480]  $-5110 [ 103% of $-4960] 
 assets:bank          || $-2445 [  99% of $-2480]  $-5110 [ 103% of $-4960] 
 assets:bank:checking || $-2445 [  99% of $-2480]  $-5110 [ 103% of $-4960] 
 expenses             ||   $495 [ 103% of   $480]   $1060 [ 110% of   $960] 
 expenses:bus         ||    $49 [  98% of    $50]    $102 [ 102% of   $100] 
 expenses:food        ||   $396 [  99% of   $400]    $808 [ 101% of   $800] 
 expenses:movies      ||    $30 [ 100% of    $30]     $30 [  50% of    $60] 
 income               ||  $1950 [  98% of  $2000]   $4050 [ 101% of  $4000] 
----------------------++----------------------------------------------------
                      ||      0 [              0]       0 [              0] 
```

For more examples and notes, see [Budgeting](budgeting.html).

#### Budget report start date

This might be a bug, but for now:
when making budget reports, it's a good idea to explicitly set the
report's start date to the first day of a reporting period, because
a periodic rule like `~ monthly` generates its transactions on the 1st
of each month, and if your journal has no regular transactions on the 1st,
the default report start date could exclude that budget goal, which can
be a little surprising. Eg here the default report period is just the
day of 2020-01-15:

```journal
~ monthly in 2020
  (expenses:food)  $500

2020-01-15
  expenses:food    $400
  assets:checking
```
```shell
$ hledger bal expenses --budget
Budget performance in 2020-01-15:

              || 2020-01-15 
==============++============
 <unbudgeted> ||       $400 
--------------++------------
              ||       $400 
```

To avoid this, specify the budget report's period, or at least the start date,
with `-b`/`-e`/`-p`/`date:`, to ensure it includes the budget goal transactions
(periodic transactions) that you want. Eg, adding `-b 2020/1/1` to the above:

```shell
$ hledger bal expenses --budget -b 2020/1/1
Budget performance in 2020-01-01..2020-01-15:

               || 2020-01-01..2020-01-15 
===============++========================
 expenses:food ||     $400 [80% of $500] 
---------------++------------------------
               ||     $400 [80% of $500] 
```

#### Nested budgets

You can add budgets to any account in your account hierarchy. If you have budgets on both parent account and some of its children, then budget(s)
of the child account(s) would be added to the budget of their parent, much like account balances behave.

In the most simple case this means that once you add a budget to any account, all its parents would have budget as well. 

To illustrate this, consider the following budget:
```
~ monthly from 2019/01
    expenses:personal             $1,000.00
    expenses:personal:electronics    $100.00
    liabilities
```

With this, monthly budget for electronics is defined to be $100 and budget for personal expenses is an additional $1000, which implicitly means
that budget for both `expenses:personal` and `expenses` is $1100.

Transactions in `expenses:personal:electronics` will be counted both towards its $100 budget and $1100 of `expenses:personal` , and transactions in any other subaccount of `expenses:personal` would be
counted towards only towards the budget of `expenses:personal`.

For example, let's consider these transactions:
```journal
~ monthly from 2019/01
    expenses:personal             $1,000.00
    expenses:personal:electronics    $100.00
    liabilities

2019/01/01 Google home hub
    expenses:personal:electronics          $90.00
    liabilities                           $-90.00

2019/01/02 Phone screen protector
    expenses:personal:electronics:upgrades          $10.00
    liabilities

2019/01/02 Weekly train ticket
    expenses:personal:train tickets       $153.00
    liabilities

2019/01/03 Flowers
    expenses:personal          $30.00
    liabilities
```

As you can see, we have transactions in `expenses:personal:electronics:upgrades` and `expenses:personal:train tickets`, and since both of these accounts are without explicitly defined budget,
these transactions would be counted towards budgets of `expenses:personal:electronics` and `expenses:personal` accordingly:

```shell
$ hledger balance --budget -M
Budget performance in 2019/01:

                               ||                           Jan 
===============================++===============================
 expenses                      ||  $283.00 [  26% of  $1100.00] 
 expenses:personal             ||  $283.00 [  26% of  $1100.00] 
 expenses:personal:electronics ||  $100.00 [ 100% of   $100.00] 
 liabilities                   || $-283.00 [  26% of $-1100.00] 
-------------------------------++-------------------------------
                               ||        0 [                 0] 
```

And with `--empty`, we can get a better picture of budget allocation and consumption:
```shell
$ hledger balance --budget -M --empty
Budget performance in 2019/01:

                                        ||                           Jan 
========================================++===============================
 expenses                               ||  $283.00 [  26% of  $1100.00] 
 expenses:personal                      ||  $283.00 [  26% of  $1100.00] 
 expenses:personal:electronics          ||  $100.00 [ 100% of   $100.00] 
 expenses:personal:electronics:upgrades ||   $10.00                      
 expenses:personal:train tickets        ||  $153.00                      
 liabilities                            || $-283.00 [  26% of $-1100.00] 
----------------------------------------++-------------------------------
                                        ||        0 [                 0] 
```

### Customising single-period balance reports

For single-period balance reports displayed in the terminal (only),
you can use `--format FMT` to customise the format and content of each
line. Eg:

```shell
$ hledger balance --format "%20(account) %12(total)"
              assets          $-1
         bank:saving           $1
                cash          $-2
            expenses           $2
                food           $1
            supplies           $1
              income          $-2
               gifts          $-1
              salary          $-1
   liabilities:debts           $1
---------------------------------
                                0
```

The FMT format string (plus a newline) specifies the formatting
applied to each account/balance pair. It may contain any suitable
text, with data fields interpolated like so:

`%[MIN][.MAX](FIELDNAME)`

- MIN pads with spaces to at least this width (optional)
- MAX truncates at this width (optional)
- FIELDNAME must be enclosed in parentheses, and can be one of:

    - `depth_spacer` - a number of spaces equal to the account's depth, or if MIN is specified, MIN * depth spaces.
    - `account`      - the account's name
    - `total`        - the account's balance/posted total, right justified

Also, FMT can begin with an optional prefix to control how
multi-commodity amounts are rendered:

- `%_` - render on multiple lines, bottom-aligned (the default)
- `%^` - render on multiple lines, top-aligned
- `%,` - render on one line, comma-separated

There are some quirks. Eg in one-line mode, `%(depth_spacer)` has no
effect, instead `%(account)` has indentation built in.
<!-- XXX retest:
Consistent column widths are not well enforced, causing ragged edges unless you set suitable widths.
Beware of specifying a maximum width; it will clip account names and amounts that are too wide, with no visible indication.
-->
Experimentation may be needed to get pleasing results.

Some example formats:

- `%(total)`         - the account's total
- `%-20.20(account)` - the account's name, left justified, padded to 20 characters and clipped at 20 characters
- `%,%-50(account)  %25(total)` - account name padded to 50 characters, total padded to 20 characters, with multiple commodities rendered on one line
- `%20(total)  %2(depth_spacer)%-(account)` - the default format for the single-column balance report

[valuation]: #valuation
[valuation date(s)]: #valuation-date
[valuation commodity]: #valuation-commodity
