# Budgeting and forecasting

Budgeting and forecasting allows you to keep better track of your expenses and future financial situation.
If you write down your expectations of what your income/expenses/investment yields/etc should be, you can use them to:
- check how far off are your expectations from reality (budgeting)
- project your future account activity or balances (forecasting)

(This section uses examples/bcexample.hledger from hledger source repository).

## Periodic budget
To start budgeting, you need to know what your average yearly or weekly expenditures are. Hledger could help you with that.
Usually the interval for which you compute budget figures will be the same as the interval between
your paychecks -- monthly or weekly.

Lets create monthly (-M) report for years 2013-2014 (-b 2013) of all
top-level expense categories (--depth 2 Expenses), looking for average
figures (-A) in the cost at the time of transaction (-B), limiting
ourselves to USD transactions only, to save screen space:

```shell
$ hledger balance -f bcexample.hledger -MBA -b 2013 --depth 2 Expenses cur:USD
Balance changes in 2013/01/01-2014/10/31:

                    ||     2013/01      2013/02      2013/03  ...      2014/07      2014/08      2014/09      2014/10      Average 
====================++========================================...==================================================================
 Expenses:Financial ||    4.00 USD    12.95 USD    39.80 USD  ...    30.85 USD    21.90 USD    12.95 USD     4.00 USD    17.83 USD 
 Expenses:Food      ||  396.46 USD   481.48 USD   603.32 USD  ...   871.20 USD   768.23 USD   466.72 USD    83.00 USD   562.10 USD 
 Expenses:Health    ||  290.70 USD   193.80 USD   193.80 USD  ...   290.70 USD   193.80 USD   193.80 USD    96.90 USD   207.01 USD 
 Expenses:Home      || 2544.98 USD  2545.02 USD  2544.97 USD  ...  2545.12 USD  2545.01 USD  2545.10 USD            0  2429.33 USD 
 Expenses:Taxes     || 5976.60 USD  3984.40 USD  4901.83 USD  ...  5976.60 USD  3984.40 USD  3984.40 USD  1992.20 USD  4322.27 USD 
 Expenses:Transport ||  120.00 USD   120.00 USD   120.00 USD  ...            0   120.00 USD   120.00 USD   120.00 USD   109.09 USD 
--------------------++----------------------------------------...------------------------------------------------------------------
                    || 9332.74 USD  7337.65 USD  8403.72 USD  ...  9714.47 USD  7633.34 USD  7322.97 USD  2296.10 USD  7647.64 USD 
```

This report is rather wide and portion of it had been cut out for
brevity. Most interesting column is the last one, it shows average
monthly expenses for each category. Expenses in Food, Health, Home and
Transport categories seem to roughly similar month to month, so lets
create a budget for them.

Budgets are described with periodic transactions. Periodic transaction
has `~` instead of date and period expression instead of description. In this case
we want to create a monthly budget that will come into effect starting from January 2013,
which will include income of 10000 USD that is partically spent on Food, Health, Home and Transport
and the rest becomes our Assets:

```journal
~ monthly from 2013/01
  Expenses:Food    500 USD
  Expenses:Health  200 USD
  Expenses:Home    2545 USD
  Expenses:Transport   120 USD
  Income:US        -10700 USD ;; Taken as monthy average of Income account group
  Assets:US
```

This transaction could be put into separate file (budget.journal) or
could be kept in the main journal. Normally hledger will ignore it and
will not include it in any computations or reports.

To put it into action, you need to add `--budget` switch to your balance invocation. If you do that,
you would be able to see how your past expenses aligned with the budget that you just created. This
time, lets not limit accounts in any way:
```shell
$ hledger balance -f bcexample.hledger -f budget.journal -MB -b 2013 --budget cur:USD
Balance changes in 2013/01/01-2014/10/31:

                          ||                            2013/01                            2013/02                             2013/03 
==========================++===========================================================================================================
 <unbudgeted>:Expenses    ||                        5980.60 USD                        3997.35 USD                         4941.63 USD 
 <unbudgeted>:Liabilities ||                         293.09 USD                        -147.51 USD                          -66.01 USD 
 Assets:US                ||      1893.32 USD [26% of 7335 USD]      2929.77 USD [40% of 7335 USD]     -3898.89 USD [-53% of 7335 USD] 
 Expenses:Food            ||        396.46 USD [79% of 500 USD]        481.48 USD [96% of 500 USD]        603.32 USD [121% of 500 USD] 
 Expenses:Health          ||       290.70 USD [145% of 200 USD]        193.80 USD [97% of 200 USD]         193.80 USD [97% of 200 USD] 
 Expenses:Home            ||     2544.98 USD [100% of 2545 USD]     2545.02 USD [100% of 2545 USD]      2544.97 USD [100% of 2545 USD] 
 Expenses:Transport       ||       120.00 USD [100% of 120 USD]       120.00 USD [100% of 120 USD]        120.00 USD [100% of 120 USD] 
 Income:US                || -15119.10 USD [141% of -10700 USD]  -10331.21 USD [97% of -10700 USD]  -11079.40 USD [104% of -10700 USD] 
--------------------------++-----------------------------------------------------------------------------------------------------------
                          ||                       -3599.95 USD                        -211.30 USD                        -6640.58 USD 

```

Numbers in square brackets give you your budget estimate and percentage of it used by your real expenses. Numbers below 100% mean
that you have some of your budget left, numbers over 100% mean that you went over your budget.

You can notice that actual numbers for Assets:US seem to be well below computed budget of 7335 USD. Why? Answer to this is in the first
row of the report: we have quite a lot of unbudgeted Expenses!

Notice that even though we have not limited accounts in any way, report includes just those mentioned in the budget. This is on purpose,
assumption is that when you are checking your budgets you probably do not want unbudgeted accounts getting in your way. Another thing to
note is that budget numbers have been allocated to top-level expense subcategories (like Expenses:Food). Journal has subaccounts under
Food, but to compute budget report they have all been rolled up into a nearest parent with budget number associated with it. Accounts that
do not have such parent went into `<unbudgeted>` row.

Allright, it seems that for Jan 2013 we have ~3000 USD of budgeted expenses and almost twice as much unbudgeted. Lets figure out what they are.
We can see more details if we add `--show-unbudgeted` switch:
```shell
$ hledger balance -f bcexample.hledger -f budget.journal -M -b 2013-01 -e 2013-02 --budget cur:USD --show-unbudgeted
Balance changes in 2013/01:

                                  ||                            2013/01 
==================================++====================================
 Assets:US                        ||      1893.32 USD [26% of 7335 USD] 
 Expenses:Financial:Fees          ||                           4.00 USD 
 Expenses:Food                    ||        396.46 USD [79% of 500 USD] 
 Expenses:Health                  ||       290.70 USD [145% of 200 USD] 
 Expenses:Home                    ||     2544.98 USD [100% of 2545 USD] 
 Expenses:Taxes:Y2013:US:CityNYC  ||                         524.76 USD 
 Expenses:Taxes:Y2013:US:Federal  ||                        3188.76 USD 
 Expenses:Taxes:Y2013:US:Medicare ||                         319.86 USD 
 Expenses:Taxes:Y2013:US:SDI      ||                           3.36 USD 
 Expenses:Taxes:Y2013:US:SocSec   ||                         844.62 USD 
 Expenses:Taxes:Y2013:US:State    ||                        1095.24 USD 
 Expenses:Transport               ||       120.00 USD [100% of 120 USD] 
 Income:US                        || -15119.10 USD [141% of -10700 USD] 
 Liabilities:US:Chase:Slate       ||                         293.09 USD 
----------------------------------++------------------------------------
                                  ||                       -3599.95 USD 

```

All the accounts that were rolled up into `<unbudgeted>` category are now shown with their original name, but budgeted accounts are still rolled up. It
is easy to see now that we forgot taxes. Lets add them to our budget:
```journal
~ monthly from 2013/01
  Expenses:Food    500 USD
  Expenses:Health  200 USD
  Expenses:Home    2545 USD
  Expenses:Transport   120 USD
  Expenses:Taxes   4300 USD ;; Taken from monthly average report
  Income:US        -10700 USD
  Assets:US
```

Lets try again for a couple of month with this updated budget:
```shell
$ hledger balance -f bcexample.hledger -f budget.journal -M -b 2013-01 -e 2013-04 --budget cur:USD 
Balance changes in 2013q1:

                          ||                            2013/01                            2013/02                             2013/03 
==========================++===========================================================================================================
 <unbudgeted>:Expenses    ||                           4.00 USD                          12.95 USD                           39.80 USD 
 <unbudgeted>:Liabilities ||                         293.09 USD                        -147.51 USD                          -66.01 USD 
 Assets:US                ||      1893.32 USD [62% of 3035 USD]      2929.77 USD [97% of 3035 USD]    -3898.89 USD [-128% of 3035 USD] 
 Expenses:Food            ||        396.46 USD [79% of 500 USD]        481.48 USD [96% of 500 USD]        603.32 USD [121% of 500 USD] 
 Expenses:Health          ||       290.70 USD [145% of 200 USD]        193.80 USD [97% of 200 USD]         193.80 USD [97% of 200 USD] 
 Expenses:Home            ||     2544.98 USD [100% of 2545 USD]     2545.02 USD [100% of 2545 USD]      2544.97 USD [100% of 2545 USD] 
 Expenses:Taxes           ||     5976.60 USD [139% of 4300 USD]      3984.40 USD [93% of 4300 USD]      4901.83 USD [114% of 4300 USD] 
 Expenses:Transport       ||       120.00 USD [100% of 120 USD]       120.00 USD [100% of 120 USD]        120.00 USD [100% of 120 USD] 
 Income:US                || -15119.10 USD [141% of -10700 USD]  -10331.21 USD [97% of -10700 USD]  -11079.40 USD [104% of -10700 USD] 
--------------------------++-----------------------------------------------------------------------------------------------------------
                          ||                       -3599.95 USD                        -211.30 USD                        -6640.58 USD 

```

Now unbudgeted amounts are much smaller and some of them could be dismissed as noise, and we can see that budget created is actually
close enough to the real numbers, meaning that they are usually close to average that we put in our budget.

## Envelope budget

Budget report that we have used so far assumes that any unused budget amount for a given (monthly) period will not contribute to the
budget of the next period. Alternative popular "envelope budget" strategy assumes that you put a certain amount of money into an envelope
each month, and any unused amount stays there for future expenses. This is easy to simulate by adding --cumulative switch. Lets redo
the last report with it:
```shell
$ hledger balance -f bcexample.hledger -f budget.journal -M -b 2013-01 -e 2013-04 --cumulative --budget cur:USD
Ending balances (cumulative) in 2013q1:

                          ||                         2013/01/31                          2013/02/28                          2013/03/31 
==========================++============================================================================================================
 <unbudgeted>:Expenses    ||                           4.00 USD                           16.95 USD                           56.75 USD 
 <unbudgeted>:Liabilities ||                         293.09 USD                          145.58 USD                           79.57 USD 
 Assets:US                ||      1893.32 USD [62% of 3035 USD]       4823.09 USD [79% of 6070 USD]        924.20 USD [10% of 9105 USD] 
 Expenses:Food            ||        396.46 USD [79% of 500 USD]        877.94 USD [88% of 1000 USD]       1481.26 USD [99% of 1500 USD] 
 Expenses:Health          ||       290.70 USD [145% of 200 USD]        484.50 USD [121% of 400 USD]        678.30 USD [113% of 600 USD] 
 Expenses:Home            ||     2544.98 USD [100% of 2545 USD]      5090.00 USD [100% of 5090 USD]      7634.97 USD [100% of 7635 USD] 
 Expenses:Taxes           ||     5976.60 USD [139% of 4300 USD]      9961.00 USD [116% of 8600 USD]    14862.83 USD [115% of 12900 USD] 
 Expenses:Transport       ||       120.00 USD [100% of 120 USD]        240.00 USD [100% of 240 USD]        360.00 USD [100% of 360 USD] 
 Income:US                || -15119.10 USD [141% of -10700 USD]  -25450.31 USD [119% of -21400 USD]  -36529.71 USD [114% of -32100 USD] 
--------------------------++------------------------------------------------------------------------------------------------------------
                          ||                       -3599.95 USD                        -3811.25 USD                       -10451.83 USD 

```

If you look at Expenses:Food category, you will see that every month budget is increased by 500 USD, and by March total amount budgeted
is 1500 USD, of which 1481.26 USD is spent. If you look back at the previous non-cumulative monthly budget report, you will see that in March food expenses
were 121% of the budgeted amount, but cumulative report shows that taking into account budget carry-over from Jan and Feb we are well withing planned numbers.

# Forecasting

Budget transaction that was created could be used to predict what would be our financial situation in the future. If you add `--forecast` switch, you will
see how budgeted income and expense affects you past the last transaction in the journal. Since journal ends in Oct 2014, lets see next two month:

```shell
$ hledger balance -f bcexample.hledger -f budget.journal -M -b 2014-10 -e 2015 --forecast cur:USD
Balance changes in 2014q4:

                                    ||      2014/10     2014/11     2014/12 
====================================++======================================
 Assets:US                          ||            0    3035 USD    3035 USD 
 Assets:US:BofA:Checking            || -2453.40 USD           0           0 
 Assets:US:ETrade:Cash              ||  5000.00 USD           0           0 
 Expenses:Financial:Fees            ||     4.00 USD           0           0 
 Expenses:Food                      ||            0     500 USD     500 USD 
 Expenses:Food:Restaurant           ||    83.00 USD           0           0 
 Expenses:Health                    ||            0     200 USD     200 USD 
 Expenses:Health:Dental:Insurance   ||     2.90 USD           0           0 
 Expenses:Health:Life:GroupTermLife ||    24.32 USD           0           0 
 Expenses:Health:Medical:Insurance  ||    27.38 USD           0           0 
 Expenses:Health:Vision:Insurance   ||    42.30 USD           0           0 
 Expenses:Home                      ||            0    2545 USD    2545 USD 
 Expenses:Taxes                     ||            0    4300 USD    4300 USD 
 Expenses:Taxes:Y2014:US:CityNYC    ||   174.92 USD           0           0 
 Expenses:Taxes:Y2014:US:Federal    ||  1062.92 USD           0           0 
 Expenses:Taxes:Y2014:US:Medicare   ||   106.62 USD           0           0 
 Expenses:Taxes:Y2014:US:SDI        ||     1.12 USD           0           0 
 Expenses:Taxes:Y2014:US:SocSec     ||   281.54 USD           0           0 
 Expenses:Taxes:Y2014:US:State      ||   365.08 USD           0           0 
 Expenses:Transport                 ||            0     120 USD     120 USD 
 Expenses:Transport:Tram            ||   120.00 USD           0           0 
 Income:US                          ||            0  -10700 USD  -10700 USD 
 Income:US:Hoogle:GroupTermLife     ||   -24.32 USD           0           0 
 Income:US:Hoogle:Salary            || -4615.38 USD           0           0 
 Liabilities:US:Chase:Slate         ||  -203.00 USD           0           0 
------------------------------------++--------------------------------------
                                    ||            0           0           0 
```

Note that this time there is no roll-up of accounts. Unlike `--budget`, which could be used with `balance` command only, `--forecast`
could be used with any report. Forecast transactions would be added to your real journal and would appear in the report you requested as
if you have entered them on the scheduled dates.

Since quite a lot of accounts do not have any budgeted transactions, lets limit the depth of the report to avoid seeing lots of zeroes:
```shell
$ hledger balance -f bcexample.hledger -f budget.journal -M -b 2014-10 -e 2015 --forecast cur:USD --depth 2
Balance changes in 2014q4:

                    ||      2014/10     2014/11     2014/12 
====================++======================================
 Assets:US          ||  2546.60 USD    3035 USD    3035 USD 
 Expenses:Financial ||     4.00 USD           0           0 
 Expenses:Food      ||    83.00 USD     500 USD     500 USD 
 Expenses:Health    ||    96.90 USD     200 USD     200 USD 
 Expenses:Home      ||            0    2545 USD    2545 USD 
 Expenses:Taxes     ||  1992.20 USD    4300 USD    4300 USD 
 Expenses:Transport ||   120.00 USD     120 USD     120 USD 
 Income:US          || -4639.70 USD  -10700 USD  -10700 USD 
 Liabilities:US     ||  -203.00 USD           0           0 
--------------------++--------------------------------------
                    ||            0           0           0 
```

As you can see, we should expect 3035 USD to be added into Assets:US each month. It is quite easy to see how overal amount of Assets will change with time if you use
`--cumulative` switch:
```shell
$ hledger balance -f bcexample.hledger -f budget.journal -M -b 2014-10 -e 2015 --forecast cur:USD --depth 2 --cumulative
Ending balances (cumulative) in 2014q4:

                    ||   2014/10/31     2014/11/30     2014/12/31 
====================++============================================
 Assets:US          ||  2546.60 USD    5581.60 USD    8616.60 USD 
 Expenses:Financial ||     4.00 USD       4.00 USD       4.00 USD 
 Expenses:Food      ||    83.00 USD     583.00 USD    1083.00 USD 
 Expenses:Health    ||    96.90 USD     296.90 USD     496.90 USD 
 Expenses:Home      ||            0       2545 USD       5090 USD 
 Expenses:Taxes     ||  1992.20 USD    6292.20 USD   10592.20 USD 
 Expenses:Transport ||   120.00 USD     240.00 USD     360.00 USD 
 Income:US          || -4639.70 USD  -15339.70 USD  -26039.70 USD 
 Liabilities:US     ||  -203.00 USD    -203.00 USD    -203.00 USD 
--------------------++--------------------------------------------
                    ||            0              0              0 
```

According to forecast, assets are expected to grow to 8600+ USD by the end of 2014. However, our forecast does not include a couple
of big one-off year end expenses. First, we plan to buy prize turkey for the Christmas table every year from 2014, spending up to 500 USD on it.
And on 17th Nov 2014 we would celebrate birthday of significant other, spending up to 6000 USD in a fancy restaurant:
```journal
~ every 20th Dec from 2014
  Expenses:Food   500 USD ; Prize turkey, the biggest of the big
  Assets:US

~ 2014/11/17
  Assets:US
  Expenses:Food   6000 USD ; Birthday, lots of guests 
```

Note that turkey transaction is not entered as "yearly from 2014/12/20", since yearly/quarterly/monthy/weekly periodic expressions always generate
entries at the first day of the calendar year/quarter/month/week. Thus "monthly from 2014/12" will occur on 2014/12/01, 2015/01/01, ..., whereas
"every 20th of month from 2014/12" will happen on 2014/12/20, 2015/12/20, etc.

With latest additions forecast now looks like this:
```shell
hledger balance -f bcexample.hledger -f budget.journal -M -b 2014-10 -e 2015 --forecast cur:USD --depth 2 --cumulative
Ending balances (cumulative) in 2014q4:

                    ||   2014/10/31     2014/11/30     2014/12/31 
====================++============================================
 Assets:US          ||  2546.60 USD    -418.40 USD    2116.60 USD 
 Expenses:Financial ||     4.00 USD       4.00 USD       4.00 USD 
 Expenses:Food      ||    83.00 USD    6583.00 USD    7583.00 USD 
 Expenses:Health    ||    96.90 USD     296.90 USD     496.90 USD 
 Expenses:Home      ||            0       2545 USD       5090 USD 
 Expenses:Taxes     ||  1992.20 USD    6292.20 USD   10592.20 USD 
 Expenses:Transport ||   120.00 USD     240.00 USD     360.00 USD 
 Income:US          || -4639.70 USD  -15339.70 USD  -26039.70 USD 
 Liabilities:US     ||  -203.00 USD    -203.00 USD    -203.00 USD 
--------------------++--------------------------------------------
                    ||            0              0              0 
```

It is easy to see that in Nov 2014 we will run out of Assets. Using `register` we can figure out when or why it would happen:
```shell
$ hledger register -f bcexample.hledger -f budget.journal -b 2014-10 -e 2014-12 --forecast cur:USD Assets
2014/10/04 "BANK FEES" | "Monthly bank fee"         Assets:US:BofA:Checking                      -4.00 USD     -4.00 USD
2014/10/09 "Hoogle" | "Payroll"                     Assets:US:BofA:Checking                    2550.60 USD   2546.60 USD
2014/10/10 "Transfering accumulated savings to o..  Assets:US:BofA:Checking                   -5000.00 USD  -2453.40 USD
                                                    Assets:US:ETrade:Cash                      5000.00 USD   2546.60 USD
2014/11/01 Forecast transaction                     Assets:US                                     3035 USD   5581.60 USD
2014/11/17 Forecast transaction                     Assets:US                                    -6000 USD   -418.40 USD
```

It is 6000 USD planned for birthday! Something will have to be done about the birthday plans.
