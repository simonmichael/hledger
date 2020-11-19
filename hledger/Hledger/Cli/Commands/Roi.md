roi\
Shows the time-weighted (TWR) and money-weighted (IRR) rate of return
on your investments.

_FLAGS

This command assumes that you have account(s) that hold nothing but
your investments and whenever you record current appraisal/valuation
of these investments you offset unrealized profit and loss into
account(s) that, again, hold nothing but unrealized profit and loss.

Any transactions affecting balance of investment account(s) and not
originating from unrealized profit and loss account(s) are assumed to
be your investments or withdrawals.

At a minimum, you need to supply a query (which could be just an
account name) to select your investments with `--inv`, and another
query to identify your profit and loss transactions with `--pnl`.

This command will compute and display the internalized rate of return (IRR) and
time-weighted rate of return (TWR) for your investments for the time
period requested. Both rates of return are annualized before display,
regardless of the length of reporting interval.

Note, in some cases this report can fail, for these reasons:

- Error (NotBracketed): No solution for Internal Rate of Return (IRR).
  Possible causes: IRR is huge (>1000000%), balance of investment becomes negative at some point in time.
- Error (SearchFailed): Failed to find solution for Internal Rate of Return (IRR).
  Either search does not converge to a solution, or converges too slowly.

Examples:

- Using roi to report unrealised gains:
https://github.com/simonmichael/hledger/blob/master/examples/roi-unrealised.ledger

More background:

"ROI" stands for "return on investment". Traditionally this was
computed as a difference between current value of investment and its
initial value, expressed in percentage of the initial value.

However, this approach is only practical in simple cases, where
investments receives no in-flows or out-flows of money, and where rate
of growth is fixed over time. For more complex scenarios you need
different ways to compute rate of return, and this command implements
two of them: IRR and TWR.

Internal rate of return, or "IRR" (also called "money-weighted rate of
return") takes into account effects of in-flows and out-flows.
Naively, if you are withdrawing from your investment, your future
gains would be smaller (in absolute numbers), and will be a smaller
percentage of your initial investment, and if you are adding to your
investment, you will receive bigger absolute gains (but probably at
the same rate of return). IRR is a way to compute rate of return for
each period between in-flow or out-flow of money, and then combine
them in a way that gives you an annual rate of return that investment
is expected to generate.

As mentioned before, in-flows and out-flows would be any cash that you
personally put in or withdraw, and for the "roi" command, these are
transactions that involve account(s) matching `--inv` argument and NOT
involve account(s) matching `--pnl` argument.

Presumably, you will also record changes in the value of your
investment, and balance them against "profit and loss" (or "unrealized
gains") account. Note that in order for IRR to compute the precise
effect of your in-flows and out-flows on the rate of return, you will
need to record the value of your investement on or close to the days
when in- or out-flows occur.

Implementation of IRR in hledger should match the `XIRR` formula in
Excel.

Second way to compute rate of return that `roi` command implements is
called "time-weighted rate of return" or "TWR". Like IRR, it will also
break the history of your investment into periods between in-flows and
out-flows to compute rate of return per each period and then a
compound rate of return. However, internal workings of TWR are quite
different.

In technical terms, IRR uses the same approach as computation of net
present value, and tries to find a discount rate that makes net
present value of all the cash flows of your investment to add up to
zero. This could be hard to wrap your head around, especially if you haven't
done discounted cash flow analysis before.

TWR represents your investment as an imaginary "unit fund" where in-flows/
out-flows lead to buying or selling "units" of your investment and changes in
its value change the value of "investment unit". Change in "unit price" over the
reporting period gives you rate of return of your investment.

References:
* [Explanation of rate of return](https://www.investopedia.com/terms/r/rateofreturn.asp)
* [Explanation of IRR](https://www.investopedia.com/terms/i/irr.asp)
* [Explanation of TWR](https://www.investopedia.com/terms/t/time-weightedror.asp)
* [Examples of computing IRR and TWR and discussion of the limitations of both metrics](https://blog.commonwealth.com/measuring-portfolio-performance-twr-vs.-irr)

More examples:

Lets say that we found an investment in Snake Oil that is proising to give us 10% annually:
```hledger
2019-01-01 Investing in Snake Oil
  assets:cash  -$100
  investment:snake oil

2019-12-24 Recording the growth of Snake Oil
  investment:snake oil   = $110
  equity:unrealized gains
```

For now, basic computation of the rate of return, as well as IRR and TWR, gives us the expected 10%:

```
$ hledger roi -Y --inv investment --pnl "unrealized"
+---++------------+------------++---------------+----------+-------------+-----++--------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) | PnL ||    IRR |    TWR |
+===++============+============++===============+==========+=============+=====++========+========+
| 1 || 2019-01-01 | 2019-12-31 ||             0 |      100 |         110 |  10 || 10.00% | 10.00% |
+---++------------+------------++---------------+----------+-------------+-----++--------+--------+
```

However, lets say that shorty after investing in the Snake Oil we started to have second thoughs, so we prompty withdrew $90, leaving only $10 in.
Before Christmas, though, we started to get the "fear of mission out", so we put the $90 back in. So for most of the year, our investment was just $10 dollars,
and it gave us just $1 in growth:

```hledger
2019-01-01 Investing in Snake Oil
  assets:cash  -$100
  investment:snake oil

2019-01-02 Buyers remorse
  assets:cash  $90
  investment:snake oil
       
2019-12-30 Fear of missing out
  assets:cash  -$90
  investment:snake oil

2019-12-31 Recording the growth of Snake Oil
  investment:snake oil   = $101
  equity:unrealized gains
```

Now IRR and TWR are drastically different:
```
$ hledger roi -Y --inv investment --pnl "unrealized"
+---++------------+------------++---------------+----------+-------------+-----++-------+-------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) | PnL ||   IRR |   TWR |
+===++============+============++===============+==========+=============+=====++=======+=======+
| 1 || 2019-01-01 | 2019-12-31 ||             0 |      100 |         101 |   1 || 9.32% | 1.00% |
+---++------------+------------++---------------+----------+-------------+-----++-------+-------+
```

Here, IRR tells us that we made close to 10% on the $10 dollars that
we had in the account most of the time. And TWR is ... just 1%? Why?

Based on the transactions in our journal, TWR "think" that we are buying back $90 worst of Snake Oil
at the same price that it had at the beginning of they year, and then after that our $100 investment
gets $1 increase in value, or 1% of $100. Let's take a closer look at what is happening here by
asking for quarterly reports instead of annual:

```
$ hledger roi -Q --inv investment --pnl "unrealized"
+---++------------+------------++---------------+----------+-------------+-----++--------+-------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) | PnL ||    IRR |   TWR |
+===++============+============++===============+==========+=============+=====++========+=======+
| 1 || 2019-01-01 | 2019-03-31 ||             0 |       10 |          10 |   0 ||  0.00% | 0.00% |
| 2 || 2019-04-01 | 2019-06-30 ||            10 |        0 |          10 |   0 ||  0.00% | 0.00% |
| 3 || 2019-07-01 | 2019-09-30 ||            10 |        0 |          10 |   0 ||  0.00% | 0.00% |
| 4 || 2019-10-01 | 2019-12-31 ||            10 |       90 |         101 |   1 || 37.80% | 4.03% |
+---++------------+------------++---------------+----------+-------------+-----++--------+-------+
```

Now both IRR and TWR are thrown off by the fact that all of the growth for our investment happens in
Q4 2019. This happes because IRR computation is still yielding 9.32% and TWR is still 1%, but
this time these are rates for three month period instead of twelve, so in order to get an annual rate
they should be multiplied by four!

Let's try to keep a better record of how Snake Oil grew in value:
```hledger
2019-01-01 Investing in Snake Oil
  assets:cash  -$100
  investment:snake oil

2019-01-02 Buyers remorse
  assets:cash  $90
  investment:snake oil

2019-02-28 Recording the growth of Snake Oil
  investment:snake oil  
  equity:unrealized gains  -$0.25

2019-06-30 Recording the growth of Snake Oil
  investment:snake oil  
  equity:unrealized gains  -$0.25

2019-09-30 Recording the growth of Snake Oil
  investment:snake oil  
  equity:unrealized gains  -$0.25

2019-12-30 Fear of missing out
  assets:cash  -$90
  investment:snake oil

2019-12-31 Recording the growth of Snake Oil
  investment:snake oil
  equity:unrealized gains  -$0.25
```

Would our quartery report look better now? Almost:
```
$ hledger roi -Q --inv investment --pnl "unrealized"
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) |  PnL ||    IRR |    TWR |
+===++============+============++===============+==========+=============+======++========+========+
| 1 || 2019-01-01 | 2019-03-31 ||             0 |       10 |       10.25 | 0.25 ||  9.53% | 10.53% |
| 2 || 2019-04-01 | 2019-06-30 ||         10.25 |        0 |       10.50 | 0.25 || 10.15% | 10.15% |
| 3 || 2019-07-01 | 2019-09-30 ||         10.50 |        0 |       10.75 | 0.25 ||  9.79% |  9.78% |
| 4 || 2019-10-01 | 2019-12-31 ||         10.75 |       90 |      101.00 | 0.25 ||  8.05% |  1.00% |
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
```

Something is still wrong with TWR computation for Q4, and if you have been paying attention you know
what it is already: big $90 buy-back is recorded prior to the only transaction that captures the
change of value of Snake Oil that happened in this time period. Lets combine transactions from 30th
and 31st of Dec into one:

```hledger
2019-12-30 Fear of missing out and growth of Snake Oil
  assets:cash  -$90
  investment:snake oil
  equity:unrealized gains  -$0.25
```

Now growth of investment properly affects its price at the time of buy-back:
```
$ hledger roi -Q --inv investment --pnl "unrealized"
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) |  PnL ||    IRR |    TWR |
+===++============+============++===============+==========+=============+======++========+========+
| 1 || 2019-01-01 | 2019-03-31 ||             0 |       10 |       10.25 | 0.25 ||  9.53% | 10.53% |
| 2 || 2019-04-01 | 2019-06-30 ||         10.25 |        0 |       10.50 | 0.25 || 10.15% | 10.15% |
| 3 || 2019-07-01 | 2019-09-30 ||         10.50 |        0 |       10.75 | 0.25 ||  9.79% |  9.78% |
| 4 || 2019-10-01 | 2019-12-31 ||         10.75 |       90 |      101.00 | 0.25 ||  8.05% |  9.57% |
+---++------------+------------++---------------+----------+-------------+------++--------+--------+
```

And for annual report, TWR now reports the exact profitability of our investment:
```
$ hledger roi -Y --inv investment --pnl "unrealized"
+---++------------+------------++---------------+----------+-------------+------++-------+--------+
|   ||      Begin |        End || Value (begin) | Cashflow | Value (end) |  PnL ||   IRR |    TWR |
+===++============+============++===============+==========+=============+======++=======+========+
| 1 || 2019-01-01 | 2019-12-31 ||             0 |      100 |      101.00 | 1.00 || 9.32% | 10.00% |
+---++------------+------------++---------------+----------+-------------+------++-------+--------+
```
