roi\
Shows the time-weighted (TWR) and money-weighted (IRR) rate of return
on your investments.

_FLAGS

At a minimum, you need to supply a query (which could be just an
account name) to select your investment(s) with `--inv`, and another
query to identify your profit and loss transactions with `--pnl`.

If you do not record changes in the value of your investment manually,
or do not require computation of time-weighted return (TWR), `--pnl`
could be an empty query (`--pnl ""` or `--pnl STR` where `STR` does
not match any of your accounts).

This command will compute and display the internalized rate of return
(IRR) and time-weighted rate of return (TWR) for your investments for
the time period requested. Both rates of return are annualized before
display, regardless of the length of reporting interval.

Price directives will be taken into account if you supply appropriate
`--cost` or `--value` flags (see [VALUATION](https://hledger.org/hledger.html#valuation)).

Note, in some cases this report can fail, for these reasons:

- Error (NotBracketed): No solution for Internal Rate of Return (IRR).
  Possible causes: IRR is huge (>1000000%), balance of investment becomes negative at some point in time.
- Error (SearchFailed): Failed to find solution for Internal Rate of Return (IRR).
  Either search does not converge to a solution, or converges too slowly.

Examples:

- Using roi to compute total return of investment in stocks:
https://github.com/simonmichael/hledger/blob/master/examples/roi-unrealised.ledger

- Cookbook -> [Return on Investment](return-on-investment.html)

### Semantics of `--inv` and `--pnl`

Query supplied to `--inv` has to match all transactions that are
related to your investment. Transactions not matching `--inv` will be
ignored.

In these transactions, ROI will conside postings that match `--inv` to
be "investment postings" and other postings (not matching `--inv`)
will be sorted into two categories: "cash flow" and "profit and loss",
as ROI needs to know which part of the investment value is your
contributions and which is due to the return on investment.


- "Cash flow" is depositing or withdrawing money, buying or selling
assets, or otherwise converting between your investment commodity and
any other commodity. Example:

    ```
    2019-01-01 Investing in Snake Oil
      assets:cash          -$100
      investment:snake oil

    2020-01-01 Selling my Snake Oil
      assets:cash           $10
      investment:snake oil  = 0
    ```

- "Profit and loss" is change in the value of your investment:

    ```
    2019-06-01 Snake Oil falls in value
      investment:snake oil  = $57
      equity:unrealized profit or loss
    ```

All non-investment postings are assumed to be "cash flow", unless they
match `--pnl` query. Changes in value of your investment due to
"profit and loss" postings will be considered as part of your
investment return.

Example: if you use `--inv snake --pnl equity:unrealized`, then
postings in the example below would be classifed as:

```
2019-01-01 Snake Oil #1
  assets:cash          -$100   ; cash flow posting
  investment:snake oil         ; investment posting

2019-03-01 Snake Oil #2
  equity:unrealized pnl  -$100 ; profit and loss posting
  snake oil                    ; investment posting

2019-07-01 Snake Oil #3
  equity:unrealized pnl        ; profit and loss posting
  cash          -$100          ; cash flow posting
  snake oil     $50            ; investment posting
```


### IRR and TWR explained

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
them in a way that gives you a compound annual rate of return that investment
is expected to generate.

As mentioned before, in-flows and out-flows would be any cash that you
personally put in or withdraw, and for the "roi" command, these are
the postings that match the query in the`--inv` argument and NOT
match the query in the`--pnl` argument.

If you manually record changes in the value of your investment as
transactions that balance them against "profit and loss" (or
"unrealized gains") account or use price directives, then in order for
IRR to compute the precise effect of your in-flows and out-flows on
the rate of return, you will need to record the value of your
investement on or close to the days when in- or out-flows occur.

In technical terms, IRR uses the same approach as computation of net
present value, and tries to find a discount rate that makes net
present value of all the cash flows of your investment to add up to
zero. This could be hard to wrap your head around, especially if you
haven't done discounted cash flow analysis before. Implementation of
IRR in hledger should produce results that match the `XIRR` formula in
Excel.

Second way to compute rate of return that `roi` command implements is
called "time-weighted rate of return" or "TWR". Like IRR, it will also
break the history of your investment into periods between in-flows,
out-flows and value changes, to compute rate of return per each period
and then a compound rate of return. However, internal workings of TWR
are quite different.

TWR represents your investment as an imaginary "unit fund" where in-flows/
out-flows lead to buying or selling "units" of your investment and changes in
its value change the value of "investment unit". Change in "unit price" over the
reporting period gives you rate of return of your investment.

References:
* [Explanation of rate of return](https://www.investopedia.com/terms/r/rateofreturn.asp)
* [Explanation of IRR](https://www.investopedia.com/terms/i/irr.asp)
* [Explanation of TWR](https://www.investopedia.com/terms/t/time-weightedror.asp)
* [Examples of computing IRR and TWR and discussion of the limitations of both metrics](https://blog.commonwealth.com/measuring-portfolio-performance-twr-vs.-irr)

