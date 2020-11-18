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

It will compute and display the internalized rate of return (IRR) and
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

ROI is return on investment. There are various ways to compute it.
IRR (internal/money-weighted rate of return) is one strategy.
TWR (time-weighted rate of return) is another.
hledger's "roi" command computes both IRR and TWR.

If you have a single "incoming" cash flow (e.g. you put money in a savings account) 
and a single "outgoing" flow (you extracted money + interest at the end of term) 
then both IRR and TWR will be the same, and will be equal to the rate of return of your saving account.

But if you keep paying into your account at irregular intervals, then IRR and TWR will be different.
IRR assumes that periods between cash flows are equal, the more this assumption is violated the more "off" it will be. 
TWR does not require time intervals to be equal and gives a much better estimate of how much your "average dollar" of investment grew.
However, IRR is wildly popular (possibly because Excel initially only had IRR, but not TWR), so it is included for completeness.

Now, what about unrealized gains reporting ?
In order to compute IRR or TWR, you need to know (1) how much you paid in, (2) how much you withdrew, and (3) how much the investment has grown in value.
1 and 2 are your transactions (regular ones) and 3 is in "unrealized gains" transactions.
So 1, 2 and 3 together are the cashflows of your investment that could be used to compute IRR and TWR.
So "unrealized gains" transactions are one of the inputs for "roi".

https://blog.commonwealth.com/measuring-portfolio-performance-twr-vs.-irr has good small examples.
It also explains why both numbers can be problematic.
But in layman's terms:

- TWR will show you how well your investment is performing (even though you might be adding and removing cash from your investment along the way); so if you have different investments or investment strategies, you can use TWR to compare them.
- IRR will show you how much money you are actually making. So if you are comparing different pay-in/withdrawal schedules into the same investment trying to find one that is best for you, IRR is your tool.

If you squint just the right way, your pay-ins/withdrawals will have effect on IRR, and growth of investment will have effect on TWR.
