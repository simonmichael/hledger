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
