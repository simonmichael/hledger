% hledger_timelog(5)
% 
% April 2016

<div class="web">
* toc
</div>
<div class="man">

# NAME

Timelog - the timeclock time logging format, as read by hledger

# DESCRIPTION

</div>

hledger can read timelog files.
[As with Ledger](http://ledger-cli.org/3.0/doc/ledger3.html#Time-Keeping),
these are (a subset of)
[timeclock.el](http://www.emacswiki.org/emacs/TimeClock)'s format,
containing clock-in and clock-out entries as in the example below.
The date is a [simple date](#simple-dates) (also, [default year directives](#default-year) work).
The time format is HH:MM[:SS][+-ZZZZ]. Seconds and timezone are optional.
The timezone, if present, must be four digits and is ignored
(currently the time is always interpreted as a local time).

```timelog
i 2015/03/30 09:00:00 some:account name  optional description after two spaces
o 2015/03/30 09:20:00
i 2015/03/31 22:21:45 another account
o 2015/04/01 02:00:34
```

hledger treats each clock-in/clock-out pair as a transaction posting
some number of hours to an account. Or if the session spans more than
one day, it is split into several transactions, one for each day. For
the above time log, `hledger print` generates these journal entries:

``` {.shell}
$ hledger -f t.timelog print
2015/03/30 * optional description after two spaces
    (some:account name)         0.33h

2015/03/31 * 22:21-23:59
    (another account)         1.64h

2015/04/01 * 00:00-02:00
    (another account)         2.01h

```

Here is a
[sample.timelog](https://raw.github.com/simonmichael/hledger/master/data/sample.timelog) to
download and some queries to try:

```shell
$ hledger -f sample.timelog balance                               # current time balances
$ hledger -f sample.timelog register -p 2009/3                    # sessions in march 2009
$ hledger -f sample.timelog register -p weekly --depth 1 --empty  # time summary by week
```

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://hub.darcs.net/simon/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:
    ```shell
    alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
    alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"
    ```
- or use the old `ti` and `to` scripts in the [ledger 2.x repository](https://github.com/ledger/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.
