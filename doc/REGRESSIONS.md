# REGRESSIONS

<div class=pagetoc>

<!-- toc -->
</div>

Short url for this page: [hledger.org/regressions](https://hledger.org/regressions)

A regression is "something that used to work, that broke", or "an unintended not-good change"
in a released version of hledger.

We don't like regressions. We want to detect them quickly, repair them quickly, and reduce their frequency.

## Regression bounty

You can help!
[Since 2021-06-14](https://github.com/simonmichael/hledger/issues/1570) we have offered a $100 bounty for each new regression reported in hledger releases.
Since 2024-01-01, the bounty is split: $50 to the finder and $50 to the fixer (can be the same person, can't be the breaker).

To claim the bounty:

1. Discover a new regression yourself (and don't be the one who caused it)
2. Report it in the [hledger bug tracker](http://bugs.hledger.org)
3. Wait for the issue manager ([SM](https://joyful.com)) confirm it with the `regression` label
4. And/or, fix a regression yourself.
5. Send an expense reimbursement request to our Open Collective. 
   Be aware this might reveal your real name, on opencollective.com and here on hledger.org.
   Here's the suggested procedure for fastest processing:
   - click [Submit Expense](https://opencollective.com/hledger/expenses/new)  (if you are logged in, it will be under the ACTIONS menu) 
   - choose Invoice
   - choose a Payout method; click Next
   - Expense title: "Regression finder bounty for #NNNN" (or "fixer" or "finder & fixer")
   - Description: "Found ISSUEURL, WORKDATE(S)" (or "Fixed ...")
   - Date: today's date
   - Amount: USD 50 (or 100 if you found and fixed).
     Or if you choose to receive another currency, convert from USD with that day's conversion rate, and mention the rate in Description.
   - Next, Submit Expense
5. Announce on the issue page or in chat that you've submitted (to help ensure it is not overlooked)
6. Wait for the finance manager (SM) to approve it. This should not take more than a day or two.
7. Then wait for Open Collective to pay it. Payouts happen twice a week.

## Regressions reported

- [Issue tracker: all regression reports](https://bugs.hledger.org/regressions)
- [Open Collective: expense requests](https://opencollective.com/hledger/expenses)  <!-- not ?amount=50-100 because other currencies -->
- [Open Collective: payments](https://opencollective.com/hledger/transactions?kind=EXPENSE)

[#1439]: https://github.com/simonmichael/hledger/issues/1439
[#1468]: https://github.com/simonmichael/hledger/issues/1468
[#1508]: https://github.com/simonmichael/hledger/issues/1508
[#1523]: https://github.com/simonmichael/hledger/issues/1523
[#1526]: https://github.com/simonmichael/hledger/issues/1526
[#1527]: https://github.com/simonmichael/hledger/issues/1527
[#1566]: https://github.com/simonmichael/hledger/issues/1566
[#1568]: https://github.com/simonmichael/hledger/issues/1568
[#1597]: https://github.com/simonmichael/hledger/issues/1597
[#1607]: https://github.com/simonmichael/hledger/issues/1607
[#1625]: https://github.com/simonmichael/hledger/issues/1625
[#1638]: https://github.com/simonmichael/hledger/issues/1638
[#1642]: https://github.com/simonmichael/hledger/issues/1642
[#1656]: https://github.com/simonmichael/hledger/issues/1656
[#1688]: https://github.com/simonmichael/hledger/issues/1688
[#1698]: https://github.com/simonmichael/hledger/issues/1698
[#1736]: https://github.com/simonmichael/hledger/issues/1736
[#1745]: https://github.com/simonmichael/hledger/issues/1745
[#1782]: https://github.com/simonmichael/hledger/issues/1782
[#1800]: https://github.com/simonmichael/hledger/issues/1800
[#1851]: https://github.com/simonmichael/hledger/issues/1851
[#1932]: https://github.com/simonmichael/hledger/issues/1932
[#1933]: https://github.com/simonmichael/hledger/issues/1933
[#2012]: https://github.com/simonmichael/hledger/issues/2012
[#2018]: https://github.com/simonmichael/hledger/issues/2018
[#2020]: https://github.com/simonmichael/hledger/issues/2020
[#2023]: https://github.com/simonmichael/hledger/issues/2023
[#2032]: https://github.com/simonmichael/hledger/issues/2032
[#2034]: https://github.com/simonmichael/hledger/issues/2034
[#2045]: https://github.com/simonmichael/hledger/issues/2045
[#2071]: https://github.com/simonmichael/hledger/issues/2071
[#2072]: https://github.com/simonmichael/hledger/issues/2072
[#2091]: https://github.com/simonmichael/hledger/issues/2091
[#2115]: https://github.com/simonmichael/hledger/issues/2115
[#2125]: https://github.com/simonmichael/hledger/issues/2125
[#2127]: https://github.com/simonmichael/hledger/issues/2127
[#2130]: https://github.com/simonmichael/hledger/issues/2130
[#2134]: https://github.com/simonmichael/hledger/issues/2134
[#2137]: https://github.com/simonmichael/hledger/issues/2137
[#2150]: https://github.com/simonmichael/hledger/issues/2150
[#2153]: https://github.com/simonmichael/hledger/issues/2153
[#2156]: https://github.com/simonmichael/hledger/issues/2156
[#2196]: https://github.com/simonmichael/hledger/issues/2196
[#2224]: https://github.com/simonmichael/hledger/issues/2224
[#2225]: https://github.com/simonmichael/hledger/issues/2225
[#2227]: https://github.com/simonmichael/hledger/issues/2227
[#2254]: https://github.com/simonmichael/hledger/issues/2254

<!-- 
This table keeps evolving.
Dates following names are initially the finding/fixing date (useful to identify bounties),
then the latest submission/payment/donation date (useful to track resolution).
Bold things are unresolved. The flow is:
**FINDORFIXDATE unclaimed** -> **[EXPENSEDATE](EXPENSEPAGE)** -> [PAIDDATE](EXPENSEPAGE)
                                                              -> DONATEDATE donated
-->
| hledger&nbsp;version, <br>regressions&nbsp;found | Finder&nbsp;bounties <br>(since 2021-06-14)                  | Fixer&nbsp;bounties <br>(since 2024-01-01) <!-- some missing -->
|--------------------------|--------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------
| **1.19** 2020-09-01      | ---                                                                                  | ---
| [#1568]                  | jolmg           N/A                                                                  | N/A
| [#1688]                  | Simon Michael   N/A                                                                  | N/A
| [#1698]                  | David Lowe      [2021-09-18](https://opencollective.com/hledger/expenses/50380)      | N/A
| [#1745]                  | Arne Schlüter   [2021-11-02](https://opencollective.com/hledger/expenses/54446)      | N/A
| [#1800]                  | Chuck Holmes    [2022-01-21](https://opencollective.com/hledger/expenses/61802)      | N/A
| **1.20** 2020-12-05      | ---                                                                                  | ---
| [#1439]                  | apauley         N/A                                                                  | N/A
| [#1468]                  | Simon Michael   N/A                                                                  | N/A
| **1.20.3** 2021-01-14    | ---                                                                                  | ---
| [#1566]                  | benwebber       N/A                                                                  | N/A
| **1.21** 2021-03-10      | ---                                                                                  | ---
| [#1508]                  | edlanglois      N/A                                                                  | N/A
| [#1523]                  | Simon Michael   N/A                                                                  | N/A
| [#1526]                  | lestephane      N/A                                                                  | N/A
| [#1527]                  | lestephane      N/A                                                                  | N/A
| [#1656]                  | Stephen Morgan  [2021-08-22](https://opencollective.com/hledger/expenses/48246)      | N/A
| **1.22** 2021-07-03      | ---                                                                                  | ---
| [#1597]                  | Simon Michael   [2021-07-08](https://opencollective.com/hledger/expenses/44939)      | N/A
| [#1607]                  | Simon Michael   [2021-07-16](https://opencollective.com/hledger/expenses/45547)      | N/A
| [#1625]                  | Julian Klode    [2021-07-30](https://opencollective.com/hledger/expenses/46431)      | N/A
| [#1736]                  | Romain Gehrig   [2021-11-14](https://opencollective.com/hledger/expenses/55510)      | N/A
| [#1851]                  | Eric Langlois   [2022-04-11](https://opencollective.com/hledger/expenses/72187)      | N/A
| **1.22.1** 2021-08-02    | ---                                                                                  | ---
| [#1638]                  | Yann Büchau     [2021-08-03](https://opencollective.com/hledger/expenses/46918)      | N/A
| [#1642]                  | Simon Michael   N/A                                                                  | N/A
| **1.23** 2021-09-21      | ---                                                                                  | ---
| [#1933]                  | Simon Michael   [2022-09-14](https://opencollective.com/hledger/expenses/95068)      | N/A
| [#2071]                  | William Pierce  [2024-04-02](https://opencollective.com/hledger/expenses/195768)     | Simon Michael **[2024-02-25](https://opencollective.com/hledger/expenses/223927)**
| **1.24** 2021-12-01      | ---                                                                                  | ---
| [#1782]                  | Simon Michael   N/A                                                                  | N/A
| **1.25** 2022-03-04      | ---                                                                                  | ---
| [#2032]                  | Simon Michael   [2023-05-03](https://opencollective.com/hledger/expenses/137410)     | N/A
| [#2196]                  | Pranesh Prakash [2024-09-18](https://opencollective.com/hledger/expenses/220683)     | Simon Michael **[2024-04-25](https://opencollective.com/hledger/expenses/223926)**, <br>Bas van Dijk [#2224] 2024-09-22 donated
| **1.26** 2022-06-04      | ---                                                                                  | ---
| **1.27** 2022-09-01      | ---                                                                                  | ---
| [#1932]                  | Andras Fabian   [2022-09-15](https://opencollective.com/hledger/expenses/95112)      | N/A
| [#2018]                  | Allan Odgaard   [2023-03-28](https://opencollective.com/hledger/expenses/130591)     | N/A
| **1.28** 2022-12-01      | ---                                                                                  | ---
| **1.29** 2023-03-11      | ---                                                                                  | ---
| [#2012]                  | Simon Michael   N/A                                                                  | N/A
| [#2020]                  | Pablo Mora      [2023-03-31](https://opencollective.com/hledger/expenses/131350)     | N/A
| [#2023]                  | Simon Michael   [2023-04-06](https://opencollective.com/hledger/expenses/132635)     | N/A
| [#2034]                  | Simon Michael   N/A                                                                  | N/A
| [#2045]                  | Pranesh Prakash [2023-10-17](https://opencollective.com/hledger/expenses/150171)     | N/A
| [#2153]                  | markokocic      2024-01-25 donated                                                   | N/A
| **1.30** 2023-06-01      | ---                                                                                  | ---
| [#2072]                  | Simon Michael   **[2023-08-22](https://opencollective.com/hledger/expenses/223926)**  , <br>usaAmch [#2137] 2024-09-18 donated, <br>ipvych [#2150] [2024-09-25](https://opencollective.com/hledger/expenses/221597)  | Simon Michael **[2024-01-28](https://opencollective.com/hledger/expenses/223926)**
| **1.31** 2023-09-03      | ---                                                                                  | ---
| [#2091]                  | Petr Slansky    [2023-10-16](https://opencollective.com/hledger/expenses/166632)     | N/A
| [#2115]                  | usaAmch         2024-09-25 donated                                                   | N/A
| **1.32** 2023-12-01      | ---                                                                                  | ---
| [#2125]                  | Simon Michael   N/A                                                                  | N/A
| [#2127]                  | Rajeev N        [2023-12-15](https://opencollective.com/hledger/expenses/177761)     | N/A
| [#2130]                  | Simon Michael   N/A                                                                  | N/A
| [#2134]                  | pepe_pecas      2023-12-15 donated                                                   | N/A
| [#2156]                  | ishmaelavila    **2024-01-18 unclaimed**                                             | Simon Michael **[2024-01-21](https://opencollective.com/hledger/expenses/223926)**
| [#2254]                  | akanshaG42      2024-10-09 donated                                                   | Simon Michael **[2024-10-05](https://opencollective.com/hledger/expenses/223926)**
| **1.33** 2024-04-18      | ---                                                                                  | ---
| [#2227]                  | Henning Thielemann **2024-09-12 unclaimed**                                          | Henning Thielemann **2024-09-24 unclaimed**
| **1.34** 2024-06-01      | ---                                                                                  | ---
| **1.40** 2024-09-09      | ---                                                                                  | ---
| [#2225]                  | Bas van Dijk    2024-09-22 donated                                                   | Henning Thielemann **2024-09-18 unclaimed**
