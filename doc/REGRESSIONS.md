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
   - Be aware this might reveal your real name, on opencollective.com and here on hledger.org
   - [Submit Expense](https://opencollective.com/hledger/expenses/new)
     (if you are logged in, it will be under the ACTIONS menu) 
   - choose Invoice
   - choose a Payout method; Next
   - Expense title: Regression bounty for #NNNN
   - Description: "QA", and for fast processing also include
     - the WORK DATE or WORK PERIOD
     - the ISSUE URL
   - Date: today's date
   - Amount: USD 50, or 100 if you reported and fixed.
     Or if you choose to receive another currency, convert from USD with that day's conversion rate and mention the rate in Description.
   - Next; Submit Expense
5. Announce on the issue page or in chat that you've submitted (to help ensure it is not overlooked)
6. Wait for the finance manager (SM) to approve it. This should not take more than a day or two.
7. Then wait for Open Collective to pay it. Payouts happen twice a week.

## Regressions reported

| hledger version, bug report                                  | Reporter        | Bounty paid on                                                   |
|--------------------------------------------------------------|-----------------|------------------------------------------------------------------|
| **1.19** 2020-09-01                                          | -               | -                                                                |
| [#1568](https://github.com/simonmichael/hledger/issues/1568) | jolmg           | pre bounty                                                       |
| [#1688](https://github.com/simonmichael/hledger/issues/1688) | Simon Michael   | N/A                                                              |
| [#1698](https://github.com/simonmichael/hledger/issues/1698) | David Lowe      | [2021-09-18](https://opencollective.com/hledger/expenses/50380)  |
| [#1745](https://github.com/simonmichael/hledger/issues/1745) | Arne Schlüter   | [2021-11-02](https://opencollective.com/hledger/expenses/54446)  |
| [#1800](https://github.com/simonmichael/hledger/issues/1800) | Chuck Holmes    | [2022-01-21](https://opencollective.com/hledger/expenses/61802)  |
| **1.20** 2020-12-05                                          | -               | -                                                                |
| [#1439](https://github.com/simonmichael/hledger/issues/1439) | apauley         | pre bounty                                                       |
| [#1468](https://github.com/simonmichael/hledger/issues/1468) | Simon Michael   | N/A                                                              |
| **1.20.3** 2021-01-14                                        | -               | -                                                                |
| [#1566](https://github.com/simonmichael/hledger/issues/1566) | benwebber       | pre bounty                                                       |
| **1.21** 2021-03-10                                          | -               | -                                                                |
| [#1508](https://github.com/simonmichael/hledger/issues/1508) | edlanglois      | pre bounty                                                       |
| [#1523](https://github.com/simonmichael/hledger/issues/1526) | Simon Michael   | N/A                                                              |
| [#1526](https://github.com/simonmichael/hledger/issues/1526) | lestephane      | pre bounty                                                       |
| [#1527](https://github.com/simonmichael/hledger/issues/1527) | lestephane      | pre bounty                                                       |
| [#1656](https://github.com/simonmichael/hledger/issues/1656) | Stephen Morgan  | [2021-08-22](https://opencollective.com/hledger/expenses/48246)  |
| **1.22** 2021-07-03                                          | -               | -                                                                |
| [#1597](https://github.com/simonmichael/hledger/issues/1597) | Simon Michael   | [2021-07-08](https://opencollective.com/hledger/expenses/44939)  |
| [#1607](https://github.com/simonmichael/hledger/issues/1607) | Simon Michael   | [2021-07-16](https://opencollective.com/hledger/expenses/45547)  |
| [#1625](https://github.com/simonmichael/hledger/issues/1625) | Julian Klode    | [2021-07-30](https://opencollective.com/hledger/expenses/46431)  |
| [#1736](https://github.com/simonmichael/hledger/issues/1736) | Romain Gehrig   | [2021-11-14](https://opencollective.com/hledger/expenses/55510)  |
| [#1851](https://github.com/simonmichael/hledger/issues/1851) | Eric Langlois   | [2022-04-11](https://opencollective.com/hledger/expenses/72187)  |
| **1.22.1** 2021-08-02                                        | -               | -                                                                |
| [#1638](https://github.com/simonmichael/hledger/issues/1638) | Yann Büchau     | [2021-08-03](https://opencollective.com/hledger/expenses/46918)  |
| [#1642](https://github.com/simonmichael/hledger/issues/1642) | Simon Michael   | N/A                                                              |
| **1.23** 2021-09-21                                          | -               | -                                                                |
| [#1933](https://github.com/simonmichael/hledger/issues/1933) | Simon Michael   | [2022-09-14](https://opencollective.com/hledger/expenses/95068)  |
| [#2071](https://github.com/simonmichael/hledger/issues/2071) | William Pierce  | [2024-04-02](https://opencollective.com/hledger/expenses/195768) |
| **1.24** 2021-12-01                                          | -               | -                                                                |
| [#1782](https://github.com/simonmichael/hledger/issues/1782) | Simon Michael   | N/A                                                              |
| **1.25** 2022-03-04                                          | -               | -                                                                |
| [#2032](https://github.com/simonmichael/hledger/issues/2032) | Simon Michael   | [2023-05-03](https://opencollective.com/hledger/expenses/137410) |
| [#2196](https://github.com/simonmichael/hledger/issues/2196) | Pranesh Prakash |                                                                  |
| **1.27** 2022-09-01                                          | -               | -                                                                |
| [#1932](https://github.com/simonmichael/hledger/issues/1932) | Andras Fabian   | [2022-09-15](https://opencollective.com/hledger/expenses/95112)  |
| [#2018](https://github.com/simonmichael/hledger/issues/2018) | Allan Odgaard   | [2023-03-28](https://opencollective.com/hledger/expenses/130591) |
| **1.29** 2023-03-11                                          | -               | -                                                                |
| [#2012](https://github.com/simonmichael/hledger/issues/2012) | Simon Michael   | N/A                                                              |
| [#2020](https://github.com/simonmichael/hledger/issues/2020) | Pablo Mora      | [2023-03-31](https://opencollective.com/hledger/expenses/131350) |
| [#2023](https://github.com/simonmichael/hledger/issues/2023) | Simon Michael   | [2023-04-06](https://opencollective.com/hledger/expenses/132635) |
| [#2034](https://github.com/simonmichael/hledger/issues/2034) | Simon Michael   | N/A                                                              |
| [#2045](https://github.com/simonmichael/hledger/issues/2045) | Pranesh Prakash | [2023-10-17](https://opencollective.com/hledger/expenses/150171) |
| [#2153](https://github.com/simonmichael/hledger/issues/2153) | markokocic      | 2024-01-25, $50 donated                                          |
| **1.30** 2023-06-01                                          | -               | -                                                                |
| [#2072](https://github.com/simonmichael/hledger/issues/2072), #2137, #2150 | Simon Michael, usaAmch, ipvych |                                     |
| **1.31** 2023-09-03                                          | -               | -                                                                |
| [#2091](https://github.com/simonmichael/hledger/issues/2091) | Petr Slansky    | [2023-10-16](https://opencollective.com/hledger/expenses/166632) |
| [#2115](https://github.com/simonmichael/hledger/issues/2115) | pepe_pecas      | 2023-12-15, $100 donated                                         |
| **1.32** 2023-12-01                                          | -               | -                                                                |
| [#2125](https://github.com/simonmichael/hledger/issues/2125) | Simon Michael   | N/A                                                              |
| [#2127](https://github.com/simonmichael/hledger/issues/2127) | rajeevn1        |                                                                  |
| [#2130](https://github.com/simonmichael/hledger/issues/2130) | Simon Michael   | N/A                                                              |
| [#2134](https://github.com/simonmichael/hledger/issues/2134) | pepe_pecas      | 2023-12-15, $100 donated                                         |
| [#2156](https://github.com/simonmichael/hledger/issues/2156) | ishmaelavila    |                                                                  |


- [all regression reports](https://bugs.hledger.org/regressions)
- [regression bounty requests](https://opencollective.com/hledger/expenses?amount=50-100)
- [regression bounty payments](https://opencollective.com/hledger/transactions?kind=EXPENSE&amount=50-100)

