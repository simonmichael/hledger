# hledger
## Robust, intuitive plain text accounting
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)
[![on hackage](https://img.shields.io/hackage/v/hledger.svg?label=hackage&colorB=green)](https://hackage.haskell.org/package/hledger)
[![](https://repology.org/badge/version-for-repo/stackage_lts/hledger.svg)](https://repology.org/metapackage/hledger)
[![](https://repology.org/badge/version-for-repo/stackage_nighly/hledger.svg)](https://repology.org/metapackage/hledger)
[![github issues](https://img.shields.io/github/issues/simonmichael/hledger.svg)](http://bugs.hledger.org)

Welcome! This a brief intro to hledger. For a more detailed version, see the home page: **<https://hledger.org>**

hledger is lightweight, cross platform, multi-currency, double-entry accounting software.
It lets you track money, investments, cryptocurrencies, invoices, time, inventory and more, 
in a safe, future-proof plain text data format with full version control and privacy. 

hledger aims to help both computer experts and regular folks
gain clarity in their finances and time management.
Though the UIs are basic, hledger can model any accounting situation and provide precise, clear reports.
It is reliable, quick, and backed by the highly supportive [Plain Text Accounting](https://plaintextaccounting.org) ecosystem. 
Using it is an excellent way to learn double entry accounting.

Compared to [other PTA apps](https://plaintextaccounting.org/#software), 
hledger is actively maintained, with regular releases,
and a strong focus on being easy to use and practical for everyday accounting.

More features:
- Installs easily on unix, mac or windows
- Complete, built-in documentation in multiple formats, beginner videos, tutorials etc.
- Multiple UIs: command-line, terminal, web, mobile, editors/IDEs
- Good at importing and exporting CSV; also outputs text/HTML/JSON/SQL
- A robust, general, well-specified multi-currency accounting engine
- Fast, analysing 25k transactions per second on a macbook air m1
- Accurate to 255 decimal places
- Supports your preferred account names, currencies, number formats
- Inspired by and partly compatible with Ledger CLI; interconvertible with Beancount
- Scriptable by CLI, HTTP or API, with plenty of examples
- Clean Haskell codebase, continually improved since 2007, with $100 regression bounties
- Free software licensed under GPLv3+.

## Examples

I use hledger to:
- track income and spending, sometimes with budgets
- see time reports by day/week/month/project
- track reimbursables, invoices and payments
- predict cashflow and account balances
- get accurate numbers for tax filing
- research past events

Here's an example of the journal file format:
```journal
2022-01-01 opening balances as of this date
    assets:bank:checking                $1000
    assets:bank:savings                 $2000
    assets:cash                          $100
    liabilities:creditcard               $-50
    equity:opening/closing balances

2022-01-15 market
    expenses:food             $50
    assets:cash              $-50

2022-02-01 GOODWORKS CORP
    assets:bank:checking           $1000
    income:salary                 $-1000
```
and some simple reports:
```cli
$ hledger bs
Balance Sheet 2022-02-15

                        || 2022-02-15 
========================++============
 Assets                 ||            
------------------------++------------
 assets:bank:checking   ||      $2000 
 assets:bank:savings    ||      $2000 
 assets:cash            ||        $50 
------------------------++------------
                        ||      $4050 
========================++============
 Liabilities            ||            
------------------------++------------
 liabilities:creditcard ||        $50 
------------------------++------------
                        ||        $50 
========================++============
 Net:                   ||      $4000 
```
```cli
$ hledger is --monthly                                            
Income Statement 2022-01-01..2022-02-28                                               
                                                                                      
               ||  Jan    Feb                                                         
===============++=============                                                        
 Revenues      ||                                                                     
---------------++-------------                                                        
 income:salary ||    0  $1000                                                         
---------------++-------------                                                        
               ||    0  $1000                                                         
===============++=============                                                        
 Expenses      ||                                                                     
---------------++-------------                                                        
 expenses:food ||  $50      0                                                         
---------------++-------------                                                        
               ||  $50      0                                                         
===============++=============                                                        
 Net:          || $-50  $1000                                                         
```

More examples: <https://hledger.org/#how-to-get-started>

## Sponsors

hledger is brought to you by [Simon Michael](http://joyful.com)
and [140+ contributors](CREDITS.html).
After enjoying some personal or organisational success with hledger,
you might want to become one of the generous sponsors helping to sustain this work.
(More info: <https://hledger.org/sponsor.html>)

<!-- keep synced with sponsor.md: -->

### Sponsor Simon (project leader)
[![github](https://img.shields.io/badge/Sponsor_on-Github-limegreen "Sponsor the project leader via Github")](https://github.com/sponsors/simonmichael)
[![liberapay](https://img.shields.io/badge/Sponsor_on-Liberapay-limegreen "Sponsor the project leader via Liberapay")](https://liberapay.com/simonmichael)
[![paypal](https://www.paypal.com/en_US/i/btn/x-click-but04.gif "Give one time or recurringly via Paypal")](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5J33NLXYXCYAY)

### Organisations sponsoring hledger
[![OpenCollective](https://opencollective.com/hledger/sponsors/badge.svg)][oc contributors] <!-- wrong count --> \
[![](https://opencollective.com/hledger/sponsor/0/avatar.svg)](https://opencollective.com/hledger/sponsor/0/website)
[![](https://opencollective.com/hledger/sponsor/1/avatar.svg)](https://opencollective.com/hledger/sponsor/1/website)
[![](https://opencollective.com/hledger/sponsor/2/avatar.svg)](https://opencollective.com/hledger/sponsor/2/website)
[![](https://opencollective.com/hledger/sponsor/3/avatar.svg)](https://opencollective.com/hledger/sponsor/3/website)
[![](https://opencollective.com/hledger/sponsor/4/avatar.svg)](https://opencollective.com/hledger/sponsor/4/website)
[![](https://opencollective.com/hledger/sponsor/5/avatar.svg)](https://opencollective.com/hledger/sponsor/5/website)
[![](https://opencollective.com/hledger/sponsor/6/avatar.svg)](https://opencollective.com/hledger/sponsor/6/website)
[![](https://opencollective.com/hledger/sponsor/7/avatar.svg)](https://opencollective.com/hledger/sponsor/7/website)
[![](https://opencollective.com/hledger/sponsor/8/avatar.svg)](https://opencollective.com/hledger/sponsor/8/website)
[![](https://opencollective.com/hledger/sponsor/9/avatar.svg)](https://opencollective.com/hledger/sponsor/9/website)

### Individuals sponsoring hledger
[![OpenCollective](https://opencollective.com/hledger/backers/badge.svg)][oc contributors] <!-- wrong count --> \
[![](https://opencollective.com/hledger/backer/0/avatar.svg)](https://opencollective.com/hledger/backer/0/website)
[![](https://opencollective.com/hledger/backer/1/avatar.svg)](https://opencollective.com/hledger/backer/1/website)
[![](https://opencollective.com/hledger/backer/2/avatar.svg)](https://opencollective.com/hledger/backer/2/website)
[![](https://opencollective.com/hledger/backer/3/avatar.svg)](https://opencollective.com/hledger/backer/3/website)
[![](https://opencollective.com/hledger/backer/4/avatar.svg)](https://opencollective.com/hledger/backer/4/website)
[![](https://opencollective.com/hledger/backer/5/avatar.svg)](https://opencollective.com/hledger/backer/5/website)
[![](https://opencollective.com/hledger/backer/6/avatar.svg)](https://opencollective.com/hledger/backer/6/website)
[![](https://opencollective.com/hledger/backer/7/avatar.svg)](https://opencollective.com/hledger/backer/7/website)
[![](https://opencollective.com/hledger/backer/8/avatar.svg)](https://opencollective.com/hledger/backer/8/website)
[![](https://opencollective.com/hledger/backer/9/avatar.svg)](https://opencollective.com/hledger/backer/9/website)
[![](https://opencollective.com/hledger/backer/10/avatar.svg)](https://opencollective.com/hledger/backer/10/website)
[![](https://opencollective.com/hledger/backer/11/avatar.svg)](https://opencollective.com/hledger/backer/11/website)
[![](https://opencollective.com/hledger/backer/12/avatar.svg)](https://opencollective.com/hledger/backer/12/website)
[![](https://opencollective.com/hledger/backer/13/avatar.svg)](https://opencollective.com/hledger/backer/13/website)
[![](https://opencollective.com/hledger/backer/14/avatar.svg)](https://opencollective.com/hledger/backer/14/website)
[![](https://opencollective.com/hledger/backer/15/avatar.svg)](https://opencollective.com/hledger/backer/15/website)
[![](https://opencollective.com/hledger/backer/16/avatar.svg)](https://opencollective.com/hledger/backer/16/website)
[![](https://opencollective.com/hledger/backer/17/avatar.svg)](https://opencollective.com/hledger/backer/17/website)
[![](https://opencollective.com/hledger/backer/18/avatar.svg)](https://opencollective.com/hledger/backer/18/website)
[![](https://opencollective.com/hledger/backer/19/avatar.svg)](https://opencollective.com/hledger/backer/19/website)
[![](https://opencollective.com/hledger/backer/20/avatar.svg)](https://opencollective.com/hledger/backer/20/website)
[![](https://opencollective.com/hledger/backer/21/avatar.svg)](https://opencollective.com/hledger/backer/21/website)
[![](https://opencollective.com/hledger/backer/22/avatar.svg)](https://opencollective.com/hledger/backer/22/website)
[![](https://opencollective.com/hledger/backer/23/avatar.svg)](https://opencollective.com/hledger/backer/23/website)
[![](https://opencollective.com/hledger/backer/24/avatar.svg)](https://opencollective.com/hledger/backer/24/website)
[![](https://opencollective.com/hledger/backer/25/avatar.svg)](https://opencollective.com/hledger/backer/25/website)
[![](https://opencollective.com/hledger/backer/26/avatar.svg)](https://opencollective.com/hledger/backer/26/website)
[![](https://opencollective.com/hledger/backer/27/avatar.svg)](https://opencollective.com/hledger/backer/27/website)
[![](https://opencollective.com/hledger/backer/28/avatar.svg)](https://opencollective.com/hledger/backer/28/website)
[![](https://opencollective.com/hledger/backer/29/avatar.svg)](https://opencollective.com/hledger/backer/29/website)

### Sponsor specific tasks
[![all bounties](https://img.shields.io/badge/github-All_bountied_issues-30bae8 "all bountied issues, bountysource and otherwise")](https://github.com/simonmichael/hledger/issues?q=label:bounty)
[![bountysource bounties](https://api.bountysource.com/badge/team?team_id=75979&style=bounties_received "issues bountied via bountysource")](https://www.bountysource.com/teams/hledger)

[oc contributors]: https://opencollective.com/hledger#section-contributors
