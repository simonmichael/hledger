# hledger
## Robust, intuitive plain text accounting
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)
[![on hackage](https://img.shields.io/hackage/v/hledger.svg?label=hackage&colorB=green)](https://hackage.haskell.org/package/hledger)
[![](https://repology.org/badge/version-for-repo/stackage_nighly/hledger.svg)](https://repology.org/metapackage/hledger)
[![](https://repology.org/badge/version-for-repo/stackage_lts/hledger.svg)](https://repology.org/metapackage/hledger)
[![github issues](https://img.shields.io/github/issues/simonmichael/hledger.svg)](http://bugs.hledger.org)

Welcome! This is a brief intro to hledger. For a more detailed version, see the home page: **<https://hledger.org>**

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

More examples and screenshots: <https://hledger.org/#how-to-get-started>

## Funding

hledger is brought to you by
[Simon Michael](http://joyful.com),
[140+ contributors](doc/CREDITS.md),
and the generous financial sponsors below.

After enjoying some personal or organisational success with hledger,
you might want to become one of them, to help support this work.
It's easy! Please see <https://hledger.org/sponsor.html> for details.

<!-- keep synced with sponsor.md: -->

### Organisational sponsors

<a href="https://opencollective.com/hledger/organization/0/website"><img src="https://opencollective.com/hledger/organization/0/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/1/website"><img src="https://opencollective.com/hledger/organization/1/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/2/website"><img src="https://opencollective.com/hledger/organization/2/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/3/website"><img src="https://opencollective.com/hledger/organization/3/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/4/website"><img src="https://opencollective.com/hledger/organization/4/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/5/website"><img src="https://opencollective.com/hledger/organization/5/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/6/website"><img src="https://opencollective.com/hledger/organization/6/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/7/website"><img src="https://opencollective.com/hledger/organization/7/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/8/website"><img src="https://opencollective.com/hledger/organization/8/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/9/website"><img src="https://opencollective.com/hledger/organization/9/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/10/website"><img src="https://opencollective.com/hledger/organization/10/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/11/website"><img src="https://opencollective.com/hledger/organization/11/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/12/website"><img src="https://opencollective.com/hledger/organization/12/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/13/website"><img src="https://opencollective.com/hledger/organization/13/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/14/website"><img src="https://opencollective.com/hledger/organization/14/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/15/website"><img src="https://opencollective.com/hledger/organization/15/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/16/website"><img src="https://opencollective.com/hledger/organization/16/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/17/website"><img src="https://opencollective.com/hledger/organization/17/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/18/website"><img src="https://opencollective.com/hledger/organization/18/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/19/website"><img src="https://opencollective.com/hledger/organization/19/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/20/website"><img src="https://opencollective.com/hledger/organization/20/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/21/website"><img src="https://opencollective.com/hledger/organization/21/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/22/website"><img src="https://opencollective.com/hledger/organization/22/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/23/website"><img src="https://opencollective.com/hledger/organization/23/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/24/website"><img src="https://opencollective.com/hledger/organization/24/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/25/website"><img src="https://opencollective.com/hledger/organization/25/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/26/website"><img src="https://opencollective.com/hledger/organization/26/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/27/website"><img src="https://opencollective.com/hledger/organization/27/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/28/website"><img src="https://opencollective.com/hledger/organization/28/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/29/website"><img src="https://opencollective.com/hledger/organization/29/avatar.svg?avatarHeight=200"></a>
<a href="https://opencollective.com/hledger/organization/30/website"><img src="https://opencollective.com/hledger/organization/30/avatar.svg?avatarHeight=200"></a>

### Individual sponsors

<a href="https://opencollective.com/hledger/individual/0/website"><img src="https://opencollective.com/hledger/individual/0/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/1/website"><img src="https://opencollective.com/hledger/individual/1/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/2/website"><img src="https://opencollective.com/hledger/individual/2/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/3/website"><img src="https://opencollective.com/hledger/individual/3/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/4/website"><img src="https://opencollective.com/hledger/individual/4/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/5/website"><img src="https://opencollective.com/hledger/individual/5/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/6/website"><img src="https://opencollective.com/hledger/individual/6/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/7/website"><img src="https://opencollective.com/hledger/individual/7/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/8/website"><img src="https://opencollective.com/hledger/individual/8/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/9/website"><img src="https://opencollective.com/hledger/individual/9/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/10/website"><img src="https://opencollective.com/hledger/individual/10/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/11/website"><img src="https://opencollective.com/hledger/individual/11/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/12/website"><img src="https://opencollective.com/hledger/individual/12/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/13/website"><img src="https://opencollective.com/hledger/individual/13/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/14/website"><img src="https://opencollective.com/hledger/individual/14/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/15/website"><img src="https://opencollective.com/hledger/individual/15/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/16/website"><img src="https://opencollective.com/hledger/individual/16/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/17/website"><img src="https://opencollective.com/hledger/individual/17/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/18/website"><img src="https://opencollective.com/hledger/individual/18/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/19/website"><img src="https://opencollective.com/hledger/individual/19/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/20/website"><img src="https://opencollective.com/hledger/individual/20/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/21/website"><img src="https://opencollective.com/hledger/individual/21/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/22/website"><img src="https://opencollective.com/hledger/individual/22/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/23/website"><img src="https://opencollective.com/hledger/individual/23/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/24/website"><img src="https://opencollective.com/hledger/individual/24/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/25/website"><img src="https://opencollective.com/hledger/individual/25/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/26/website"><img src="https://opencollective.com/hledger/individual/26/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/27/website"><img src="https://opencollective.com/hledger/individual/27/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/28/website"><img src="https://opencollective.com/hledger/individual/28/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/29/website"><img src="https://opencollective.com/hledger/individual/29/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/30/website"><img src="https://opencollective.com/hledger/individual/30/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/31/website"><img src="https://opencollective.com/hledger/individual/31/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/32/website"><img src="https://opencollective.com/hledger/individual/32/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/33/website"><img src="https://opencollective.com/hledger/individual/33/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/34/website"><img src="https://opencollective.com/hledger/individual/34/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/35/website"><img src="https://opencollective.com/hledger/individual/35/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/36/website"><img src="https://opencollective.com/hledger/individual/36/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/37/website"><img src="https://opencollective.com/hledger/individual/37/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/38/website"><img src="https://opencollective.com/hledger/individual/38/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/39/website"><img src="https://opencollective.com/hledger/individual/39/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/40/website"><img src="https://opencollective.com/hledger/individual/40/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/41/website"><img src="https://opencollective.com/hledger/individual/41/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/42/website"><img src="https://opencollective.com/hledger/individual/42/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/43/website"><img src="https://opencollective.com/hledger/individual/43/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/44/website"><img src="https://opencollective.com/hledger/individual/44/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/45/website"><img src="https://opencollective.com/hledger/individual/45/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/46/website"><img src="https://opencollective.com/hledger/individual/46/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/47/website"><img src="https://opencollective.com/hledger/individual/47/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/48/website"><img src="https://opencollective.com/hledger/individual/48/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/49/website"><img src="https://opencollective.com/hledger/individual/49/avatar.svg?avatarHeight=100"></a>
<a href="https://opencollective.com/hledger/individual/50/website"><img src="https://opencollective.com/hledger/individual/50/avatar.svg?avatarHeight=100"></a>

<!-- (If your logo/avatar isn't appearing here, eg because you didn't use Open Collective, please [let me know](mailto:webmaster@hledger.org).) -->
