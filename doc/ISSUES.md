# Issues

<div class=pagetoc>

<!-- toc -->
</div>

The hledger project\'s issue tracker is on github. It contains:

-   BUG issues - failures in some part of the hledger project (the main
    hledger packages, docs, website..)
-   WISH issues - feature proposals, enhancement requests
-   uncategorised issues - we don\'t know what these are yet
-   pull requests - proposed changes to code and docs

## Quick urls

- <http://issues.hledger.org>    - all issues, open or closed
- <http://bugs.hledger.org>      - open BUGs
- <http://wishes.hledger.org>    - open WISHes
- <http://prs.hledger.org>       - open pull requests
- <http://readyprs.hledger.org>  - open pull requests ready for review
- <http://draftprs.hledger.org>  - open draft pull requests
- <http://bugs.hledger.org/new>  - report a new issue
- <https://hledger.org/regressions  - how to claim the regression finder's bounty

## Open issues

<!-- 
This table doesn't have to be aligned, but it helps.
Editing it may require editor support, search/replace etc.
Syntax: https://www.pandoc.org/MANUAL.html#tables -> pipe_tables
-->

| COMPONENT/TOPIC                                                                                                           | BUGS                                                                                                                | WISHES                                                                                                                 | PRS                                                                                           | OTHER                                                                                                                                     |
|---------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------|
| [all](https://github.com/simonmichael/hledger/issues?q=is:open)                                                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22)                         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22)                         | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr)                         | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22)                         |
| **Tools:**                                                                                                                |                                                                                                                     |                                                                                                                        |                                                                                               |                                                                                                                                           |
| [install](https://github.com/simonmichael/hledger/issues?q=is:open+label:install) (hledger-install.sh)                    | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:install)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:install)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:install)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:install)           |
| [cli](https://github.com/simonmichael/hledger/issues?q=is:open+label:cli) (hledger)                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:cli)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:cli)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:cli)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:cli)               |
| [ui](https://github.com/simonmichael/hledger/issues?q=is:open+label:ui) (hledger-ui)                                      | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:ui)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:ui)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:ui)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:ui)                |
| [web](https://github.com/simonmichael/hledger/issues?q=is:open+label:web) (hledger-web)                                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:web)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:web)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:web)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:web)               |
| **Input/Output Formats:**                                                                                                 |                                                                                                                     |                                                                                                                        |                                                                                               |                                                                                                                                           |
| [journal](https://github.com/simonmichael/hledger/issues?q=is:open+label:journal)                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:journal)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:journal)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:journal)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:journal)           |
| [timeclock](https://github.com/simonmichael/hledger/issues?q=is:open+label:timeclock)                                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:timeclock)         | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:timeclock)         | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:timeclock)         | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:timeclock)         |
| [timedot](https://github.com/simonmichael/hledger/issues?q=is:open+label:timedot)                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:timedot)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:timedot)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:timedot)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:timedot)           |
| [csv](https://github.com/simonmichael/hledger/issues?q=is:open+label:csv)                                                 | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:csv)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:csv)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:csv)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:csv)               |
| [json](https://github.com/simonmichael/hledger/issues?q=is:open+label:json)                                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:json)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:json)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:json)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:json)              |
| [html](https://github.com/simonmichael/hledger/issues?q=is:open+label:html)                                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:html)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:html)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:html)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:html)              |
| **Commands:**                                                                                                             |                                                                                                                     |                                                                                                                        |                                                                                               |                                                                                                                                           |
| [accounts](https://github.com/simonmichael/hledger/issues?q=is:open+label:accounts)                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:accounts)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:accounts)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:accounts)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:accounts)          |
| [activity](https://github.com/simonmichael/hledger/issues?q=is:open+label:activity)                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:activity)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:activity)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:activity)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:activity)          |
| [add](https://github.com/simonmichael/hledger/issues?q=is:open+label:add)                                                 | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:add)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:add)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:add)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:add)               |
| [balcmds](https://github.com/simonmichael/hledger/issues?q=is:open+label:balcmds) (bal/bs/bse/cf/is/...)                  | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:balcmds)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:balcmds)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balcmds)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:balcmds)           |
| [balance](https://github.com/simonmichael/hledger/issues?q=is:open+label:balance)                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:balance)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:balance)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balance)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:balance)           |
| [balancesheet](https://github.com/simonmichael/hledger/issues?q=is:open+label:balancesheet)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:balancesheet)      | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:balancesheet)      | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balancesheet)      | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:balancesheet)      |
| [cashflow](https://github.com/simonmichael/hledger/issues?q=is:open+label:cashflow)                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:cashflow)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:cashflow)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:cashflow)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:cashflow)          |
| [close](https://github.com/simonmichael/hledger/issues?q=is:open+label:close)                                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:close)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:close)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:close)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:close)             |
| [import](https://github.com/simonmichael/hledger/issues?q=is:open+label:import)                                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:import)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:import)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:import)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:import)            |
| [incomestatement](https://github.com/simonmichael/hledger/issues?q=is:open+label:incomestatement)                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:incomestatement)   | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:incomestatement)   | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:incomestatement)   | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:incomestatement)   |
| [prices](https://github.com/simonmichael/hledger/issues?q=is:open+label:prices)                                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:prices)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:prices)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:prices)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:prices)            |
| [print](https://github.com/simonmichael/hledger/issues?q=is:open+label:print)                                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:print)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:print)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:print)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:print)             |
| [register](https://github.com/simonmichael/hledger/issues?q=is:open+label:register)                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:register)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:register)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:register)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:register)          |
| [rewrite](https://github.com/simonmichael/hledger/issues?q=is:open+label:rewrite)                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:rewrite)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:rewrite)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:rewrite)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:rewrite)           |
| [roi](https://github.com/simonmichael/hledger/issues?q=is:open+label:roi)                                                 | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:roi)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:roi)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:roi)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:roi)               |
| [stats](https://github.com/simonmichael/hledger/issues?q=is:open+label:stats)                                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:stats)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:stats)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:stats)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:stats)             |
| [tags](https://github.com/simonmichael/hledger/issues?q=is:open+label:tags)                                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:tags)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:tags)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:tags)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:tags)              |
| **Miscellaneous:**                                                                                                        |                                                                                                                     |                                                                                                                        |                                                                                               |                                                                                                                                           |
| [budget](https://github.com/simonmichael/hledger/issues?q=is:open+label:budget) (budgeting)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:budget)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:budget)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:budget)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:budget)            |
| [packaging](https://github.com/simonmichael/hledger/issues?q=is:open+label:deps) (packaging, dependencies)                | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:deps)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:deps)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:deps)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:deps)              |
| [doc](https://github.com/simonmichael/hledger/issues?q=is:open+label:doc) (documentation, help)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:doc)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:doc)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:doc)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:doc)               |
| [periodexpressions](https://github.com/simonmichael/hledger/issues?q=is:open+label:periodexpressions) (-b, -e, -p, date:) | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:periodexpressions) | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:periodexpressions) | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:periodexpressions) | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:periodexpressions) |
| [site](https://github.com/simonmichael/hledger/issues?q=is:open+label:site) (website, web presence)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:site)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:site)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:site)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:site)              |
| [tools](https://github.com/simonmichael/hledger/issues?q=is:open+label:tools) (dev tools, infrastructure)                 | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+BUG%22+label:tools)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:%22A+WISH%22+label:tools)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:tools)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:%22A+BUG%22+-label:%22A+WISH%22+label:tools)             |



Some loose conventions:

- In bug titles, mention the hledger version in which the bug first appeared 
  (and avoid mentioning version numbers otherwise).
  This allows searches like
  [new issues in 1.22](https://github.com/simonmichael/hledger/issues?q=in%3Atitle+1.22+)
  and
  [regressions in 1.22](https://github.com/simonmichael/hledger/issues?q=in%3Atitle+1.22+label%3Aregression%21)


## Labels

<https://github.com/simonmichael/hledger/labels>,
also listed at [open issues](#open-issues) above,
are used to categorise:

- whether an issue is a bug (red) or a wish (pink)
- related subcomponents (tools, commands, input/output formats) (light blue)
- related general topics (light green)
- related platforms (light purple)
- whether a bounty has been offered (dark green)
- why an issue is blocked (dark grey) or was closed (black)
- low priority info, like "imported" (white)

Labels can also be used as prefixes in issue/PR titles,
as prefixes in [commit messages](#commit-messages), etc.

## Custodians

If you are interested in helping with a particular component for a
while, please add yourself as a custodian in Open Issues table above.
A custodian\'s job is to help manage the issues, rally the troops, and
drive the open issue count towards zero. The more custodians, the
better! By dividing up the work this way, we can scale and make forward
progress.

## Milestones and Projects

Milestones are used a little bit to plan releases. In 2017 we
experimented with projects, but in 2018 milestones are in favour again..

## Estimates

You might see some experiments in estimate tracking, where some issue
names might have a suffix noting estimated and spent time. Basic format:
\[ESTIMATEDTOTALTASKTIME\|TIMESPENTSOFAR\]. Examples: \`\`\` \[2\] two
hours estimated, no time spent \[..\] half an hour estimated (a dot is
\~a quarter hour, as in timedot format) \[1d\] one day estimated (a day
is \~4 hours) \[1w\] one week estimated (a week is \~5 days or \~20
hours) \[3\|2\] three hours estimated, about two hours spent so far
\[1\|1w\|2d\] first estimate one hour, second estimate one week, about
two days spent so far \`\`\` Estimates are always for the total time
cost (not time remaining). Estimates are not usually changed, a new
estimate is added instead. Numbers are very approximate, but better than
nothing.

## Trello

The [trello board](http://trello.hledger.org) (trello.hledger.org) is an
old collection of wishlist items. This should probably be considered
deprecated.

## Prioritising

As of 2023 it's not too much of a problem knowing what's high priority to fix.
Still, <https://lostgarden.home.blog/2008/05/20/improving-bug-triage-with-user-pain/> 
describes an interesting method of ranking issues by a single "User Pain" metric,
what might be useful to try on our open bugs.
What adaptation of it might work for the hledger project ?
Here's a simplified version:

**Severity (How serious is this bug ?)**

- 5: Data loss or privacy/security loss bug.
- 4: Regression, crash or major usability/doc bug.
- 3: Installability, packaging or new user experience bug. A potential user could fail to get started.
- 2: Minor/moderate usability/doc bug. Easy to avoid or not a big deal.
- 1: Cleanup/design/developer bug. Significant only to developers and design-minded users.

**Likelihood (Who is likely to be affected by this bug ?)**

- 5: Affects all users.
- 4: Affects most users.
- 3: Affects a minority of users.
- 2: Affects only packagers or developers.
- 1: Affects almost no one.

**User Pain = S * L / 25**

  (Severity * Likelihood / (Max Severity * Max Likelihood) )

- All open bugs are listed in order of User Pain (AKA Priority).
- Developers check the Pain List daily and fix the highest pain bugs on the list.
- The team can set easy-to-understand quality bars. For example, they can say “In order to release, we want no bugs greater than 30 pain.” 
- If there are no bugs left above the current quality bar, they work on feature work.
- When you do stumble upon a bug that will take more than a week to fix, it is flagged as a ‘killer’ bug, for special treatment.
