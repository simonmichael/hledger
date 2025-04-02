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

Here are some shortcut urls:

- <https://issues.hledger.org>       - all issues, open or closed
- <https://bugs.hledger.org>         - open BUGs
- <https://wishes.hledger.org>       - open WISHes
- <https://prs.hledger.org>          - open pull requests
- <https://readyprs.hledger.org>     - open pull requests ready for review
- <https://draftprs.hledger.org>     - open draft pull requests
- <https://bugs.hledger.org/new>     - report a new issue
- <https://hledger.org/regressions>  - how to claim regression bounties

In 2017 we experimented with Github projects, in 2018 with Github milestones.
Long ago we collected some wishlist items in a [trello board](https://trello.hledger.org) (trello.hledger.org).

## Open issues

<!-- 
This table doesn't have to be aligned, but it helps.
Editing it may require editor support, search/replace etc.
Syntax: https://www.pandoc.org/MANUAL.html#tables -> pipe_tables
-->

By topic and type.

| COMPONENT/TOPIC                                                                                         | BUGS                                                                                                           | WISHES                                                                                                            | PRS                                                                                            | OTHER                                                                                                                          |
|---------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| [all](https://github.com/simonmichael/hledger/issues?q=is:open)                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG)                          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH)                          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr)                          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH)                          |
| **Severity:**                                                                                           |                                                                                                                |                                                                                                                   |                                                                                                |                                                                                                                                |
| severity5-critical                                                                                      | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aseverity5-critical)
| severity4-major                                                                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aseverity4-major)
| severity3-newuser                                                                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aseverity3-newuser)
| severity2-minor                                                                                         | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aseverity2-minor)
| severity1-trivial                                                                                       | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aseverity1-trivial)
| **Impact:**                                                                                             |                                                                                                                |                                                                                                                   |                                                                                                |                                                                                                                                |
| impact5-many                                                                                            | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aimpact5-many)
| impact4-some                                                                                            | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aimpact4-some)
| impact3-few                                                                                             | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aimpact3-few)
| impact2-devs                                                                                            | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aimpact2-devs)
| impact1-noone                                                                                           | [bugs](https://github.com/simonmichael/hledger/issues?q=label%3AA-BUG%20state%3Aopen%20label%3Aimpact1-noone)
| **UIs/apps:**                                                                                           |                                                                                                                |                                                                                                                   |                                                                                                |                                                                                                                                |
| [install](https://github.com/simonmichael/hledger/issues?q=is:open+label:install)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:install)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:install)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:install)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:install)            |
| [cli](https://github.com/simonmichael/hledger/issues?q=is:open+label:cli)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:cli)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:cli)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:cli)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:cli)                |
| [ui](https://github.com/simonmichael/hledger/issues?q=is:open+label:ui)                                 | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:ui)                 | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:ui)                 | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:ui)                 | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:ui)                 |
| [web](https://github.com/simonmichael/hledger/issues?q=is:open+label:web)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:web)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:web)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:web)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:web)                |
| [interest](https://github.com/simonmichael/hledger/issues?q=is:open+label:interest)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:interest)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:interest)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:interest)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:interest)           |
| **Input/output Formats:**                                                                               |                                                                                                                |                                                                                                                   |                                                                                                |                                                                                                                                |
| [journal](https://github.com/simonmichael/hledger/issues?q=is:open+label:journal)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:journal)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:journal)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:journal)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:journal)            |
| [timeclock](https://github.com/simonmichael/hledger/issues?q=is:open+label:timeclock)                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:timeclock)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:timeclock)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:timeclock)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:timeclock)          |
| [timedot](https://github.com/simonmichael/hledger/issues?q=is:open+label:timedot)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:timedot)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:timedot)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:timedot)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:timedot)            |
| [csv](https://github.com/simonmichael/hledger/issues?q=is:open+label:csv)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:csv)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:csv)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:csv)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:csv)                |
| [beancount](https://github.com/simonmichael/hledger/issues?q=is:open+label:beancount)                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:beancount)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:beancount)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:beancount)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:beancount)          |
| [fods](https://github.com/simonmichael/hledger/issues?q=is:open+label:fods)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:fods)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:fods)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:fods)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:fods)               |
| [html](https://github.com/simonmichael/hledger/issues?q=is:open+label:html)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:html)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:html)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:html)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:html)               |
| [json](https://github.com/simonmichael/hledger/issues?q=is:open+label:json)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:json)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:json)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:json)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:json)               |
| [sql](https://github.com/simonmichael/hledger/issues?q=is:open+label:sql)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:sql)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:sql)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:sql)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:sql)                |
| **Commands:**                                                                                           |                                                                                                                |                                                                                                                   |                                                                                                |                                                                                                                                |
| [accounts](https://github.com/simonmichael/hledger/issues?q=is:open+label:accounts)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:accounts)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:accounts)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:accounts)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:accounts)           |
| [activity](https://github.com/simonmichael/hledger/issues?q=is:open+label:activity)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:activity)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:activity)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:activity)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:activity)           |
| [add](https://github.com/simonmichael/hledger/issues?q=is:open+label:add)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:add)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:add)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:add)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:add)                |
| [aregister](https://github.com/simonmichael/hledger/issues?q=is:open+label:aregister)                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:aregister)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:aregister)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:aregister)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:aregister)          |
| [balance](https://github.com/simonmichael/hledger/issues?q=is:open+label:balance)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:balance)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:balance)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balance)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:balance)            |
| [balancesheet](https://github.com/simonmichael/hledger/issues?q=is:open+label:balancesheet)             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:balancesheet)       | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:balancesheet)       | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balancesheet)       | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:balancesheet)       |
| [balancesheetequity](https://github.com/simonmichael/hledger/issues?q=is:open+label:balancesheetequity) | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:balancesheetequity) | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:balancesheetequity) | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:balancesheetequity) | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:balancesheetequity) |
| [cashflow](https://github.com/simonmichael/hledger/issues?q=is:open+label:cashflow)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:cashflow)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:cashflow)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:cashflow)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:cashflow)           |
| [check](https://github.com/simonmichael/hledger/issues?q=is:open+label:check)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:check)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:check)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:check)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:check)              |
| [close](https://github.com/simonmichael/hledger/issues?q=is:open+label:close)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:close)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:close)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:close)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:close)              |
| [codes](https://github.com/simonmichael/hledger/issues?q=is:open+label:codes)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:codes)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:codes)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:codes)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:codes)              |
| [commodities](https://github.com/simonmichael/hledger/issues?q=is:open+label:commodities)               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:commodities)        | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:commodities)        | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:commodities)        | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:commodities)        |
| [demo](https://github.com/simonmichael/hledger/issues?q=is:open+label:demo)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:demo)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:demo)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:demo)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:demo)               |
| [descriptions](https://github.com/simonmichael/hledger/issues?q=is:open+label:descriptions)             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:descriptions)       | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:descriptions)       | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:descriptions)       | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:descriptions)       |
| [diff](https://github.com/simonmichael/hledger/issues?q=is:open+label:diff)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:diff)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:diff)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:diff)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:diff)               |
| [files](https://github.com/simonmichael/hledger/issues?q=is:open+label:files)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:files)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:files)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:files)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:files)              |
| [import](https://github.com/simonmichael/hledger/issues?q=is:open+label:import)                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:import)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:import)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:import)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:import)             |
| [incomestatement](https://github.com/simonmichael/hledger/issues?q=is:open+label:incomestatement)       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:incomestatement)    | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:incomestatement)    | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:incomestatement)    | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:incomestatement)    |
| [notes](https://github.com/simonmichael/hledger/issues?q=is:open+label:notes)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:notes)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:notes)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:notes)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:notes)              |
| [payees](https://github.com/simonmichael/hledger/issues?q=is:open+label:payees)                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:payees)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:payees)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:payees)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:payees)             |
| [prices](https://github.com/simonmichael/hledger/issues?q=is:open+label:prices)                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:prices)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:prices)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:prices)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:prices)             |
| [print](https://github.com/simonmichael/hledger/issues?q=is:open+label:print)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:print)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:print)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:print)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:print)              |
| [register](https://github.com/simonmichael/hledger/issues?q=is:open+label:register)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:register)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:register)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:register)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:register)           |
| [repl](https://github.com/simonmichael/hledger/issues?q=is:open+label:repl)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:repl)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:repl)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:repl)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:repl)               |
| [rewrite](https://github.com/simonmichael/hledger/issues?q=is:open+label:rewrite)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:rewrite)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:rewrite)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:rewrite)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:rewrite)            |
| [roi](https://github.com/simonmichael/hledger/issues?q=is:open+label:roi)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:roi)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:roi)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:roi)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:roi)                |
| [run](https://github.com/simonmichael/hledger/issues?q=is:open+label:run)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:run)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:run)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:run)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:run)                |
| [stats](https://github.com/simonmichael/hledger/issues?q=is:open+label:stats)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:stats)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:stats)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:stats)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:stats)              |
| [tags](https://github.com/simonmichael/hledger/issues?q=is:open+label:tags)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:tags)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:tags)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:tags)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:tags)               |
| [test](https://github.com/simonmichael/hledger/issues?q=is:open+label:test)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:test)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:test)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:test)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:test)               |
| **Miscellaneous:**                                                                                      |                                                                                                                |                                                                                                                   |                                                                                                |                                                                                                                                |
| [bounty](https://github.com/simonmichael/hledger/issues?q=is:open+label:bounty)                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:bounty)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:bounty)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:budget)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:budget)             |
| [budget](https://github.com/simonmichael/hledger/issues?q=is:open+label:budget)                         | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:budget)             | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:budget)             | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:budget)             | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:budget)             |
| [doc](https://github.com/simonmichael/hledger/issues?q=is:open+label:doc)                               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:doc)                | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:doc)                | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:doc)                | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:doc)                |
| [i18n](https://github.com/simonmichael/hledger/issues?q=is:open+label:i18n)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:i18n)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:i18n)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:i18n)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:i18n)               |
| [interest](https://github.com/simonmichael/hledger/issues?q=is:open+label:interest)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:interest)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:interest)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:interest)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:interest)           |
| [investing](https://github.com/simonmichael/hledger/issues?q=is:open+label:investing)                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:investing)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:investing)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:investing)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:investing)          |
| [ledger-compat](https://github.com/simonmichael/hledger/issues?q=is:open+label:ledger-compat)           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:ledger-compat)      | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:ledger-compat)      | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:ledger-compat)      | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:ledger-compat)      |
| [packaging](https://github.com/simonmichael/hledger/issues?q=is:open+label:deps)                        | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:deps)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:deps)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:deps)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:deps)               |
| [performance](https://github.com/simonmichael/hledger/issues?q=is:open+label:performance)               | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:performance)        | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:performance)        | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:performance)        | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:performance)        |
| [period-expressions](https://github.com/simonmichael/hledger/issues?q=is:open+label:period-expressions) | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:period-expressions) | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:period-expressions) | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:period-expressions) | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:period-expressions) |
| [queries](https://github.com/simonmichael/hledger/issues?q=is:open+label:queries)                       | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:queries)            | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:queries)            | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:queries)            | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:queries)            |
| [regression](https://github.com/simonmichael/hledger/issues?q=is:open+label:regression)                 | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:regression)         |                                                                                                                   |                                                                                                |                                                                                                                                |
| [security](https://github.com/simonmichael/hledger/issues?q=is:open+label:security)                     | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:security)           | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:security)           | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:security)           | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:security)           |
| [site](https://github.com/simonmichael/hledger/issues?q=is:open+label:site)                             | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:site)               | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:site)               | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:site)               | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:site)               |
| [tools](https://github.com/simonmichael/hledger/issues?q=is:open+label:tools)                           | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:tools)              | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:tools)              | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:tools)              | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:tools)              |
| [valuation](https://github.com/simonmichael/hledger/issues?q=is:open+label:valuation)                   | [bugs](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-BUG+label:valuation)          | [wishes](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+label:A-WISH+label:valuation)          | [PRs](https://github.com/simonmichael/hledger/issues?q=is:open+is:pr+label:valuation)          | [other](https://github.com/simonmichael/hledger/issues?q=is:open+is:issue+-label:A-BUG+-label:A-WISH+label:valuation)          |

Other:

- [issues needing old bountysource boilerplate removed](https://github.com/simonmichael/hledger/issues?q=%22bountysource-plugin%22%20)


## Labels

In the github issue tracker we use various <https://github.com/simonmichael/hledger/labels>,
some which appear above, to categorise things like:

- whether an issue is a bug (red) or a wish (pink)
- which subcomponents (tools, commands, input/output formats) are involved (light blue)
- which platforms are involved (light purple)
- whether a bounty has been offered (bright green)
- what is blocking an issue (grey)
- bug impact and severity (light orange and light pink, see below)
- miscellaneous things like security (bright red), regressions (black), release needed (orange)
- etc.

These labels also get used as prefixes in [commit messages](#commit-messages), in issue/PR titles, etc.

## Conventions

Some loose conventions:

- We can prefix bug titles (especially regressions) with the hledger version in which the bug first appeared.
  This allows searches like
  [new issues in 1.22](https://github.com/simonmichael/hledger/issues?q=in%3Atitle+1.22)
  and
  [regressions in 1.22](https://github.com/simonmichael/hledger/issues?q=in%3Atitle+1.22+label%3Aregression)

## Estimates

You might see some old experiments in estimate tracking, where issue titles have a suffix noting estimated and spent time.
Basic format: `[ESTIMATEDTOTALTASKTIME|TIMESPENTSOFAR]`.
Examples:

- `[2]` two hours estimated, no time spent
- `[..]` half an hour estimated (a dot is ~a quarter hour, as in timedot format)
- `[1d]` one day estimated (a day is ~4 hours)
- `[1w]` one week estimated (a week is ~5 days or ~20 hours)
- `[3|2]` three hours estimated, about two hours spent so far
- `[1|1w|2d]` first estimate one hour, second estimate one week, about two days spent so far

Estimates are always for the total time cost (not time remaining).
Estimates are not usually changed, a new estimate is added instead.
Numbers are very approximate, but better than nothing.

## Prioritising

<https://lostgarden.home.blog/2008/05/20/improving-bug-triage-with-user-pain/> 
describes an interesting method of ranking issues by a single "User Pain" metric.
What adaptation of this might be useful for the hledger project ?

Here's a simplified version, currently being tested in the hledger issue tracker:

Two [labels](https://github.com/simonmichael/hledger/labels) can be applied to bug reports, each with levels from 1 to 5:

**Impact**

Who may be impacted by this bug ?

- impact1: Affects almost no one.
- impact2: Affects packagers or developers.
- impact3: Affects just a few users.
- impact4: Affects more than a few users.
- impact5: Affects most or all users.

**Severity**

To people impacted, how serious is this bug ?

- severity1: Cleanliness/consistency/developer bug. Only perfectionists care.
- severity2: Minor to moderate usability/doc bug, reasonably easy to avoid or tolerate.
- severity3: New user experience or installability bug. A potential user could fail to get started.
- severity4: Major usability/doc bug, crash, or any regression.
- severity5: Any loss of user's data, privacy, security, or trust.

**User Pain**

The bug's User Pain score is **Impact * Severity / 25**, ranging from 0.04 to 1.

Then, practices like these are possible:

- All open bugs can be listed in order of User Pain (AKA priority).
- Developers can check the Pain List daily and fix the highest pain bugs on the list.
- The team can set easy-to-understand quality bars. For example, they could say “In order to release, we must have no open bugs with more than 15 pain.” 
- If there are no bugs left above the current quality bar, they can work on feature work.
- If a bug is found that will take more than a week to fix, it can be flagged as a ‘killer’ bug, for special treatment.

<!--
$ for i in `seq 1 5`; do for s in `seq 1 5`; do echo "$i x $s / 25 = " `ruby -e "p $i*$s/25.0"`; done; done

1 x 1 / 25 =  0.04
1 x 2 / 25 =  0.08
1 x 3 / 25 =  0.12
1 x 4 / 25 =  0.16
1 x 5 / 25 =  0.2
2 x 1 / 25 =  0.08
2 x 2 / 25 =  0.16
2 x 3 / 25 =  0.24
2 x 4 / 25 =  0.32
2 x 5 / 25 =  0.4
3 x 1 / 25 =  0.12
3 x 2 / 25 =  0.24
3 x 3 / 25 =  0.36
3 x 4 / 25 =  0.48
3 x 5 / 25 =  0.6
4 x 1 / 25 =  0.16
4 x 2 / 25 =  0.32
4 x 3 / 25 =  0.48
4 x 4 / 25 =  0.64
4 x 5 / 25 =  0.8
5 x 1 / 25 =  0.2
5 x 2 / 25 =  0.4
5 x 3 / 25 =  0.6
5 x 4 / 25 =  0.8
5 x 5 / 25 =  1.0 

$ ghc -ignore-dot-ghci -package-env - -e 'import Data.List; import Text.Printf' -e 'let (is,ss) = ([1..5],[1..5]) in mapM_ (printf "%.2f\n") $ nub $ sort [i*s/(maximum is * maximum ss) | i<-is, s<-ss]'
0.04
0.08
0.12
0.16
0.20
0.24
0.32
0.36
0.40
0.48
0.60
0.64
0.80
1.00

-->

## Reducing bugs and regressions

Some ideas in 2024-01:

- Maintain ratio of user-visible bugfixes to new features, eg above 10:1 (a new master merge test, human checked)
- A release cycle with no new features
- Alternate bugfix and feature release cycles
- Set bug count targets
- Label all issues for impact/severity/user pain; set max user pain targets
- Gate releases on user pain targets or other bug metrics
- Document and follow more disciplined bug triage/fixing methods
- Identify every new bug early as a regression/non-regression
- Prioritise rapid fixing and releasing for regressions / new bugs
- Cheaper, more frequent bugfix releases
- More intentional systematic tests ? Analyse for weak spots ?
- Property tests ?
- Internal cleanup, architectural improvements, more type safety ?
- Custom issue dashboards (HTMX on hledger.org ?)
- Public list / QA dashboard
- Grow a QA team

Older ideas

- Custodians for particular components/topics
  ("If you are interested in helping with a particular component for a while, please add yourself as a custodian in the Open Issues table. A custodian\'s job is to help manage the issues, rally the troops, and drive the open issue count towards zero. The more custodians, the better! By dividing up the work this way, we can scale and make forward progress.")

