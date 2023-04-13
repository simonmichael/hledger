# ROADMAP

<div class="pagetoc">

<!-- toc -->
</div>


Ideas about where the hledger project should be going next.
Being listed here suggests a bit of commitment, perhaps even a schedule.
Related: <http://projects.hledger.org>

## Targets

### Next Targets

### Past Targets

1.  hledger 1.19, 2020-09-01

    account transactions register, stricter/more correct handling of
    unbalanced multicommodity transactions (#1177), Track & show
    deposited lots (#1022), Report unrealized capital gains/losses
    (#1029)

2.  hledger 1.18, 2020-06-01

    more effective CI setup, updated home page, quickstart, tutorials
    etc., negative matching in CSV rules,

3.  hledger 1.17, 2020-03-01

    field matching in CSV rules, reduce install hassles with terminfo C
    lib (?), more import/export options, simple console charts,
    refreshed home page, faq, tutorials, manuals,

4.  hledger 1.16, 2019-12-01

    ghc 8.8 support, more powerful CSV conversion, updated home page,
    faq, manuals, reduce install hassles with terminfo C lib

## 2020 Priorities

### Documentation

Improve the docs.

-   home & faq
-   manuals (more discoverable structure of web manuals)
-   cookbook docs (survey, plan, update)
-   contrib guide (update, build from readmes)

### Effectiveness

Improve getting-started experience, just-works quality, practicality,
real-world usefulness.

-   ghc 8.8, get back in stackage nightly
-   install issues (C libs..)
-   more powerful CSV conversion
-   fill out holes in feature matrix

### Investment

Improve suitability for investment tracking
([#1015](https://github.com/simonmichael/hledger/issues/1015))

-   market price inference from transactions
-   easy market price fetching
-   lot tracking
-   capital gains reporting

### Charts

Add charts and more visual appeal.

-   console charts, basic bar charts
-   clarify architecture/UI for charts
-   graphical charts using Chart/matplotlib/hvega
-   review/design/add more attractive/colourful output (see eg
    taskwarrior)

### Correctness

More support for enforcing correctness & accounting rules.

-   account names
-   commodity symbols
-   payees
-   notes/descriptions ?
-   account lifetimes ?
-   account balance conditions ?
-   transaction templates ?
