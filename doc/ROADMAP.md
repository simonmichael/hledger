# ROADMAP

<div class="pagetoc">

<!-- toc -->
</div>


Ideas of where the hledger project should be going next.
Being listed here suggests a bit of commitment, perhaps even a schedule.
Related: [TODO](TODO.md).

## 2024

**Targets:**
- hledger 1.33, april
- hledger 1.34, june ? TBD
- hledger 1.35, september ?
- hledger 1.36, december ?

## 2023

**Targets:**
- hledger 1.32, december
- hledger 1.31, september
- hledger 1.30, june
  - demos: built in asciinema demos and maintenance process *done, 4 simple demos*
  - ghc 9.6 support *partly done, hledger-web is blocked on deps*
  - process/tools improvement *done*
  - docs improvement *done: 115 doc commits + 112 site commits, manual cleanups & rewrites*
- hledger 1.29, march

**Goals:**
  - CSV extensibility: workflows to obtain, use, develop, share, contribute ready-to-use CSV rules
  - Scripts extensibility: workflows to obtain, use, develop, share, contribute ready-to-use scripts
  - Interop: clear ledger & beancount import/export how-tos documenting issues & workarounds
  - Better installer: more robust, binary-installing
  - Bar charts: simple built in bar charts
  - Investment: clear updated how-to documenting available tools & best practices for common needs (price fetching, lot reducing, lot reporting, cost reporting, gains reporting)

**Priorities:**
- newcomer/learner experience: docs, installers, demos
- customiser/contributor experience: easy csv rules install/contrib, scripts install/contrib
- maintainer experience: reduce tech/doc/process/issue debt, increase velocity
- marketing/community: news updates, mastodon presence
- interop: solve Ledger/Beancount reading/writing/conversion
- features: charts, investment

**Mission:**
1. Make plain text accounting more usable and useful for all
2. Bring relief to people experiencing financial and financial technology stress
3. Help people and communities in all countries increase their financial mastery and freedom
4. Help grow a shared global culture of accountability and sustainability
5. Starting with this project and ourselves.
<!-- see also: sponsor.md, faq.md -->

## 2020

**Targets:**

- hledger 1.19, september
  - account transactions register, stricter/more correct handling of
    unbalanced multicommodity transactions (#1177), Track & show
    deposited lots (#1022), Report unrealized capital gains/losses
    (#1029)
- hledger 1.18, june
  - more effective CI setup, updated home page, quickstart, tutorials
    etc., negative matching in CSV rules,
- hledger 1.17, march
  - field matching in CSV rules, reduce install hassles with terminfo C
    lib (?), more import/export options, simple console charts,
    refreshed home page, faq, tutorials, manuals,

**Priorities:**
- Documentation: Improve the docs.
- Effectiveness: Improve getting-started experience, just-works quality, practicality,
real-world usefulness.
- Investment: Improve suitability for investment tracking
([#1015](https://github.com/simonmichael/hledger/issues/1015))
- Charts: Add charts and more visual appeal.
- Correctness: More support for enforcing correctness & accounting rules.

## 2019
**Targets:**
- hledger 1.16, december
  - ghc 8.8 support, more powerful CSV conversion, updated home page,
    faq, manuals, reduce install hassles with terminfo C lib
